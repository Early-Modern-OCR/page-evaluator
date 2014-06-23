package edu.illinois.i3.emop.apps.pagecorrector

import org.rogach.scallop.{ValueConverter, ScallopConf, singleArgConverter, listArgConverter}
import com.typesafe.scalalogging.slf4j.Logging
import edu.illinois.i3.spellcheck.engine.{SpellDictionaryHashMap, SpellDictionary}
import com.google.common.io.Files
import java.io.{FileWriter, PrintWriter, File}
import scala.collection.immutable.TreeMap
import scala.io.{Codec, Source}
import resource._
import net.liftweb.json.{parse => parseJson, JString, JField}
import scala.util._
import TextTransformer._
import edu.illinois.i3.emop.apps.pagecorrector.utils.BoneCPConnPool
import scala.collection.mutable
import org.w3c.dom.{Element, NodeList, Document}
import javax.xml.xpath.{XPathConstants, XPathFactory}
import edu.illinois.i3.scala.utils.implicits.XmlImplicits._
import scala.xml._
import scala.util.Left
import scala.util.Failure
import scala.Some
import scala.util.Success
import scala.util.Right


object Main extends App with Logging {
  implicit val codec = Codec.UTF8

  /**
   * Command line argument parser
   *
   * @param arguments The cmd line arguments
   */
  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    object ValueConverters {
      private def getFilenameWithoutExtension(filePath: String) = {
        var idx = filePath.lastIndexOf(File.separator)
        val fname = if (idx != -1) filePath.substring(idx + 1) else filePath
        idx = fname.lastIndexOf(".")
        if (idx != -1) fname.substring(0, idx) else fname
      }

      implicit val dictionaryListConverter: ValueConverter[List[SpellDictionary]] =
        listArgConverter[Try[SpellDictionary]](f =>
          Try {
            val dictName = getFilenameWithoutExtension(f)
            managed(Source.fromFile(f).reader()).acquireAndGet {
              dictReader => new SpellDictionaryHashMap(dictName, dictReader)
            }
          }
        ).flatMap { d =>
          Try(d.map(_.get)) match {
            case Success(dicts) => Right(Some(dicts))
            case Failure(t) => Left(t.getMessage)
          }
        }

      implicit val transformRulesConverter: ValueConverter[TransformRules] =
        singleArgConverter[Try[TransformRules]](f =>
          Try {
            val rulesJson = managed(Source.fromFile(f)).acquireAndGet(_.mkString)
            val transformList: List[(String, String)] = for {
              JField(correct, transforms) <- parseJson(rulesJson)
              JString(transform) <- transforms
            } yield transform -> correct
            val rulesMap = transformList.groupBy(_._1).map { case (k, v) => (k, v.map(_._2).toSet) }
            rulesMap
          }
        ).flatMap {
          case Success(rules) => Right(Some(rules))
          case Failure(t) => Left(t.getMessage)
        }
    }

    import ValueConverters._

    version {
      val p = getClass.getPackage
      val nameOpt = Option(p.getImplementationTitle)
      val versionOpt = Option(p.getImplementationVersion)
      nameOpt.flatMap(name => versionOpt.map(version => s"$name $version")).getOrElse("PageCorrector")
    }

    // Database connection parameters
    val dbUrl = opt[String]("db", descr = "The DB connection URL", required = true)
    val dbUser = opt[String]("user", descr = "The DB user name")
    val dbPassword = opt[String]("password", descr = "The DB user password")
    dependsOnAll(dbUrl, List(dbUser, dbPassword))

    // Transformation rules
    val transformRules = opt[TransformRules]("transform",
        descr = "The transformation rules file",
        required = true)

    // Dictionaries
    val dictionaries = opt[List[SpellDictionary]]("dict",
        descr = "Specifies one or more dictionaries to use",
        required = true)

    // Page OCR file parameter
    val pageOcrFile = trailArg[File]("page-ocr",
        descr = "The page OCR file",
        required = true)

    // Output directory
    val outputDir = opt[String]("outputDir",
        descr = "The directory where the results should be written to",
        required = true)

    val saveTransformationStats = opt[Boolean]("save",
        descr = "Save stats about which transformation rules were applied",
        default = Some(false)
    )
  }

  // Parse the command line args and extract values
  val conf = new Conf(args)
  val dbUrl = conf.dbUrl()
  val dbUser = conf.dbUser()
  val dbPasswd = conf.dbPassword()
  val transformRules = conf.transformRules()
  val dictionaries = conf.dictionaries()
  val pageOcrFile = conf.pageOcrFile()
  val outputDir = conf.outputDir()
  val saveTransformationStats = conf.saveTransformationStats()

  val pageOcrName = pageOcrFile.getName.substring(0, pageOcrFile.getName.lastIndexOf('.'))
  val outAltoXmlFile = new File(outputDir, s"${pageOcrName}_ALTO.xml")
  val outAltoTxtFile = new File(outputDir, s"${pageOcrName}_ALTO.txt")

  val connPool = {
    val connPoolFactory = new BoneCPConnPool {
      override val BONECP_MIN_CONN_PER_PART: Int = 2
      override val BONECP_MAX_CONN_PER_PART: Int = 10
      override val BONECP_PARTITION_COUNT: Int = 2
    }
    connPoolFactory.createConnectionPool("com.mysql.jdbc.Driver", dbUrl, dbUser, dbPasswd)
  }

  managed(Source.fromFile(pageOcrFile).bufferedReader()).acquireAndGet { reader =>
    val pageParser = new HOCRPageParser with LineCleaner with HyphenatedTokenJoiner {
      import org.w3c.dom.Element

      override type TokenType = HOCRToken
      override type HyphenatedTokenType = HyphenatedToken

      protected override def createToken(xmlWord: Element) = {
        val id = xmlWord.getAttribute("id")
        val text = xmlWord.getTextContent
        new HOCRToken(id, text) {
          override val dictionaries = Main.dictionaries
          override val transformRules = Main.transformRules
        }
      }

      protected override def createHyphenatedToken(firstToken: TokenType, secondToken: TokenType) =
        new HyphenatedToken(firstToken, secondToken) {
          override val dictionaries = Main.dictionaries
          override val transformRules = Main.transformRules
        }

      override def getLines(document: Document) = joinHyphenatedTokens(cleanLines(super.getLines(document)))
    }

    def correctTokens(tokens: Seq[HOCRToken]) {
      val replacementCache = mutable.HashMap.empty[String, Iterable[String]]

      val contextChecker = connPool match {
        case Success(pool) => new NgramContextMatcher(pool)
        case Failure(e) => throw new RuntimeException(s"Error creating database connection pool", e)
      }

      tokens.sliding(3).withFilter(_.exists(_.isMisspelled)).foreach {
        case window@Seq(token1, token2, token3) =>
          logger.debug("window: {} {} {}", token1, token2, token3)
          val ngram1 = if (token1.isMisspelled) {
            logger.debug("Finding replacements for {}", token1)
            replacementCache.getOrElseUpdate(token1.cleanedText, {
              token1.replacements.map(_.toLowerCase).toSet
            })
          } else Set(token1.cleanedText)

          val ngram2 = if (token2.isMisspelled) {
            logger.debug("Finding replacements for {}", token2)
            replacementCache.getOrElseUpdate(token2.cleanedText, {
              token2.replacements.map(_.toLowerCase).toSet
            })
          } else Set(token2.cleanedText)

          val ngram3 = if (token3.isMisspelled) {
            logger.debug("Finding replacements for {}", token3)
            replacementCache.getOrElseUpdate(token3.cleanedText, {
              token3.replacements.map(_.toLowerCase).toSet
            })
          } else Set(token3.cleanedText)

          if (ngram1.size + ngram2.size + ngram3.size > 3) {
            logger.debug("Variants used for context matching:")
            logger.debug("{} -> {}", token1.text, ngram1)
            logger.debug("{} -> {}", token2.text, ngram2)
            logger.debug("{} -> {}", token3.text, ngram3)

            contextChecker.matches(ngram1, ngram2, ngram3) match {
              case Success(contextMatches) if contextMatches.nonEmpty => contextMatches.foreach {
                case ContextMatch(text1, text2, text3, matchCount, volCount) =>
                  logger.debug("ContextMatch: {} {} {}  (matchCount: {} , volCount: {})",
                    text1, text2, text3, matchCount.toString, volCount.toString)
                  if (token1.isMisspelled) token1.addContextMatch(1, text1, matchCount, volCount)
                  if (token2.isMisspelled) token2.addContextMatch(2, text2, matchCount, volCount)
                  if (token3.isMisspelled) token3.addContextMatch(3, text3, matchCount, volCount)
              }
              case Failure(e) => println(window.mkString(" ") concat s" -> ${e.getMessage}")
              case _ => logger.debug("No 3-gram context matches found")  // TODO check for 2-gram matches?
            }
          }

        case _ => // skip context matching if page has < 3 tokens
      }
    }

    val pageXml = pageParser.readXml(reader) match {
      case Success(xml) => xml
      case Failure(e) => throw new RuntimeException(s"Error reading document: $pageOcrFile", e)
    }

    val lines = pageParser.getLines(pageXml)
    val tokens = lines.flatten

    val tokenMap = mutable.LinkedHashMap.empty[String, HOCRToken]
    tokens.foreach {
      case hyphenatedToken: HyphenatedToken =>
        tokenMap.put(hyphenatedToken.firstToken.id, hyphenatedToken)
        tokenMap.put(hyphenatedToken.secondToken.id, hyphenatedToken)

      case token: HOCRToken =>
        tokenMap.put(token.id, token)
    }

    correctTokens(tokens)

    if (logger.underlying.isDebugEnabled) {
      logger.debug("Corrections:")
      for (token <- tokens.withFilter(t => t.isMisspelled && t.bestUnformattedReplacement.isDefined))
        logger.debug("{} -> {}", token.text, token.bestReplacement.get)
    }

    if (saveTransformationStats) {
      val transformStats = mutable.Map.empty[(String, String), Int]
      for (token <- tokens.withFilter(t => t.isMisspelled && t.bestUnformattedReplacement.isDefined)) {
        val replacement = token.bestUnformattedReplacement.get
        token.correctTransformations.find(_.text equalsIgnoreCase replacement) match {
          case Some(TransformedText(_, _, transformations)) =>
            val transformCounts = transformations.map(t => t.original -> t.replacement).groupBy(t => t).map {
              case (k, v) => k -> v.length
            }
            for ((transform, count) <- transformCounts)
              transformStats(transform) = transformStats.getOrElse(transform, 0) + 1

          case None =>
        }
      }

      if (transformStats.nonEmpty) {
        val outRuleStatsTxtFile = new File(outputDir, s"${pageOcrName}_rules.tsv")
        managed(new FileWriter(outRuleStatsTxtFile)) acquireAndGet { txtFile =>
          txtFile.write("orig\trepl\tcount\n")
          for (((o, r), count) <- transformStats.toSeq.sortBy(- _._2))
            txtFile.write(s"$o\t$r\t$count\n")
        }
      }
    }

    def getProperties(s: String) = {
      val Pattern = """(\S+) (.+)""".r
      s.split(';').map(_.trim).collect { case Pattern(key, value) => (key, value) }.toMap
    }

    val xpathFactory = XPathFactory.newInstance
    val xpath = xpathFactory.newXPath

    val pageNode = xpath.evaluate("/html/body/div[@class='ocr_page']",
                   pageXml, XPathConstants.NODE).asInstanceOf[Element]
    val pageProps = getProperties(pageNode.getAttribute("title"))
    val tesseractVersion = xpath.evaluate("/html/head/meta[@name='ocr-system']/@content",
                           pageXml, XPathConstants.STRING).toString
    val DimPattern = """(\d+) (\d+) (\d+) (\d+)""".r

    val altoXml =
      <alto xmlns="http://schema.ccs-gmbh.com/ALTO"
            xmlns:emop="http://emop.tamu.edu">
        <Description>
          <MeasurementUnit>pixel</MeasurementUnit>
          <sourceImageInformation>
            <filename>{ pageProps("image").stripPrefix("\"").stripSuffix("\"") }</filename>
          </sourceImageInformation>
          <OCRProcessing>
            <preProcessingStep>
              <processingSoftware>
                <softwareCreator>The Early Modern OCR Project (eMOP) at Texas A&amp;M University. emop.tamu.edu</softwareCreator>
                <softwareName>de-noising algorithm</softwareName>
                <softwareVersion>1</softwareVersion>
              </processingSoftware>
            </preProcessingStep>
            <ocrProcessingStep>
              <processingSoftware>
                <softwareCreator>Google</softwareCreator>
                <softwareName>Tesseract</softwareName>
                <softwareVersion>{ tesseractVersion }</softwareVersion>
              </processingSoftware>
            </ocrProcessingStep>
            <postProcessingStep>
              <processingSoftware>
                <softwareCreator>The Software Environment for the Advancement for Scholarly Research (SEASR) at the University of Illinois, Urbana/Champaign. www.seasr.org</softwareCreator>
                <softwareName>word correction</softwareName>
                <softwareVersion>1</softwareVersion>
              </processingSoftware>
            </postProcessingStep>
          </OCRProcessing>
        </Description>
        <Layout>
          {
          val (width, height) = pageProps("bbox") match {
            case DimPattern(_, _, w, h) => (w.toInt, h.toInt)
            case _ => (0, 0)
          }
          <Page ID={ pageNode.getAttribute("id") }
                WIDTH={ s"$width" }
                HEIGHT={ s"$height" }>
            <PrintSpace>
              {
              for (parNode <- xpath.evaluate("div[@class='ocr_carea']/p[@class='ocr_par']",
                              pageNode, XPathConstants.NODESET).asInstanceOf[NodeList])
                yield {
                  val parXml = parNode.asInstanceOf[Element]
                  val parProps = getProperties(parXml.getAttribute("title"))
                  val (parTop, parLeft, parBottom, parRight) = parProps("bbox") match {
                    case DimPattern(t, l, b, r) => (t.toInt, l.toInt, b.toInt, r.toInt)
                    case _ => (0, 0, 0, 0)
                  }
                  <TextBlock ID={ parXml.getAttribute("id") }
                             WIDTH={ s"${parRight-parLeft}" }
                             HEIGHT={ s"${parBottom-parTop}" }
                             HPOS={ s"$parLeft" }
                             VPOS={ s"$parTop" }>
                    {
                    for (lineNode <- xpath.evaluate("span[@class='ocr_line']",
                                     parNode, XPathConstants.NODESET).asInstanceOf[NodeList])
                      yield {
                        val lineXml = lineNode.asInstanceOf[Element]
                        val lineProps = getProperties(lineXml.getAttribute("title"))
                        val (lineTop, lineLeft, lineBottom, lineRight) = lineProps("bbox") match {
                          case DimPattern(t, l, b, r) => (t.toInt, l.toInt, b.toInt, r.toInt)
                          case _ => (0, 0, 0, 0)
                        }
                        <TextLine ID={ lineXml.getAttribute("id") }
                                  WIDTH={ s"${lineRight-lineLeft}" }
                                  HEIGHT={ s"${lineBottom-lineTop}" }
                                  HPOS={ s"$lineLeft" }
                                  VPOS={ s"$lineTop" }>
                          {
                          var firstOnLine = true
                          val wordNodes = xpath.evaluate("span[@class='ocrx_word']",
                                          lineNode, XPathConstants.NODESET).asInstanceOf[NodeList].toIterator
                          for (wordNode <- wordNodes)
                            yield {
                              val wordXml = wordNode.asInstanceOf[Element]
                              val wordId = wordXml.getAttribute("id")
                              val wordProps = getProperties(wordXml.getAttribute("title"))
                              val (wordTop, wordLeft, wordBottom, wordRight) = wordProps("bbox") match {
                                case DimPattern(t, l, b, r) => (t.toInt, l.toInt, b.toInt, r.toInt)
                                case _ => (0, 0, 0, 0)
                              }

                              tokenMap.get(wordId) match {
                                case Some(token) =>
                                  val nodeBuffer = new xml.NodeBuffer
                                  if (!firstOnLine)
                                    nodeBuffer += <SP WIDTH="10"/>
                                  else
                                    firstOnLine = false

                                  def add(n: Elem, c: Elem): Elem = n match { case e: Elem => e.copy(child = e.child ++ c) }

                                  val (wordIsHyphenated, isFirstPartOfHyphen, subsContent, subsType, content) = token match {
                                    case ht: HyphenatedToken if ht.isMisspelled =>
                                      val replacement = ht.bestReplacement.getOrElse(ht.text)
                                      val diff = ht.text.length - replacement.length
                                      val hypPos = ht.firstToken.text.length-1 - (if (diff > 0) diff else 0)
                                      val isFirstPart = ht.firstToken.id equals wordId
                                      val (part, c) = if (isFirstPart)
                                        ("HypPart1", replacement.take(hypPos)) else
                                        ("HypPart2", replacement.substring(hypPos))
                                      (true, isFirstPart, replacement, part, c)

                                    case ht: HyphenatedToken =>
                                      val isFirstPart = ht.firstToken.id equals wordId
                                      val (part, c) = if (isFirstPart)
                                        ("HypPart1", ht.firstToken.text.dropRight(1)) else
                                        ("HypPart2", ht.secondToken.text)
                                      (true, isFirstPart, ht.text, part, c)

                                    case t: HOCRToken if t.isMisspelled =>
                                      (false, false, null, null, t.bestReplacement.getOrElse(t.text))

                                    case t: HOCRToken =>
                                      (false, false, null, null, t.text)
                                  }

                                  var altoStringNode =
                                      <String ID={wordId}
                                              WIDTH={ s"${wordRight - wordLeft}" }
                                              HEIGHT={ s"${wordBottom - wordTop}" }
                                              HPOS={ s"$wordLeft" }
                                              VPOS={ s"$wordTop" }
                                              CONTENT={ content }
                                              WC={ wordProps("x_wconf") }
                                              emop:DNC="De-noising Confidence Value"/>

                                  if (wordIsHyphenated)
                                    altoStringNode = altoStringNode %
                                      Attribute(null, "SUBS_CONTENT", subsContent,
                                        Attribute(null, "SUBS_TYPE", subsType, Null))

                                  // Add the alternatives
                                  if (token.isMisspelled && content != token.text) {
                                    import HOCRToken.preservePunctuationAndStyle

                                    val alternatives =
                                      mutable.LinkedHashSet(token.sortedContextMatches.map(_._1.toLowerCase): _*) ++
                                      token.replacements.drop(1).map(_.toLowerCase)
                                    for (alt <- alternatives.map(preservePunctuationAndStyle(token.text, _))
                                                .filterNot(txt => (txt equals content) || (txt equals token.text))
                                                .take(2)) { // only record max two correction alternatives
                                      altoStringNode = add(altoStringNode, <ALTERNATIVE>{ alt }</ALTERNATIVE>)
                                    }

                                    altoStringNode = add(altoStringNode, <ALTERNATIVE>{ token.text }</ALTERNATIVE>)
                                  }

                                  nodeBuffer += altoStringNode

                                  if (isFirstPartOfHyphen)
                                    nodeBuffer += <HYP WIDTH="10" CONTENT="-"/>

                                  nodeBuffer

                                case None => NodeSeq.Empty
                              }
                            }
                          }
                        </TextLine>
                      }
                    }
                  </TextBlock>
                }
              }
            </PrintSpace>
          </Page>
          }
        </Layout>
      </alto>

    val prettyPrinter = new scala.xml.PrettyPrinter(80, 2)
    val prettyXml = prettyPrinter.format(altoXml)
    managed(new PrintWriter(outAltoXmlFile)) acquireAndGet (_.write(prettyXml))

    managed(new FileWriter(outAltoTxtFile)) acquireAndGet { txtFile =>
      for (line <- lines) {
        var skipEOL = false

        line.foreach {
          case hyphenatedToken: HyphenatedToken if hyphenatedToken.isMisspelled =>
            val replacement = hyphenatedToken.bestReplacement.getOrElse(hyphenatedToken.text)
            val hyphenText = replacement.take(hyphenatedToken.firstToken.text.length - 1) concat "-\n" concat
              replacement.substring(hyphenatedToken.firstToken.text.length - 1)
            skipEOL = true
            txtFile.write(s"$hyphenText ")

          case hyphenatedToken: HyphenatedToken =>
            skipEOL = true
            txtFile.write(s"${hyphenatedToken.firstToken.text}\n${hyphenatedToken.secondToken.text} ")

          case token: OCRToken if token.isMisspelled =>
            val replacement = token.bestReplacement.getOrElse(token.text)
            txtFile.write(s"$replacement ")

          case token: OCRToken => txtFile.write(s"${token.text} ")
        }

        if (!skipEOL)
          txtFile.write("\n")
      }
    }
  }
}
