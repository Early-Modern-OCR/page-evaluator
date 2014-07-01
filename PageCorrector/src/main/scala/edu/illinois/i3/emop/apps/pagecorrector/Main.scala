package edu.illinois.i3.emop.apps.pagecorrector

import org.rogach.scallop.{ValueConverter, ScallopConf, singleArgConverter, listArgConverter}
import com.typesafe.scalalogging.slf4j.Logging
import edu.illinois.i3.spellcheck.engine.{SpellDictionaryHashMap, SpellDictionary}
import java.io.{FileWriter, PrintWriter, File}
import scala.io.{Codec, Source}
import resource._
import net.liftweb.json.{parse => parseJson, JString, JField, compactRender}
import net.liftweb.json.JsonDSL._
import scala.util._
import TextTransformer._
import edu.illinois.i3.emop.apps.pagecorrector.utils.BoneCPConnPool
import scala.collection.mutable


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

    val (appTitle, appVersion, appVendor) = {
      val p = getClass.getPackage
      val nameOpt = Option(p.getImplementationTitle)
      val versionOpt = Option(p.getImplementationVersion)
      val vendorOpt = Option(p.getImplementationVendor)
      (nameOpt, versionOpt, vendorOpt)
    }

    version(appTitle.flatMap(
      name => appVersion.flatMap(
        version => appVendor.map(
          vendor => s"$name $version\n$vendor"))).getOrElse("PageCorrector"))

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

    val dumpCorrectionDetails = opt[Boolean]("dump",
        descr = "Dump details of the corrections made and corrections missed to individual files",
        default = Some(false)
    )

    val showCorrectionStats = opt[Boolean]("stats",
        descr = "Print correction statistics in JSON format, at the end",
        default = Some(false)
    )

    val preProcName = opt[String]("preProcName",
        noshort = true,
        descr = "The name of the software used in the pre-processing step",
        default = None
    )

    val preProcVer = opt[String]("preProcVersion",
        noshort = true,
        descr = "The version of the software used in the pre-processing step",
        default = None
    )

    val preProcVendor = opt[String]("preProcVendor",
        noshort = true,
        descr = "The name of the vendor of the software used in the pre-processing step",
        default = None
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
  val showCorrectionStats = conf.showCorrectionStats()
  val dumpCorrectionDetails = conf.dumpCorrectionDetails()

  // Define the output files
  val pageOcrName = pageOcrFile.getName.substring(0, pageOcrFile.getName.lastIndexOf('.'))
  val outAltoXmlFile = new File(outputDir, s"${pageOcrName}_ALTO.xml")
  val outAltoTxtFile = new File(outputDir, s"${pageOcrName}_ALTO.txt")

  // Create the DB connection pool
  val connPool = {
    val connPoolFactory = new BoneCPConnPool {
      override val BONECP_MIN_CONN_PER_PART: Int = 2
      override val BONECP_MAX_CONN_PER_PART: Int = 10
      override val BONECP_PARTITION_COUNT: Int = 2
    }
    connPoolFactory.createConnectionPool("com.mysql.jdbc.Driver", dbUrl, dbUser, dbPasswd)
  }

  // Use the connection pool to create the ngram context checker
  val contextChecker = connPool match {
    case Success(pool) => new NgramContextMatcher(pool)
    case Failure(e) => throw new RuntimeException(s"Error creating database connection pool", e)
  }

  managed(Source.fromFile(pageOcrFile).bufferedReader()).acquireAndGet { reader =>
    // Create an instance of the page corrector
    val pageCorrector = new EmopPageCorrector(dictionaries, transformRules, contextChecker)

    // Read the page hOCR XML
    val pageXml = pageCorrector.readXml(reader) match {
      case Success(xml) => xml
      case Failure(e) => throw new RuntimeException(s"Error reading document: $pageOcrFile", e)
    }

    // Extract the tokens from the page
    val lines = pageCorrector.getLines(pageXml)
    val tokens = lines.flatten

    // Run the correction algorithm on the extracted tokens
    pageCorrector.correctTokens(tokens)

    // Record the corrected and unchanged tokens
    lazy val correctedTokens = tokens.filter(t => t.isMisspelled && t.bestReplacement.isDefined)
    lazy val unchangedTokens = tokens.filter(t => t.isMisspelled && t.bestReplacement.isEmpty)

    // Display the corrections made if debug mode is enabled
    if (logger.underlying.isDebugEnabled) {
      logger.debug("Corrections:")
      for (token <- correctedTokens)
        logger.debug("{} -> {}", token.text, token.bestReplacement.get)
    }

    // Save stats about the transformation rules used to generate the corrections
    if (saveTransformationStats) {
      val transformStats = mutable.Map.empty[(String, String), Int]
      for (token <- correctedTokens) {
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

    // Create the mapping between token ids to tokens
    val tokensMap = tokens.foldLeft(Map.empty[String, HOCRToken])((map, token) => token match {
      case hyphenatedToken: HyphenatedToken =>
        map ++ List(hyphenatedToken.firstToken.id -> hyphenatedToken, hyphenatedToken.secondToken.id -> hyphenatedToken)

      case token: HOCRToken => map + (token.id -> token)
    })

    // Create the ALTO XML representation of the corrected page
    val preProcessingSoftware = (conf.preProcName.get, conf.preProcVer.get, conf.preProcVendor.get) match {
      case (Some(name), Some(version), Some(vendor)) => Some(ProcessingSoftware(name, version, vendor))
      case _ => None
    }
    val postProcessingSoftware = (conf.appTitle, conf.appVersion, conf.appVendor) match {
      case (Some(name), Some(version), Some(vendor)) => Some(ProcessingSoftware(name, version, vendor))
      case _ => Some(ProcessingSoftware(
        "PageCorrector",
        "N/A",
        "Illinois Informatics Institute, University of Illinois at Urbana-Champaign http://www.informatics.illinois.edu"
      ))
    }
    val altoXml = AltoXml.create(pageXml, tokensMap, preProcessingSoftware, postProcessingSoftware)

    // Write the ALTO XML output to file
    val prettyPrinter = new scala.xml.PrettyPrinter(80, 2)
    val prettyXml = prettyPrinter.format(altoXml)
    managed(new PrintWriter(outAltoXmlFile)) acquireAndGet (_.write(prettyXml))

    // Write the text to file
    managed(new FileWriter(outAltoTxtFile)) acquireAndGet { txtFile =>
      for (line <- lines) {
        var skipEOL = false

        line.foreach {
          case hyphenatedToken: HyphenatedToken if hyphenatedToken.isMisspelled =>
            val replacement = hyphenatedToken.bestReplacement.getOrElse(hyphenatedToken.text)
            var hypPos = hyphenatedToken.firstToken.text.length-1
            if (replacement.length < hypPos)
              hypPos = replacement.length
            var hyphenText = "";
            try {
              hyphenText = replacement.take(hypPos) concat "-\n" concat replacement.substring(hypPos)
            }
            catch {
              case e: Exception => println(s"f: ${hyphenatedToken.firstToken.text} s: ${hyphenatedToken.secondToken.text} r: $replacement pos: $hypPos --> $e");
            }
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

    if (dumpCorrectionDetails) {
      val outCorrectionsFile = new File(outputDir, s"${pageOcrName}_ALTO.txt.corrected")
      val outUnchangedFile = new File(outputDir, s"${pageOcrName}_ALTO.txt.unchanged")

      managed(new FileWriter(outCorrectionsFile)) acquireAndGet { correctionsFile =>
        correctionsFile.write("orig\tcorrected\n")
        for (token <- correctedTokens)
          correctionsFile.write(s"${token.text}\t${token.bestReplacement.get}\n")
      }

      managed(new FileWriter(outUnchangedFile)) acquireAndGet { unchangedFile =>
        for (token <- unchangedTokens)
          unchangedFile.write(s"${token.text}\n")
      }
    }

    if (showCorrectionStats) {
      // Compute page correction statistics
      val totalTokenCount = tokens.size
      val ignoredCount = tokens.count(!_.isCorrectable)
      val correctCount = tokens.count(!_.isMisspelled) - ignoredCount
      val correctedCount = correctedTokens.size
      val unchangedCount = unchangedTokens.size

      assert(totalTokenCount == ignoredCount + correctCount + correctedCount + unchangedCount,
        "Correction statistics sanity check failed")

      val jsonStats =
        ("total" -> totalTokenCount) ~
          ("ignored" -> ignoredCount) ~
          ("correct" -> correctCount) ~
          ("corrected" -> correctedCount) ~
          ("unchanged" -> unchangedCount)

      println(compactRender(jsonStats))
    }
  }
}
