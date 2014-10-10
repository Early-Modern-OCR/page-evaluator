package edu.illinois.i3.emop.apps.pagecorrector

import org.w3c.dom.{Element, NodeList, Document}
import javax.xml.xpath.{XPathConstants, XPathFactory}
import edu.illinois.i3.scala.utils.implicits.XmlImplicits._
import scala.collection.mutable
import scala.xml._

case class ProcessingSoftware(name: String, version: String, vendor: String)

object AltoXml {
  /**
   * Creates an ALTO XML representation of a page from the original hOCR page XML and the corrected tokens
   *
   * @param pageHocrXml The page hOCR XML
   * @param correctedTokens The corrected tokens map
   * @param preProcessingSoftware The pre-processing software to fill in the <preProcessingStep> metadata section
   * @param postProcessingSoftware The post-processing software to fill in the <postProcessingStep> metadata section
   * @return The ALTO XML element root
   */
  def create(pageHocrXml: Document,
             correctedTokens: Map[String, HOCRToken],
             noiseCutoff: Float,
             altCount: Int,
             preProcessingSoftware: Option[ProcessingSoftware],
             postProcessingSoftware: Option[ProcessingSoftware]) = {

    def getProperties(s: String) = {
      val Pattern = """(\S+) (.+)""".r
      s.split(';').map(_.trim).collect { case Pattern(key, value) => (key, value) }.toMap
    }

    val xpathFactory = XPathFactory.newInstance
    val xpath = xpathFactory.newXPath
    val pageNode = xpath.evaluate("//div[@class='ocr_page']", pageHocrXml, XPathConstants.NODE).asInstanceOf[Element]
    val pageProps = getProperties(pageNode.getAttribute("title"))
    val tesseractVersion = xpath.evaluate("/html/head/meta[@name='ocr-system']/@content", pageHocrXml, XPathConstants.STRING).toString
    val DimPattern = """(\d+) (\d+) (\d+) (\d+)""".r

    <alto xmlns="http://schema.ccs-gmbh.com/ALTO"
          xmlns:emop="http://emop.tamu.edu">
      <Description>
        <MeasurementUnit>pixel</MeasurementUnit>
        <sourceImageInformation>
          <filename>{ pageProps("image").stripPrefix("\"").stripSuffix("\"") }</filename>
        </sourceImageInformation>
        <OCRProcessing>
          <preProcessingStep>{
            if (preProcessingSoftware.nonEmpty)
              <processingSoftware>
                <softwareCreator>{ preProcessingSoftware.get.vendor }</softwareCreator>
                <softwareName>{ preProcessingSoftware.get.name }</softwareName>
                <softwareVersion>{ preProcessingSoftware.get.version }</softwareVersion>
              </processingSoftware>
            else
              NodeSeq.Empty
          }</preProcessingStep>
          <ocrProcessingStep>
            <processingSoftware>
              <softwareCreator>Google</softwareCreator>
              <softwareName>Tesseract</softwareName>
              <softwareVersion>{ tesseractVersion }</softwareVersion>
            </processingSoftware>
          </ocrProcessingStep>
          <postProcessingStep>{
            if (postProcessingSoftware.nonEmpty)
              <processingSoftware>
                <softwareCreator>{ postProcessingSoftware.get.vendor }</softwareCreator>
                <softwareName>{ postProcessingSoftware.get.name }</softwareName>
                <softwareVersion>{ postProcessingSoftware.get.version }</softwareVersion>
              </processingSoftware>
            else
              NodeSeq.Empty
          }</postProcessingStep>
        </OCRProcessing>
      </Description>
      <Layout>{
        val (width, height) = pageProps("bbox") match {
          case DimPattern(_, _, w, h) => (w.toInt, h.toInt)
          case _ => (0, 0)
        }
        <Page ID={ pageNode.getAttribute("id") }
              WIDTH={ s"$width" }
              HEIGHT={ s"$height" }>
          <PrintSpace>{
            for (parNode <- xpath.evaluate("div[@class='ocr_carea']/p[@class='ocr_par']",
              pageNode, XPathConstants.NODESET).asInstanceOf[NodeList])
            yield {
              val parXml = parNode.asInstanceOf[Element]
              val parProps = getProperties(parXml.getAttribute("title"))
              val (parLeft, parTop, parRight, parBottom) = parProps("bbox") match {
                case DimPattern(l, t, r, b) => (l.toInt, t.toInt, r.toInt, b.toInt)
                case _ => (0, 0, 0, 0)
              }
              val lineNodes = xpath.evaluate("span[@class='ocr_line']",
                parNode, XPathConstants.NODESET).asInstanceOf[NodeList]
              if (lineNodes.isEmpty)
                NodeSeq.Empty
              else
              <TextBlock ID={ parXml.getAttribute("id") }
                         WIDTH={ s"${parRight-parLeft}" }
                         HEIGHT={ s"${parBottom-parTop}" }
                         HPOS={ s"$parLeft" }
                         VPOS={ s"$parTop" }>{
                for (lineNode <- lineNodes)
                yield {
                  val lineXml = lineNode.asInstanceOf[Element]
                  val lineProps = getProperties(lineXml.getAttribute("title"))
                  val (lineLeft, lineTop, lineRight, lineBottom) = lineProps("bbox") match {
                    case DimPattern(l, t, r, b) => (l.toInt, t.toInt, r.toInt, b.toInt)
                    case _ => (0, 0, 0, 0)
                  }
                  val wordNodes = xpath.evaluate("span[@class='ocrx_word']",
                    lineNode, XPathConstants.NODESET).asInstanceOf[NodeList].toIterator.filter(
                      w => correctedTokens.contains(w.asInstanceOf[Element].getAttribute("id")))
                  if (wordNodes.isEmpty)
                    NodeSeq.Empty
                  else
                  <TextLine ID={ lineXml.getAttribute("id") }
                            WIDTH={ s"${lineRight-lineLeft}" }
                            HEIGHT={ s"${lineBottom-lineTop}" }
                            HPOS={ s"$lineTop" }
                            VPOS={ s"$lineLeft" }>{
                    var firstOnLine = true

                    for (wordNode <- wordNodes)
                    yield {
                      val wordXml = wordNode.asInstanceOf[Element]
                      val wordId = wordXml.getAttribute("id")
                      val wordProps = getProperties(wordXml.getAttribute("title"))
                      val (wordLeft, wordTop, wordRight, wordBottom) = wordProps("bbox") match {
                        case DimPattern(l, t, r, b) => (l.toInt, t.toInt, r.toInt, b.toInt)
                        case _ => (0, 0, 0, 0)
                      }

                      correctedTokens.get(wordId) match {
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
                              var hypPos = ht.firstToken.text.length-1
                              if (replacement.length < hypPos)
                                hypPos = replacement.length
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
                                      HPOS={ s"$wordTop" }
                                      VPOS={ s"$wordLeft" }
                                      CONTENT={ content }
                                      WC={ wordProps("x_wconf") }
                                      emop:DNC={ s"${token.noiseConf}" }/>

                          if (wordIsHyphenated)
                            altoStringNode = altoStringNode %
                              Attribute(null, "SUBS_CONTENT", subsContent,
                                Attribute(null, "SUBS_TYPE", subsType, Null))

                          // Add the alternatives
                          if (token.isMisspelled && token.bestReplacement.isDefined) {
                            import HOCRToken.preservePunctuationAndStyle

                            val alternatives =
                              mutable.LinkedHashSet(token.sortedContextMatches.map(_._1.toLowerCase): _*) ++
                                token.replacements.drop(token.defaultReplacementsCount).map(_.toLowerCase)

                            for (alt <- alternatives.map(preservePunctuationAndStyle(token, _))
                              .filterNot(txt => (txt equals content) || (txt equals token.text))
                              .take(altCount)) { // only record max 'altCount' correction alternatives
                              altoStringNode = add(altoStringNode, <ALTERNATIVE>{ alt }</ALTERNATIVE>)
                            }

                            // the last alternative is always the original token
                            altoStringNode = add(altoStringNode, <ALTERNATIVE>{ token.text }</ALTERNATIVE>)
                          }

                          nodeBuffer += altoStringNode

                          if (isFirstPartOfHyphen)
                            nodeBuffer += <HYP WIDTH="10" CONTENT="-"/>

                          nodeBuffer

                        case None => NodeSeq.Empty
                      }
                    }
                  }</TextLine>
                }
              }</TextBlock>
            }
          }</PrintSpace>
        </Page>
      }</Layout>
    </alto>
  }
}
