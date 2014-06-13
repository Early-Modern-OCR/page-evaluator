package edu.illinois.i3.emop.apps.pagecorrector

import java.io.{BufferedReader, Reader}
import scala.util.Try
import PageParser._
import org.w3c.dom.Document

object PageParser {
  type Line[T <: Token] = Seq[T]
}

/**
 * Trait providing functionality for parsing a page to extract the set of lines,
 * where each line represents a set of tokens
 */
trait PageParser {
  type TokenType <: Token

  /**
   * Extracts the lines from the page, where each line consists of a sequence of tokens
   *
   * @param reader The reader from which to read the page content
   * @return The sequence of lines on the page
   */
  def getLines(reader: Reader): Try[Seq[Line[TokenType]]]
}

trait SimpleTextPageParser extends PageParser {

  protected def createToken(id: String, text: String): TokenType

  override def getLines(reader: Reader) = Try {
    val bufferedReader = reader match {
      case bufferedReader: BufferedReader => bufferedReader
      case _ => new BufferedReader(reader)
    }

    var id = -1  // gets incremented first, so actually token ids will start at 0
    Iterator.continually(bufferedReader.readLine()).takeWhile(_ != null).map(_.split("\\s+"))
      .map(_.map(text => { id += 1; createToken(id.toString, text) }).toSeq).toSeq
  }
}

trait HOCRPageParser extends PageParser {
  import javax.xml.parsers.DocumentBuilderFactory
  import org.xml.sax.InputSource
  import org.w3c.dom.{NodeList, Element}
  import javax.xml.xpath.{XPathConstants, XPathFactory}
  import edu.illinois.i3.scala.utils.implicits.XmlImplicits._

  protected def createToken(xmlWord: Element): TokenType

  private val documentBuilderFactory = DocumentBuilderFactory.newInstance()
  documentBuilderFactory.setNamespaceAware(false)
  documentBuilderFactory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)

  override def getLines(reader: Reader) = readXml(reader).map(getLines)

  def getLines(document: Document): Seq[Line[TokenType]] = {
    val xpathFactory = XPathFactory.newInstance
    val xpath = xpathFactory.newXPath
    val pageXml = xpath.evaluate("//*[@class='ocr_page']", document, XPathConstants.NODE)
    val xpathOCRLine = xpath.compile("descendant::*[@class='ocr_line']")
    val xpathOCRXWord = xpath.compile("descendant::*[@class='ocrx_word']")
    xpathOCRLine.evaluate(pageXml, XPathConstants.NODESET).asInstanceOf[NodeList].map {
      case xmlLine => (
        for {
          xmlWord <- xpathOCRXWord.evaluate(xmlLine, XPathConstants.NODESET).asInstanceOf[NodeList]
          token = createToken(xmlWord.asInstanceOf[Element])
        } yield token
        ).toSeq
    }.toSeq
  }

  def readXml(reader: Reader) = Try {
    val inputSource = new InputSource(reader)
    val documentBuilder = documentBuilderFactory.newDocumentBuilder
    documentBuilder.parse(inputSource)
  }
}