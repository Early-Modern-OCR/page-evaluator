package edu.illinois.i3.emop.apps.pagecorrector

import java.io.{BufferedReader, Reader}
import edu.illinois.i3.scala.utils.xml.XmlReader

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

/**
 * Trait for parsing plain-text files into lines and tokens
 */
trait SimpleTextPageParser extends PageParser {

  protected def createToken(id: String, text: String): TokenType

  override def getLines(reader: Reader): Try[Seq[Line[TokenType]]] = Try {
    val bufferedReader = reader match {
      case bufferedReader: BufferedReader => bufferedReader
      case _ => new BufferedReader(reader)
    }

    var id = -1  // gets incremented first, so actually token ids will start at 0
    Iterator.continually(bufferedReader.readLine())
      .takeWhile(line => line != null)
      .map(line => line.split("\\s+").toSeq)
      .map(tokens =>
        tokens.map(text => {
          id += 1
          createToken(id.toString, text)
        })
      )
      .toSeq
  }
}

/**
 * Trait for parsing hOCR XML documents into lines and tokens
 */
trait HOCRPageParser extends PageParser with XmlReader {
  import org.w3c.dom.{NodeList, Element}
  import javax.xml.xpath.{XPathConstants, XPathFactory}
  import edu.illinois.i3.scala.utils.implicits.XmlImplicits._

  protected def createToken(xmlWord: Element): TokenType

  override def getLines(reader: Reader): Try[Seq[Line[TokenType]]] = readXml(reader).map(getLines)

  /**
   * Deconstructs an hOCR page into a sequence of lines, where each line is a sequence of tokens
   *
   * @param document The hOCR document
   * @return The deconstructed document
   */
  def getLines(document: Document): Seq[Line[TokenType]] = {
    val xpathFactory = XPathFactory.newInstance
    val xpath = xpathFactory.newXPath
    val pageXml = xpath.evaluate("//*[@class='ocr_page']", document, XPathConstants.NODE)
    val xpathOCRLine = xpath.compile("descendant::*[@class='ocr_line']")
    val xpathOCRXWord = xpath.compile("descendant::*[@class='ocrx_word']")

    val lines = xpathOCRLine.evaluate(pageXml, XPathConstants.NODESET).asInstanceOf[NodeList].map { case xmlLine =>
      val xmlTokenSeq = xpathOCRXWord.evaluate(xmlLine, XPathConstants.NODESET).asInstanceOf[NodeList]
      xmlTokenSeq.map(xmlToken => createToken(xmlToken.asInstanceOf[Element])).toSeq
    }.toSeq

    lines
  }
}