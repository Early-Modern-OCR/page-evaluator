package edu.illinois.i3.emop.apps.pageevaluator.hocr;

import com.google.common.collect.Sets;
import edu.illinois.i3.emop.apps.pageevaluator.OCRPage;
import edu.illinois.i3.emop.apps.pageevaluator.exceptions.PageParserException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import java.io.Reader;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Set;


public class HOCRPage extends OCRPage<HOCRPageStats> {

    // Page metadata
    private final String _pageId;
    private final String _ocrEngine;
    private final Set<String> _ocrCapabilities;
    private final Element _pageXml;

    private HOCRPage(String pageId, Element pageXml, String ocrEngine, Set<String> ocrCapabilities) {
        _pageId = pageId;
        _pageXml = pageXml;
        _ocrEngine = ocrEngine;
        _ocrCapabilities = ocrCapabilities;
    }

    public static HOCRPage parse(Reader pageReader) throws PageParserException {
        try {
            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setNamespaceAware(false);
            documentBuilderFactory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
            DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
            InputSource inputSource = new InputSource(pageReader);
            Document document = documentBuilder.parse(inputSource);
            XPathFactory xpathFactory = XPathFactory.newInstance();
            XPath xpath = xpathFactory.newXPath();

            String ocrEngine = (String) xpath.evaluate("/html/head/meta[@name='ocr-system']/@content", document, XPathConstants.STRING);
            Set<String> ocrCapabilities = Sets.newHashSet();
            String capabilities = (String) xpath.evaluate("/html/head/meta[@name='ocr-capabilities']/@content", document, XPathConstants.STRING);
            ocrCapabilities.addAll(Arrays.asList(capabilities.split(" ")));

            NodeList pagesXml = (NodeList) xpath.evaluate("//*[@class='ocr_page']", document, XPathConstants.NODESET);
            Element pageXml = (Element) pagesXml.item(0);  // we only consider the first page
            String pageId = pageXml.getAttribute("id");

            return new HOCRPage(pageId, pageXml, ocrEngine, ocrCapabilities);
        }
        catch (Exception e) {
            throw new PageParserException(e);
        }
    }

    public String getPageId() {
        return _pageId;
    }

    public String getOcrEngine() {
        return _ocrEngine;
    }

    public Set<String> getOcrCapabilities() {
        return _ocrCapabilities;
    }

    @Override
    public Iterator<HOCRToken> getTokenIterator() {
        return new HOCRTokenIterator(_pageXml);
    }
}
