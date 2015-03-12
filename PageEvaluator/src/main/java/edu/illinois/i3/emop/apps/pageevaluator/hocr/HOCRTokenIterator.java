package edu.illinois.i3.emop.apps.pageevaluator.hocr;

import com.google.common.collect.AbstractIterator;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.xpath.*;
import java.util.Iterator;

public class HOCRTokenIterator extends AbstractIterator<HOCRToken> implements Iterable<HOCRToken> {
    private final Element _pageXml;
    private final NodeList _lines;
    private final int _lineCount;
    private int _currentLineIndex;
    private final XPathExpression _xpathToken;
    private NodeList _currentLineTokens;
    private int _currentLineTokenCount;
    private int _currentTokenIndex;

    public HOCRTokenIterator(Element pageXml) {
        _pageXml = pageXml;

        XPath xpath = XPathFactory.newInstance().newXPath();
        try {
            _xpathToken = xpath.compile("descendant::*[@class='ocrx_word']");
            _lines = (NodeList) xpath.evaluate("descendant::*[@class='ocr_line']", pageXml, XPathConstants.NODESET);
        }
        catch (XPathExpressionException e) {
            throw new RuntimeException(e);
        }

        _lineCount = _lines.getLength();
        _currentLineIndex = _currentTokenIndex = _currentLineTokenCount = -1;

        advance();
    }

    @Override
    protected HOCRToken computeNext() {
        if (_currentLineTokens == null)
            return endOfData();

        Element wordXml = (Element) _currentLineTokens.item(_currentTokenIndex);
        boolean isLastTokenOnLine = _currentTokenIndex == _currentLineTokenCount - 1;

        HOCRToken word = new HOCRToken(wordXml, isLastTokenOnLine);

        // Advance to next token
        advance();

        return word;
    }

    @Override
    public Iterator<HOCRToken> iterator() {
        return new HOCRTokenIterator(_pageXml);
    }

    protected void advance() {
        _currentTokenIndex++;

        if (_currentTokenIndex >= _currentLineTokenCount) {
            do {
                // No more tokens on current line - try to advance to next line
                _currentLineIndex++;
                _currentTokenIndex = 0;

                if (_currentLineIndex < _lineCount) {
                    Node currentLineXml = _lines.item(_currentLineIndex);
                    try {
                        _currentLineTokens = (NodeList) _xpathToken.evaluate(currentLineXml, XPathConstants.NODESET);
                        _currentLineTokenCount = _currentLineTokens.getLength();
                    } catch (XPathExpressionException e) {
                        throw new RuntimeException(e);
                    }
                } else
                    // No more lines
                    _currentLineTokens = null;
            } while (_currentLineTokenCount == 0 && _currentLineTokens != null);
        }
    }
}
