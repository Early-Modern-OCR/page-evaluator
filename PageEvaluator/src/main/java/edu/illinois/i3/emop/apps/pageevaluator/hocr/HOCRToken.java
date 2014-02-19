package edu.illinois.i3.emop.apps.pageevaluator.hocr;

import com.google.common.base.Objects;
import edu.illinois.i3.emop.apps.pageevaluator.OCRToken;
import org.w3c.dom.Element;

import java.util.Properties;

public class HOCRToken implements OCRToken {
    private final Element _tokenXml;
    private final boolean _isLastTokenOnLine;
    private final String _tokenId;
    private final Properties _tokenProperties;

    public HOCRToken(Element tokenXml, boolean isLastTokenOnLine) {
        _tokenXml = tokenXml;
        _isLastTokenOnLine = isLastTokenOnLine;
        _tokenId = _tokenXml.hasAttribute("id") ? _tokenXml.getAttribute("id") : null;

        _tokenProperties = new Properties();
        String title = _tokenXml.getAttribute("title");
        String[] props = title.split(";");
        for (String prop : props) {
            prop = prop.trim();
            int idx = prop.indexOf(" ");
            String propName = prop.substring(0, idx);
            String propValue = prop.substring(idx + 1);
            _tokenProperties.put(propName, propValue);
        }
    }

    public String getTokenId() {
        return _tokenId;
    }

    public Properties getTokenProperties() {
        return _tokenProperties;
    }

    public String getText() {
        return _tokenXml.getTextContent();
    }

    public boolean isLastTokenOnLine() {
        return _isLastTokenOnLine;
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(getText())
                .add("isLastTokenOnLine", _isLastTokenOnLine)
                .add("id", _tokenId)
                .add("properties", _tokenProperties)
                .toString();
    }
}
