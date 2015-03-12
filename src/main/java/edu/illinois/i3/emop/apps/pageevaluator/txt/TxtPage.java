package edu.illinois.i3.emop.apps.pageevaluator.txt;


import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import edu.illinois.i3.emop.apps.pageevaluator.OCRPage;
import edu.illinois.i3.emop.apps.pageevaluator.OCRToken;
import edu.illinois.i3.emop.apps.pageevaluator.exceptions.PageParserException;
import opennlp.tools.tokenize.Tokenizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TxtPage extends OCRPage<TxtPageStats> {

    private static final Logger log = LoggerFactory.getLogger(TxtPage.class);
    private static final Pattern HYPHEN_WORD_MATCHER = Pattern.compile("(?m)(\\S*\\p{L})-\\n(\\p{L}\\S*)\\s*");

    private final String _pageId;
    private final List<TxtToken> _tokens;

    private TxtPage(String pageId, String[] tokens) {
        _pageId = pageId;

        _tokens = Lists.newArrayListWithExpectedSize(tokens.length);
        for (String token : tokens)
            _tokens.add(new TxtToken(token));
    }

    public static TxtPage parse(Reader pageReader, String pageId, Tokenizer tokenizer) throws PageParserException {
        BufferedReader reader = (pageReader instanceof BufferedReader) ?
            (BufferedReader) pageReader : new BufferedReader(pageReader);
        try {
            StringBuilder sb = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty())
                    sb.append(line).append('\n');
            }

            // combine hyphenated words
            String text = sb.toString();
            Matcher matcher = HYPHEN_WORD_MATCHER.matcher(text);
            text = matcher.replaceAll("$1$2\n");

            String[] tokens = tokenizer.tokenize(text);
            return new TxtPage(pageId, tokens);
        }
        catch (IOException e) {
            log.error("Txt parser error", e);
            throw new PageParserException(e);
        }
    }

    @Override
    public Iterator<? extends OCRToken> getTokenIterator() {
        return _tokens.iterator();
    }

    public String getPageId() {
        return _pageId;
    }
}
