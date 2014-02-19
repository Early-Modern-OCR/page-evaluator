package edu.illinois.i3.emop.apps.pageevaluator;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class OCRPage<T extends OCRPageStats> {

    protected static final int MAX_LEADING_PUNCT_TO_REMOVE = 1;
    protected static final int MAX_TRAILING_PUNCT_TO_REMOVE = 3;
    protected static final int CLEAN_TOKEN_LEN_THRESHOLD = 3;

    protected final Pattern NonAlphaPattern = Pattern.compile("\\P{L}", Pattern.CANON_EQ);
    protected final Pattern PunctPattern = Pattern.compile("^\\p{Punct}$");
    protected final Pattern NumberBasedObjectPattern = Pattern.compile("^\\p{Sc}?[\\.,/\\-]?(\\p{N}+[\\.,/%\\-]?)+\\p{Sc}?$");
    protected final Pattern OneAlphaPattern = Pattern.compile("^\\p{L}$", Pattern.CANON_EQ);
    protected final Pattern Repeated4orMoreCharsPattern = Pattern.compile("(\\P{N})\\1{3,}", Pattern.CANON_EQ);


    public abstract Iterator<? extends OCRToken> getTokenIterator();

    public T calculateStatistics() {
        int tokenCount = 0;
        int ge4RepeatedCharsTokenCount = 0;
        int numberObjectsTokenCount = 0;
        int punctTokenCount = 0;
        int lenGt1NonAlphaTokenCount = 0;
        int cleanOneNonAlphaNoRepTokenCount = 0;
        int cleanTwoNonAlphaNoRepTokenCount = 0;
        int cleanThreeOrMoreNonAlphaTokenCount = 0;
        int cleanAllAlphaNoRepTokenCount = 0;
        int cleanShortWordCount = 0;
        int singleLetterCount = 0;

        Iterator<? extends OCRToken> tokenIterator = getTokenIterator();
        while (tokenIterator.hasNext()) {
            OCRToken token = tokenIterator.next();
            String tokenText = token.getText().trim();

            // join end of line hyphenated words
            if (token.isLastTokenOnLine() && tokenText.endsWith("-") && tokenIterator.hasNext()) {
                String nextTokenText = tokenIterator.next().getText().trim();
                tokenText = tokenText.substring(0, tokenText.length() - 1) + nextTokenText;
            }

            if (tokenText.isEmpty())
                continue;

            tokenCount++;

            String normTokenText = tokenText.toLowerCase();
            String cleanTokenText = cleanToken(normTokenText);

            // tokenText      = the default, not-normalized, token (trimmed)
            // normTokenText  = the normalized (lowercased) tokenText
            // cleanTokenText = the normTokenText with MAX_LEADING_PUNCT_REMOVE punctuation removed, and MAX_TRAILING_PUNCT_REMOVE punctuation removed
            //                  (can be 'null' if, after cleaning, the remaining substring has a length < CLEAN_TOKEN_LEN_THRESHOLD)

            Integer tokenLength = tokenText.length();
            Integer cleanTokenLength = cleanTokenText.length();

            Matcher punctMatcher = PunctPattern.matcher(tokenText);
            if (punctMatcher.matches()) {
                punctTokenCount++;
                continue;
            }

            Matcher numberMatcher = NumberBasedObjectPattern.matcher(tokenText);
            if (numberMatcher.matches()) {
                numberObjectsTokenCount++;
                continue;
            }

            Matcher singleAlphaMatcher = OneAlphaPattern.matcher(tokenText);
            if (singleAlphaMatcher.matches()) {
                singleLetterCount++;
                continue;
            }

            Matcher ge4RepeatedCharsMatcher = Repeated4orMoreCharsPattern.matcher(normTokenText);
            if (ge4RepeatedCharsMatcher.find()) {
                ge4RepeatedCharsTokenCount++;
                continue;
            }

            // compute the number of non-alpha characters in the cleaned token (if it contains no more than 3 repeated characters in a run)
            Matcher nonAlphaMatcher = NonAlphaPattern.matcher(cleanTokenText);
            int nonAlphaCount = 0;
            while (nonAlphaMatcher.find())
                nonAlphaCount++;

            if (nonAlphaCount == cleanTokenLength) {
                lenGt1NonAlphaTokenCount++;
                continue;
            }

            // a token can be cleaned only if, after cleaning, the remaining substring has a length >= 3
            if (cleanTokenLength < CLEAN_TOKEN_LEN_THRESHOLD) {
                cleanShortWordCount++;
                continue;
            }

            switch (nonAlphaCount) {
                case 0:
                    cleanAllAlphaNoRepTokenCount++;
                    break;

                case 1:
                    cleanOneNonAlphaNoRepTokenCount++;
                    break;

                case 2:
                    cleanTwoNonAlphaNoRepTokenCount++;
                    break;

                default:
                    cleanThreeOrMoreNonAlphaTokenCount++;
                    break;
            }
        }

        T pageStats = getNewTypeParameterInstance();
        pageStats.setCleanAllAlphaNoRepTokenCount(cleanAllAlphaNoRepTokenCount);
        pageStats.setCleanOneNonAlphaNoRepTokenCount(cleanOneNonAlphaNoRepTokenCount);
        pageStats.setCleanShortWordCount(cleanShortWordCount);
        pageStats.setCleanThreeOrMoreNonAlphaTokenCount(cleanThreeOrMoreNonAlphaTokenCount);
        pageStats.setCleanTwoNonAlphaNoRepTokenCount(cleanTwoNonAlphaNoRepTokenCount);
        pageStats.setGe4RepeatedCharsTokenCount(ge4RepeatedCharsTokenCount);
        pageStats.setLenGt1NonAlphaTokenCount(lenGt1NonAlphaTokenCount);
        pageStats.setNumberObjectsTokenCount(numberObjectsTokenCount);
        pageStats.setPunctTokenCount(punctTokenCount);
        pageStats.setSingleLetterCount(singleLetterCount);
        pageStats.setTokenCount(tokenCount);

        return pageStats;
    }

    protected String cleanToken(String token) {
        String cleanToken = token.replaceFirst("^\\p{Punct}{0," + MAX_LEADING_PUNCT_TO_REMOVE + "}", "")
                .replaceFirst("\\p{Punct}{0," + MAX_TRAILING_PUNCT_TO_REMOVE + "}$", "");

        return cleanToken;
    }

    private Class<T> getTypeParameterClass()
    {
        Type type = getClass().getGenericSuperclass();
        ParameterizedType paramType = (ParameterizedType) type;
        return (Class<T>) paramType.getActualTypeArguments()[0];
    }

    private T getNewTypeParameterInstance() {
        try {
            return getTypeParameterClass().newInstance();
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
