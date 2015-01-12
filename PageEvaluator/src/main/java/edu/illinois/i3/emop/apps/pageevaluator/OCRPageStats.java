package edu.illinois.i3.emop.apps.pageevaluator;

public class OCRPageStats {

    ///////////////////////////////////////////
    // Page statistics
    ///////////////////////////////////////////

    // Note: correctable profile = tokens which, after cleaning, contain at most 2 non-alpha characters and at least 1 alpha character,
    //       have a length of at least 3, and do not contain 4 or more repeated characters in a run

    // Note: cleaning = removal of max MAX_LEADING_PUNCT_TO_REMOVE + MAX_TRAILING_PUNCT_TO_REMOVE total punctuation characters from the beginning and end of a token
    //       a token can be cleaned only if, after cleaning, the remaining substring has a length >= CLEAN_TOKEN_LEN_THRESHOLD

                                                    // number of...
    private int _tokenCount;                        // tokens on page
    private int _ge4RepeatedCharsTokenCount;        // tokens containing 4 or more repeated characters (not numbers) in a run
    private int _numberObjectsTokenCount;           // tokens that could represent numbers, dates, amounts of money, identifiers..etc. (are number based)
    private int _punctTokenCount;                   // tokens that are made up of exactly 1 punctuation character (non-alphanum)
    private int _lenGt1NonAlphaTokenCount;          // tokens of length > 1 that contain exclusively non-alpha characters (but are not made up entirely of numbers) (can be thought of as "garbage" tokens)
    private int _cleanOneNonAlphaNoRepTokenCount;   // tokens which, after cleaning, are of length at least 3, contain exactly 1 non-alpha character and at least 1 alpha, and no 4 or more repeated characters in a run
    private int _cleanTwoNonAlphaNoRepTokenCount;   // tokens which, after cleaning, are of length at least 3, contain exactly 2 non-alpha character and at least 1 alpha, and no 4 or more repeated characters in a run
    private int _cleanThreeOrMoreNonAlphaTokenCount;// tokens which, after cleaning, are of length at least 3, contain > 2 non-alpha character and at least 1 alpha (for correction purposes they can also be thought of as "garbage")
    private int _cleanAllAlphaNoRepTokenCount;      // tokens which, after cleaning, contain exclusively alpha characters and no 4 or more repeated characters in a run
    private int _cleanShortWordCount;               // tokens which, after cleaning, have length < 3 and are supposed to be words (i.e. no numbers, no single punctuation, no single letters)
    private int _singleLetterCount;                 // tokens made up of exactly 1 alpha character

    public int getTokenCount() {
        return _tokenCount;
    }

    public void setTokenCount(int tokenCount) {
        _tokenCount = tokenCount;
    }

    public int getIgnoredTokenCount() {
        return _numberObjectsTokenCount + _punctTokenCount + _singleLetterCount;
    }

    public int getGe4RepeatedCharsTokenCount() {
        return _ge4RepeatedCharsTokenCount;
    }

    public void setGe4RepeatedCharsTokenCount(int ge4RepeatedCharsTokenCount) {
        _ge4RepeatedCharsTokenCount = ge4RepeatedCharsTokenCount;
    }

    public int getNumberObjectsTokenCount() {
        return _numberObjectsTokenCount;
    }

    public void setNumberObjectsTokenCount(int numbersTokenCount) {
        _numberObjectsTokenCount = numbersTokenCount;
    }

    public int getPunctTokenCount() {
        return _punctTokenCount;
    }

    public void setPunctTokenCount(int punctTokenCount) {
        _punctTokenCount = punctTokenCount;
    }

    public int getLenGt1NonAlphaTokenCount() {
        return _lenGt1NonAlphaTokenCount;
    }

    public void setLenGt1NonAlphaTokenCount(int lenGt1NonAlphaTokenCount) {
        _lenGt1NonAlphaTokenCount = lenGt1NonAlphaTokenCount;
    }

    public int getCleanOneNonAlphaNoRepTokenCount() {
        return _cleanOneNonAlphaNoRepTokenCount;
    }

    public void setCleanOneNonAlphaNoRepTokenCount(int cleanOneNonAlphaNoRepTokenCount) {
        _cleanOneNonAlphaNoRepTokenCount = cleanOneNonAlphaNoRepTokenCount;
    }

    public int getCleanTwoNonAlphaNoRepTokenCount() {
        return _cleanTwoNonAlphaNoRepTokenCount;
    }

    public void setCleanTwoNonAlphaNoRepTokenCount(int cleanTwoNonAlphaNoRepTokenCount) {
        _cleanTwoNonAlphaNoRepTokenCount = cleanTwoNonAlphaNoRepTokenCount;
    }

    public int getCleanThreeOrMoreNonAlphaTokenCount() {
        return _cleanThreeOrMoreNonAlphaTokenCount;
    }

    public void setCleanThreeOrMoreNonAlphaTokenCount(int cleanThreeOrMoreNonAlphaTokenCount) {
        _cleanThreeOrMoreNonAlphaTokenCount = cleanThreeOrMoreNonAlphaTokenCount;
    }

    public int getCleanAllAlphaNoRepTokenCount() {
        return _cleanAllAlphaNoRepTokenCount;
    }

    public void setCleanAllAlphaNoRepTokenCount(int cleanAllAlphaNoRepTokenCount) {
        _cleanAllAlphaNoRepTokenCount = cleanAllAlphaNoRepTokenCount;
    }

    public int getCleanShortWordCount() {
        return _cleanShortWordCount;
    }

    public void setCleanShortWordCount(int cleanShortWordCount) {
        _cleanShortWordCount = cleanShortWordCount;
    }

    public int getSingleLetterCount() {
        return _singleLetterCount;
    }

    public void setSingleLetterCount(int singleLetterCount) {
        _singleLetterCount = singleLetterCount;
    }

    public float getCorrectableScore() {
        Float score = (float)
                // number of tokens matching the "correctable profile"
                (_cleanAllAlphaNoRepTokenCount + _cleanOneNonAlphaNoRepTokenCount + _cleanTwoNonAlphaNoRepTokenCount)
                // divided by
                /
                // max number of potentially correctable tokens
                (_tokenCount - getIgnoredTokenCount() - _cleanShortWordCount);

        return score.isNaN() ? -1f : score;
    }

    public float getQualityScore() {
        Float score = (float) (_cleanAllAlphaNoRepTokenCount + _cleanOneNonAlphaNoRepTokenCount + _cleanTwoNonAlphaNoRepTokenCount) / _tokenCount;

        return score.isNaN() ? -1f : score;
    }
}
