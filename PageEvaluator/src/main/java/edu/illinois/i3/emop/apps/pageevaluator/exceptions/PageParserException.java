package edu.illinois.i3.emop.apps.pageevaluator.exceptions;

public class PageParserException extends Exception {

    public PageParserException(String message) {
        super(message);
    }

    public PageParserException(String message, Throwable cause) {
        super(message, cause);
    }

    public PageParserException(Throwable cause) {
        super(cause);
    }

}
