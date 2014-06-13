package edu.illinois.i3.emop.apps.pageevaluator;

import com.google.common.base.Charsets;
import com.google.common.io.Files;
import com.martiansoftware.jsap.*;
import com.martiansoftware.jsap.stringparsers.EnumeratedStringParser;
import com.martiansoftware.jsap.stringparsers.FileStringParser;
import edu.illinois.i3.emop.apps.pageevaluator.exceptions.PageParserException;
import edu.illinois.i3.emop.apps.pageevaluator.hocr.HOCRPage;
import edu.illinois.i3.emop.apps.pageevaluator.txt.TxtPage;
import opennlp.tools.tokenize.SimpleTokenizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.Reader;

public class Main {

    private static final Logger log = LoggerFactory.getLogger(Main.class);

    public enum DocumentFormat {
        TXT, HOCR, GALEXML
    }

    public static void main(String[] args) {
        try {
            JSAPResult cmdLine = parseArguments(args);
            DocumentFormat format = DocumentFormat.valueOf(cmdLine.getString("format").toUpperCase());
            File pageOcrFile = cmdLine.getFile("pageOcrFile");
            boolean quiet = cmdLine.getBoolean("quiet");

            if (!quiet)
                log.info("Processing {}: {}", format, pageOcrFile);

            Reader pageReader = Files.newReader(pageOcrFile, Charsets.UTF_8);
            OCRPageStats pageStats = processDocument(pageReader, pageOcrFile.getName(), format);
            float correctableScore = pageStats.getCorrectableScore();
            float qualityScore = pageStats.getQualityScore();

            if (!quiet)
                log.info("Scores: correctable={}, quality={}", correctableScore, qualityScore);
            else
                System.out.println(String.format("%f,%f", correctableScore, qualityScore));
        }
        catch (Exception e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static Parameter[] getApplicationParameters() {
        Parameter format = new FlaggedOption("format")
                .setStringParser(EnumeratedStringParser.getParser("txt;hocr;galexml"))
                .setDefault("hocr")
                .setShortFlag('f')
                .setHelp("Specifies the format of the page OCR file");

        Parameter quiet = new Switch("quiet")
                .setShortFlag('q')
                .setDefault("false")
                .setHelp("Enables quiet mode - only page scores are printed, separated by a comma");

        Parameter pageOcrFile = new UnflaggedOption("pageOcrFile")
                .setStringParser(
                        FileStringParser.getParser()
                                .setMustBeFile(true)
                                .setMustExist(true))
                .setRequired(true)
                .setHelp("The page OCR file");

        return new Parameter[] { format, quiet, pageOcrFile };
    }

    private static OCRPageStats processDocument(Reader pageReader, String id, DocumentFormat format) throws PageParserException {
        OCRPage ocrPage;

        switch (format) {
            case HOCR:
                ocrPage = HOCRPage.parse(pageReader);
                break;

            case TXT:
                ocrPage = TxtPage.parse(pageReader, id, SimpleTokenizer.INSTANCE);
                break;

            default:
                throw new RuntimeException("Unsupported format: " + format);
        }

        return ocrPage.calculateStatistics();
    }

    private static String getApplicationHelp() {
        return "Compute a score that estimates the correctability of an OCR'd page";
    }

    private static JSAPResult parseArguments(String[] args) throws JSAPException {
        SimpleJSAP jsap = new SimpleJSAP("PageEvaluator", getApplicationHelp(), getApplicationParameters());
        JSAPResult result = jsap.parse(args);

        if (jsap.messagePrinted())
            System.exit(1);

        return result;
    }
}
