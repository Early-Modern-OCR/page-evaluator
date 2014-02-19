# Page Evaluator
A tool for computing the SEASR correctability score for hOCR pages.

## Compiling and Deploying
This project uses Maven. To create the deployable JAR artifact use:

    > mvn package

This will generate the deployable PageEvaluator JAR file inside the `target/` folder.
The name of the file will be `PageEvaluator-<VERSION>.jar`.

## Running
To get information about the supported command line arguments, use:

    > java <JAVA_OPTS> -jar PageEvaluator-<VERSION>.jar --help
