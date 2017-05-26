VMTranslator:
	echo "#!/bin/bash\njava" -jar vm-translator-0.1.0-SNAPSHOT-standalone.jar "\$$@" > VMTranslator
	chmod +x VMTranslator

clean:
	rm -f VMTranslator; rm -f project7.zip; rm -f vm-translator-0.1.0-SNAPSHOT-standalone.jar; rm -rf target

uberjar:
	lein uberjar && cp target/vm-translator-0.1.0-SNAPSHOT-standalone.jar .

compile-all:
	find . -name "*.vm" -exec ./VMTranslator \{\} \;

test-all:
	cd ../../tools/ && find ../projects/07/ -name "*.tst" -and -not -name "*VME*" -exec ./CPUEmulator.sh \{\} \;

zip:
	zip project7.zip Makefile project.clj vm-translator-0.1.0-SNAPSHOT-standalone.jar

all: clean uberjar VMTranslator compile-all test-all zip
