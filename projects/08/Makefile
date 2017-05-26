jar = vm-translator-0.2.0-SNAPSHOT-standalone.jar
project = 8

VMTranslator:
	echo "#!/bin/bash\njava" -jar $(jar) "\$$@" > VMTranslator
	chmod +x VMTranslator

clean:
	rm -f VMTranslator; rm -f project$(project).zip; rm -f $(jar); rm -rf target

uberjar:
	lein uberjar && cp target/$(jar) .

compile-all:
	find . -name "*.vm" -exec ./VMTranslator \{\} \;

test-all:
	cd ../../tools/ && find ../projects/0$(project)/ -name "*.tst" -and -not -name "*VME*" -exec ./CPUEmulator.sh \{\} \;

zip:
	zip project$(project).zip Makefile project.clj $(jar)

all: clean uberjar VMTranslator compile-all test-all zip
