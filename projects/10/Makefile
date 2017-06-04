jar = analyzer-0.1.0-SNAPSHOT-standalone.jar
project = 10
script = JackAnalyzer

$(script):
	echo "#!/bin/bash\njava" -jar $(jar) "\$$@" > $(script)
	chmod +x $(script)

clean:
	rm -f $(script); rm -f project$(project).zip; rm -f $(jar); rm -rf target; rm -rf *T.xml

uberjar:
	cd analyzer && lein uberjar && cp target/$(jar) ..

test-all: uberjar $(script)
	for dir in $(shell find . -mindepth 1 -maxdepth 1 -type d -and -not -name "analyzer"); do \
		find $$dir -name "*jack" -exec ./JackAnalyzer \{\} \; ;\
		find $$dir -name "*T.xml" -exec ../../tools/TextComparer.sh \{\} $$(pwd)/$$(basename \{\}) \; ;\
	done

zip: uberjar
	zip project$(project).zip Makefile analyzer/project.clj $(jar)

all: clean uberjar $(script) test-all zip
