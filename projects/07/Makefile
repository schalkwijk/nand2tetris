VMTranslator:
	echo "#!/bin/bash\njava" -Xbootclasspath/a:$(shell pwd)/leiningen-2.7.1-standalone.jar -Dfile.encoding=UTF-8 -Dmaven.wagon.http.ssl.easy=false -Dmaven.wagon.rto=10000 -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -Dleiningen.original.pwd=$(shell pwd) -classpath :$(shell pwd)/leiningen-2.7.1-standalone.jar clojure.main -m leiningen.core.main run "\$$@" > VMTranslator
	chmod +x VMTranslator

clean:
	rm -f VMTranslator
