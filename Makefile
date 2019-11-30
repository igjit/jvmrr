JAVAS = $(wildcard java/*.java)
CLASSES = $(subst java/,inst/java/,$(JAVAS:.java=.class))

USERID = $(shell id -u)

all: $(CLASSES)

inst/java/%.class: java/%.java
	docker-compose run --rm -u $(USERID) jdk javac -d inst/java/ $^

clean:
	rm -f inst/java/*.class

.PHONY: all clean
