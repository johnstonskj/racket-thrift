PACKAGENAME=thrift
COLLECTS=$(PACKAGENAME)
SCRBL=scribblings/$(PACKAGENAME).scrbl

all: setup test

clean:
	find . -name compiled -type d | xargs rm -rf
	find . -name "*~" -type f |xargs rm
	rm -rf ./doc
	rm -rf ./coverage

setup:
	raco setup --tidy $(COLLECTS)

link:
	raco pkg install --link -n $(PACKAGENAME) $(shell pwd)

unlink:
	raco pkg remove $(PACKAGENAME)

test:
	raco test -t -c $(COLLECTS)

coverage:
	raco cover -v -b -f coveralls -p $(PACKAGENAME)

readme: README.md
	markdown -r markdown_github -w html5 -o ./doc/readme.html \
		--standalone --self-contained README.md

htmldocs: $(SCRBL)
	raco scribble \
		--html \
		--dest $(COLLECTS)/doc \
		--dest-name index \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		$(SCRBL)

viewdocs:
	raco docs

thrift: format.rkt
	rthrift -o parquet/generated -m parquet/generated parquet/format.rkt
