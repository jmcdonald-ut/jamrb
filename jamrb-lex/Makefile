SDIR = source
BDIR = bin

COMPILER = raco exe
SOURCE_FILES = source/%.rkt

jamrb_lex: $(wildcard $(SDIR)/*.rkt)
	$(COMPILER) -o $(BDIR)/jamrb_lex $(SDIR)/jamrb-lex.rkt

run: jamrb_lex
	./bin/jamrb_lex

clean:
	rm bin/*
