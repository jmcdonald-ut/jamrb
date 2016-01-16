SDIR = source
BDIR = bin

COMPILER = raco exe
SOURCE_FILES = source/%.rkt

ripper_sx: $(wildcard $(SDIR)/*.rkt)
	$(COMPILER) -o $(BDIR)/ripper_sx $(SDIR)/ripper-lexer.rkt

run: ripper-sx
	./bin/ripper-sx
