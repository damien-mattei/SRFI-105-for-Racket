# example of Makefile for Scheme+ for Racket
# generic Makefile for all Scheme and Racket files
# author: Damien Mattei


SHELL := /bin/bash

# path to SRFI-105 directory
SRFI-105=../../../SRFI-105-for-Racket

# for MacOS:
#SCHEME_EXEC=/Applications/Racket\ v8.12/bin/racket
SCHEME_EXEC=racket


# Scheme+ for parser
#PARSER:=$(SCHEME_EXEC) $(SCHEME_PLUS)/src/curly-infix2prefix4racket.rkt
PARSER:=$(SRFI-105)/src/curly-infix2prefix4racket.rkt


# sub directory where parsed module files will be generated
MODULE_DIRECTORY=parsed_files_directory


# Scheme+ program to be parsed
MODULES_NAME=$(wildcard *+.rkt *+.scm)

# Scheme programs to create by parsing the Scheme+ files
OBJECT=$(addprefix $(MODULE_DIRECTORY)/,$(MODULES_NAME))




# create directory, build objects
# note: parsed files have the same name than source files but are in different directories
all: $(MODULE_DIRECTORY) $(OBJECT)


# create the sub directory where parsed module files will be
$(MODULE_DIRECTORY) :
	mkdir $@

# create Scheme files (*.scm , *+.scm , *-.scm)  by parsing Scheme+ files (*+.scm)
# if an error occur and the file is empty it will stop further Makefile call so i remove it:

.DELETE_ON_ERROR:

# create Scheme files (*.scm) by parsing Scheme+ files (*+.scm)
$(MODULE_DIRECTORY)/%+.scm: %+.scm
	@echo PARSING $< :
	$(PARSER) $< > $@

# create Scheme files (*.rkt) by parsing Scheme+ files (*+.rkt)
$(MODULE_DIRECTORY)/%+.rkt: %+.rkt
	@echo PARSING $< :
	$(PARSER) $< > $@


clean:
	rm -rf $(OBJECT)
	rm -rf $(MODULE_DIRECTORY)

