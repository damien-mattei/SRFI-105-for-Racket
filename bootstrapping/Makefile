# example of Makefile for Scheme+ for Racket
# generic Makefile for all Scheme and Racket files
# author: Damien Mattei (and some AI agentic now...)

# this Makefile parse all the Scheme+ file in the directory where the Makefile is.
# the resulting parsed scheme files are set in the subdirectory MODULE_DIRECTORY


SHELL := /bin/bash

# path to SRFI-105 directory
SRFI-105=..

# find the racket executable automatically so this Makefile works
# unmodified on both macOS and Linux:
# 1) prefer whatever "racket" is found on PATH
# 2) otherwise fall back to the usual per-OS install location
UNAME_S := $(shell uname -s)

SCHEME_EXEC := $(shell command -v racket 2>/dev/null)

ifeq ($(SCHEME_EXEC),)
ifeq ($(UNAME_S),Darwin)
SCHEME_EXEC := /Applications/Racket/bin/racket
else
SCHEME_EXEC := /usr/racket/bin/racket
endif
endif


# Scheme+ for parser
PARSER:=$(SRFI-105)/src/curly-infix2prefix4racket.rkt


# sub directory where parsed module files will be generated
MODULE_DIRECTORY=parsed_files_directory

INSTALL_DIRECTORY=..

# important: breaking the parser and you can not fix it because you're parser is broken unless you got a backup !
BACKUP_DIRECTORY=$(SRFI-105)/backup

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


# if an error occur and the file is empty it will stop further Makefile call so i remove it:
# but does my parser returns a valid error code?
.DELETE_ON_ERROR:

# create Scheme files (*+.scm) in MODULE_DIRECTORY by parsing Scheme+ files (*+.scm) in current directory
$(MODULE_DIRECTORY)/%+.scm: %+.scm
	@echo PARSING $< :
	$(SCHEME_EXEC) $(PARSER) $< > $@

# create Scheme files (*.rkt) by parsing Scheme+ files (*+.rkt)
$(MODULE_DIRECTORY)/%+.rkt: %+.rkt
	@echo PARSING $< :
	$(SCHEME_EXEC) $(PARSER) $< > $@

backup:
	@echo "Backing up parsed files in $(BACKUP_DIRECTORY) :"
	mkdir -p $(BACKUP_DIRECTORY)
	cp $(INSTALL_DIRECTORY)/main.rkt $(BACKUP_DIRECTORY)
	cp $(INSTALL_DIRECTORY)/annot+.rkt $(BACKUP_DIRECTORY)
	cp $(INSTALL_DIRECTORY)/src/curly-infix2prefix4racket.rkt $(BACKUP_DIRECTORY)
	@echo "Done."

install:
	@echo "Installing parsed files in $(INSTALL_DIRECTORY) :"
# old line : the files kept their names (main+.rkt went in ../main+.rkt and not in the active reader ../main.rkt)
#	cp $(MODULE_DIRECTORY)/* $(INSTALL_DIRECTORY)
	cp $(MODULE_DIRECTORY)/annot+.rkt $(INSTALL_DIRECTORY)/annot+.rkt
	cp $(MODULE_DIRECTORY)/main+.rkt $(INSTALL_DIRECTORY)/main.rkt
# init-annot-data is a macro : its type tables are copied in the compiled code of the modules that use it (the readers
# and the converters). racket file.rkt reuses a compiled .zo without checking its dependencies , so they must be
# recompiled here , otherwise the converter would keep the old tables.
	@echo "Compiling the installed files and the modules that use them :"
	$(SCHEME_EXEC) -l- raco make -v $(INSTALL_DIRECTORY)/annot+.rkt $(INSTALL_DIRECTORY)/main.rkt $(INSTALL_DIRECTORY)/src/curly-infix2prefix4racket.rkt
# SRFI 110 uses the same annotation pass : its reader and its converter too (ignored if SRFI-110 is not installed)
	-$(SCHEME_EXEC) -l- raco make -v $$($(SCHEME_EXEC) -e '(display (collection-file-path "main.rkt" "SRFI-110"))') $$($(SCHEME_EXEC) -e '(display (collection-file-path "curly-infix2prefix4racket.rkt" "SRFI-110" "src"))')
	@echo "Done."

clean:
	rm -rf $(OBJECT)
	rm -rf $(MODULE_DIRECTORY)

