
REBAR?=./rebar

ARCHIVETAG?=$(shell git describe --always --long --tags)
ARCHIVE?=$(shell basename $(CURDIR))-$(ARCHIVETAG)

.PHONY: all archive clean deps compile xref doc test eunit eqc eqcmini proper triq \
	compile-for-eunit compile-for-eqc compile-for-eqcmini compile-for-proper compile-for-triq

all: compile

archive:
	@rm -f $(ARCHIVE).tar.gz
	git archive --format=tar --prefix=$(ARCHIVE)/ $(ARCHIVETAG) | gzip >$(ARCHIVE).tar.gz

deps:
	$(REBAR) get-deps

clean:
	@rm -f $(ARCHIVE).tar.gz
	$(REBAR) clean

compile:
	$(REBAR) compile

xref:
	$(REBAR) xref skip_deps=true

doc:
	@rm -rf README.md doc/edoc-info doc/*.md
	$(REBAR) -C rebar.config.doc get-deps compile
	$(REBAR) -C rebar.config.doc doc skip_deps=true

test: eunit

eunit: compile-for-eunit
	$(REBAR) eunit skip_deps=true

eqc: compile-for-eqc
	$(REBAR) eqc skip_deps=true

eqcmini: compile-for-eqcmini
	$(REBAR) eqc skip_deps=true

proper: compile-for-proper
	@echo "rebar does not implement a 'proper' command" && false

triq: compile-for-triq
	$(REBAR) triq skip_deps=true

compile-for-eunit:
	$(REBAR) compile eunit compile_only=true

compile-for-eqc:
	$(REBAR) -D QC -D QC_EQC compile eqc compile_only=true

compile-for-eqcmini:
	$(REBAR) -D QC -D QC_EQCMINI compile eqc compile_only=true

compile-for-proper:
	$(REBAR) -D QC -D QC_PROPER compile eqc compile_only=true

compile-for-triq:
	$(REBAR) -D QC -D QC_TRIQ compile triq compile_only=true
