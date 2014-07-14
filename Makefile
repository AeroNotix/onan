RENAME ?= perl-rename
MOUTURE_VSN = 0.1.1
JSX_VSN = v2.0.4
MOUTURE = nox/mouture/archive/$(MOUTURE_VSN)
JSX = talentdeficit/jsx/archive/$(JSX_VSN)
DEPS := $(MOUTURE) $(JSX)
X := $(shell mkdir -p deps)


all: deps compile escriptize

deps: $(DEPS)
	$(RENAME) 's/-(\d\.|\d)+//g' deps/*

$(DEPS):/
	wget -qO- https://github.com/$@.tar.gz | tar -C deps/ -xzf -

compile:
	rebar compile

escriptize: compile
	rebar escriptize


.PHONY: all deps compile escriptize
