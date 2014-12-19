MLOPT = ocamlopt
MLOPTFLAGS = -I ./build
LDFLAGS =
MLLIBS = unix.cmxa

BINARY = phnxd
MODULES      =  build/algos.cmx \
                build/strUtils.cmx \
                build/client.cmx \
	        build/pconfig.cmx
BUILDMODULES =  $(MODULES) build/main.cmx
TESTMODULES  =  $(MODULES) build/tests.cmx

VPATH = build:src

.PHONY: all


all: $(BINARY)

#	strip --strip-all $(BINARY)

$(BINARY): $(BUILDMODULES)
	$(MLOPT) $(MLOPTFLAGS) -o $@ $(MLLIBS) $^ $(LDFLAGS)

build/pconfig.cmx: pconfig.ml
	$(MLOPT) $(MLOPTFLAGS) -o $@ -pp camlp4o.opt -c $<

build/%.cmx: src/%.ml
	$(MLOPT) $(MLOPTFLAGS) -o $@ -c $<

clean:
	rm -f ./build/*
	rm -f $(BINARY)

scratch:
	$(MLOPT) $(MLOPTFLAGS) -o test/$@ test/scratch.ml

args:
	$(MLOPT) $(MLOPTFLAGS) -o test/$@ test/args.ml

tests: $(TESTMODULES)
	$(MLOPT) $(MLOPTFLAGS) -o test/$@ $(MLLIBS) $^
