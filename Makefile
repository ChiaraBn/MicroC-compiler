TARGET=microcc

default: $(TARGET).native

$TARGET: default

native: $(TARGET).native

%.native:
	ocamlbuild -use-ocamlfind -package llvm,llvm.bitwriter,llvm.scalar_opts $@
	mv $@ $*

clean:
	ocamlbuild -clean ;\
	rm -f a.bc;

.PHONY: clean default
