TARGET=microcc

default: $(TARGET).native

$TARGET: default

native: $(TARGET).native

%.native:
	ocamlbuild -use-ocamlfind -package llvm,llvm.bitwriter,llvm.scalar_opts $@
	mv $@ $*

clean:
	ocamlbuild -clean ; \
	rm -f rt-support.*; \
	rm -f a.*; \
	rm -f ext.*;

ext: 
	clang-11 -emit-llvm -S src/rt-support.c -o rt-support.ll; \
	llvm-as-11 rt-support.ll -o rt-support.bc; \

run: clean ext $TARGET
	./$(TARGET) $(f); \
	llvm-link-11 a.bc rt-support.bc -o ext.bc; \
	llc-11 -filetype=obj ext.bc; \
	clang-11 ext.o; \
	./a.out

.PHONY: clean default ext run