FILE = hol_upto_arith

COQ = coqc

.SUFFIXES:

.PHONY: default
default: $(FILE)_opam.vo
%.vo: %.v
	$(COQ) -R . HOLLight $<
$(FILE)_opam.vo: coq.vo theory_hol.vo $(FILE)_types.vo $(FILE)_terms.vo $(FILE)_axioms.vo

.PHONY: clean
clean:
	rm -f *.vo* *.glob .*.aux .lia.cache .nia.cache Makefile.coq Makefile.coq.conf

Makefile.coq:
	coq_makefile -f _CoqProject -o $@

install: Makefile.coq
	$(MAKE) -f Makefile.coq install
