LIBNAME=HOLLight

.SUFFIXES:

.PHONY: default
default: Makefile.coq
	$(MAKE) -f Makefile.coq

Makefile.coq: _CoqProject
	coq_makefile -f _CoqProject -o $@

_CoqProject:
	echo "-R . $(LIBNAME) `ls *.v`" > $@

.PHONY: clean
clean: Makefile.coq
	$(MAKE) -f Makefile.coq $@
	rm -f _CoqProject Makefile.coq Makefile.coq.conf

%:
	$(MAKE) -f Makefile.coq $@
