all: parser

LexChi.mll ParChi.mly ParChi.mli PrintChi.ml ShowChi.ml SkelChi.ml: Chi.cf
	bnfc -ocaml Chi.cf
	rm -f TestChi.ml

ParChi.ml: ParChi.mly
	ocamlyacc $<

LexChi.ml: LexChi.mll
	ocamllex $<

parser: ParChi.ml LexChi.ml

clean:
	rm -f AbsChi.ml
	rm -f BNFC_Util.ml
	rm -f LexChi.ml
	rm -f LexChi.mll
	rm -f ParChi.ml
	rm -f ParChi.mli
	rm -f ParChi.mly
	rm -f PrintChi.ml
	rm -f ShowChi.ml
	rm -f SkelChi.ml
