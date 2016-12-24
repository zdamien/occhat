occhat: chatMod.mli chatMod.ml chatMain.ml testChatMod.ml SQueue.ml*
	ocamlopt chatMod.mli
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.ml
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.cmx chatMod.ml
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.cmx chatMod.cmx chatMain.ml -o occhat
	ocamlc -thread unix.cma threads.cma SQueue.cmo chatMod.cmo testChatMod.ml -o test_occhat

sq:     SQueue.mli SQueue.ml testSQ.ml
	ocamlopt SQueue.mli
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.ml
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.cmx testSQ.ml -o testSQ

tsq:     SQueue.mli SQueue.ml tSQ.ml
	ocamlopt SQueue.mli
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.ml
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.cmx tSQ.ml -o tSQ

clean:
	rm *.cm? *.o tSQ testSQ occhat test_occhat a.out
