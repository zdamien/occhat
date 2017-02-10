occhat: chatMod.mli chatMod.ml chatMain.ml testChatMod.ml SQueue.ml*
	make sq
	ocamlopt chatMod.mli
	ocamlopt -c -thread unix.cmxa threads.cmxa SQueue.cmx chatMod.ml
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.cmx chatMod.cmx chatMain.ml -o occhat
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.cmx chatMod.cmx testChatMod.ml -o test_occhat

sq:     SQueue.mli SQueue.ml testSQ.ml
	ocamlopt SQueue.mli
	ocamlopt -c -thread unix.cmxa threads.cmxa SQueue.ml
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.cmx testSQ.ml -o testSQ

tsq:     SQueue.mli SQueue.ml tSQ.ml
	make sq
	ocamlopt -thread unix.cmxa threads.cmxa SQueue.cmx tSQ.ml -o tSQ

clean:
	rm *.cm? *.o tSQ testSQ occhat test_occhat
