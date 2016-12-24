occhat: chatMod.mli chatMod.ml chatMain.ml testChatMod.ml
	ocamlc chatMod.mli
	ocamlc unix.cma chatMod.ml
	ocamlc unix.cma chatMod.cmo chatMain.ml -o occhat
	ocamlc unix.cma chatMod.cmo testChatMod.ml -o test_occhat

clean:
	rm *.cm? *.o occhat test_occhat a.out
