OCAMLMAKEFILE:=/usr/share/ocamlmakefile/OCamlMakefile

SOURCES=query.ml googlelang.ml pixiv.ml fa.ml irc.ml furretbot.ml
PACKS=curl extlib inifiles json-wheel netstring str unix
RESULT=furretbot

include $(OCAMLMAKEFILE)

