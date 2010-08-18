OCAMLMAKEFILE:=/usr/share/ocamlmakefile/OCamlMakefile

SOURCES=query.ml googlelang.ml pixiv.ml fa.ml youtube.ml irc.ml furretbot.ml
PACKS=csv curl extlib inifiles json-wheel netclient netstring str unix \
    xml-light
RESULT=furretbot

include $(OCAMLMAKEFILE)

