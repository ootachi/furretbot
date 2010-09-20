OCAMLMAKEFILE:=/usr/share/ocamlmakefile/OCamlMakefile

SOURCES=query.ml googlelang.ml deviantart.ml danbooru.ml pixiv.ml fa.ml \
    youtube.ml irc.ml furretbot.ml
PACKS=curl extlib inifiles json-wheel netclient netstring oembed str unix \
    xml-light
RESULT=furretbot

include $(OCAMLMAKEFILE)

