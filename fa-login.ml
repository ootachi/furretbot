#!/usr/bin/env ocamlscript
Ocaml.packs := [ "curl"; "extlib"; "netclient"; "netstring"; "str" ];
Ocaml.sources := [ "query.ml"; "fa.ml" ]
--
if Array.length Sys.argv < 4 then begin
    prerr_endline "usage: fa-login.ml <credential-file> <username> <password>";
    exit 1
end;

let creds = Fa.login Sys.argv.(2) Sys.argv.(3) in
Std.print creds;

let out = open_out_bin Sys.argv.(1) in
Std.finally (fun() -> close_out out) (Marshal.to_channel out creds) []

