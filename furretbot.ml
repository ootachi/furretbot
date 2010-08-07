#!/usr/bin/env ocamlscript
Ocaml.packs := [ "curl"; "extlib"; "inifiles"; "netstring"; "str"; "unix" ];
Ocaml.sources := [ "query.ml" ]
--
module Fa = struct
    type t = {
        fa_cookiestore: string;
        fa_username: string;
        fa_password: string
    }

    type rating = RA_general | RA_mature | RA_explicit

    type submission = {
        su_id: int;
        su_url: string;
        su_title: string;
        su_artist: string;
        su_rating: rating;
        su_faves: int;
        su_comments: int;
        su_views: int
    }

    let with_curl_request fa f =
        let req = Curl.init() in
        Std.finally (fun() -> Curl.cleanup req) begin fun() ->
            Curl.set_cookiefile req fa.fa_cookiestore;
            Curl.set_cookiejar req fa.fa_cookiestore;
            Curl.set_verbose req true;
            Curl.set_useragent req
                "Mozilla/4.0 (compatible; U; MSIE 6.0; Windows NT 5.1)";
            f req
        end ()

    let login fa =
        with_curl_request fa begin fun req ->
            Curl.set_url req "https://www.furaffinity.net/login/?url=/";
            Curl.set_post req true;
            Curl.set_referer req "https://www.furaffinity.net/login/";

            let post =
                Netencoding.Url.mk_url_encoded_parameters [
                    ("action", "login");
                    ("retard_protection", "1");
                    ("name", fa.fa_username);
                    ("pass", fa.fa_password);
                    ("login", "Login to&nbsp;FurAffinity")
                ]
            in
            Curl.set_postfields req post;
            Curl.perform req
        end

    let login_if_necessary fa =
        if not (Sys.file_exists fa.fa_cookiestore) then login fa

    let view_link_re =
        lazy (Str.regexp ".*furaffinity\\.net/view/\\([0-9]+\\)")
    let has_view_link str = Str.string_match (Lazy.force view_link_re) str 0
    let find_view_link str =
        ignore (Str.string_match (Lazy.force view_link_re) str 0);
        int_of_string (Str.matched_group 1 str)

    let url_of_submission id =
        Printf.sprintf "http://www.furaffinity.net/view/%d/" id
   
    let get_submission fa id =
        login_if_necessary fa;

        let url = url_of_submission id in

        let buf = Buffer.create 0 in
        with_curl_request fa begin fun req ->
            Curl.set_url req url;
            Curl.set_writefunction req
                (fun s -> Buffer.add_string buf s; String.length s);
            Curl.perform req
        end;

        let body = Buffer.contents buf in
        let html = Nethtml.parse (new Netchannels.input_string body) in
        let html_q() = ExtList.List.enum html in

        let html_title = Query.tag "title" (html_q()) in
        let html_title = Query.text (Query.tag "title" html_title) in

        let title_re = lazy (Str.regexp
            "^\\(.*\\) by \\(.*\\) -- Fur Affinity \\[dot\\] net$") in
        assert (Str.string_match (Lazy.force title_re) html_title 0);
        let title = Str.matched_group 1 html_title in
        let artist = Str.matched_group 2 html_title in

        let rating =
            if not (Enum.is_empty (Query.attr "alt" "Mature rating"
                    (html_q()))) then
                RA_mature
            else if not (Enum.is_empty (Query.attr "alt" "Adult rating"
                    (html_q()))) then
                RA_explicit
            else
                RA_general
        in

        let submission_div = Query.id "submission" (html_q()) in
        let maintables = Query.klass "maintable" submission_div in
        let info_maintable = Query.get ~index:2 maintables in
        let alt1s = Query.klass "alt1" (Query.of_node info_maintable) in
        let info_alt1 = Query.get ~index:1 alt1s in
        let info = Query.text (Query.of_node info_alt1) in

        let nbsp_re = lazy (Str.regexp "\\(&nbsp;\\)+") in
        let info = Str.split (Lazy.force nbsp_re) info in
        let info = List.map ExtString.String.strip info in

        let ws_re = lazy (Str.regexp "[ \t\013\n]+") in
        let info = List.map
            (Str.global_substitute (Lazy.force ws_re) (fun _ -> " ")) info in

        let info = List.filter (fun s -> String.contains s ':') info in
        let info = List.map (fun s -> ExtString.String.split s ":") info in
        let info = List.map
            (fun (a, b) -> (a, ExtString.String.strip b)) info in
        
        let faves = int_of_string (List.assoc "Favorites" info) in
        let comments = int_of_string (List.assoc "Comments" info) in
        let views = int_of_string (List.assoc "Views" info) in

        {
            su_id = id;
            su_url = url;
            su_title = title;
            su_artist = artist;
            su_rating = rating;
            su_faves = faves;
            su_comments = comments;
            su_views = views
        }
end

module Irc = struct
    exception Disconnected
    exception Quit

    type conn_info = {
        ci_serverhost: string;
        ci_serverport: int;
        ci_nick: string;
        ci_username: string;
        ci_realname: string;
        ci_autojoin: string list
    }

    type user_msg = {
        um_username: string;
        um_hostname: string;
        um_servername: string;
        um_realname: string
    }

    type msg =
    | MS_nick of string
    | MS_user of user_msg
    | MS_join of string
    | MS_privmsg of string * string
    | MS_ping of string
    | MS_pong of string
    | MS_quit of string
    | MS_custom of string * string array

    type msg_with_sender = string * msg

    let deparse msg =
        let verb, args =
            match msg with
            | MS_nick nick -> "NICK", [| nick |]
            | MS_user user_msg ->
                "USER",
                [|
                    user_msg.um_username;
                    user_msg.um_hostname;
                    user_msg.um_servername;
                    user_msg.um_realname
                |]
            | MS_join chan -> "JOIN", [| chan |]
            | MS_privmsg(target, msg) -> "PRIVMSG", [| target; msg |]
            | MS_ping cookie -> "PING", [| cookie |]
            | MS_pong cookie -> "PONG", [| cookie |]
            | MS_quit msg -> "QUIT", [| msg |]
            | MS_custom(verb, args) -> verb, args
        in
        let last_arg_idx = Array.length args - 1 in
        if String.contains args.(last_arg_idx) ' ' then
            args.(last_arg_idx) <- ":" ^ args.(last_arg_idx);
        (String.concat " " (verb::(Array.to_list args))) ^ "\n"

    let parse str =
        let colon_re = lazy (Str.regexp " *:") in

        let verb_and_args =
            if str.[0] == ':' then
                snd (ExtString.String.split str " ")
            else
                str
        in
        let verb_and_args, long_arg =
            try
                let pos = Str.search_forward (Lazy.force colon_re)
                    verb_and_args 0 in
                Str.string_before verb_and_args pos,
                    Some (Str.string_after verb_and_args (Str.match_end()))
            with Not_found ->
                verb_and_args, None
        in
        let verb_and_args = ExtString.String.nsplit verb_and_args " " in
        let verb = List.hd verb_and_args in
        let args = DynArray.of_list (List.tl verb_and_args) in
        Std.print ("long_arg", long_arg);
        Option.may (DynArray.add args) long_arg;
        try
            match verb with
            | "NICK" -> MS_nick (DynArray.get args 0)
            | "USER" ->
                MS_user {
                    um_username = DynArray.get args 0;
                    um_hostname = DynArray.get args 1;
                    um_servername = DynArray.get args 2;
                    um_realname = DynArray.get args 3
                }
            | "JOIN" -> MS_join(DynArray.get args 0)
            | "PRIVMSG" -> MS_privmsg(DynArray.get args 0, DynArray.get args 1)
            | "PING" -> MS_ping(DynArray.get args 0)
            | "PONG" -> MS_pong(DynArray.get args 0)
            | "QUIT" -> MS_quit(DynArray.get args 0)
            | _ -> MS_custom(verb, DynArray.to_array args)
        with DynArray.Invalid_arg _ ->
            failwith (Printf.sprintf
                "received message '%s' with too few parameters" verb)

    let send sock msg =
        let str = deparse msg in
        Std.print str;
        Netsys.really_write sock str 0 (String.length str)

    let recv sock =
        let buf = Buffer.create 1 in
        let ch = String.create 1 in
        while ch <> "\n" do
            Netsys.really_read sock ch 0 1;
            Buffer.add_string buf ch
        done;
        let str = Buffer.contents buf in
        Std.print str;
        parse str

    let close sock =
        send sock (MS_quit "Weaselbot");
        Unix.close sock

    let connect conn_info =
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        let hostent = Unix.gethostbyname conn_info.ci_serverhost in
        let addr = hostent.Unix.h_addr_list.(0) in
        Unix.connect sock (Unix.ADDR_INET(addr, conn_info.ci_serverport));

        ignore (recv sock);

        send sock (MS_nick conn_info.ci_nick);
        send sock (MS_user {
            um_username = conn_info.ci_username;
            um_hostname = Unix.gethostname();
            um_servername = conn_info.ci_serverhost;
            um_realname = conn_info.ci_realname
        });

        let join chan = send sock (MS_join chan) in
        List.iter join conn_info.ci_autojoin;

        sock

    module Bot = struct
        let run sock handlers =
            while true do
                let msg = recv sock in
                try
                    let try_handler f = if f sock msg then raise Exit in
                    List.iter try_handler handlers
                with Exit -> ()
            done

        let base_handler sock msg =
            match msg with
            | MS_ping cookie -> send sock (MS_pong cookie); true
            | _ -> false
    end
end

let handle_fa_link fa conn msg =
    Std.print msg;
    try
        match msg with
        | Irc.MS_privmsg(target, msg) when Fa.has_view_link msg ->
            let sub = Fa.get_submission fa (Fa.find_view_link msg) in
            let plural n = if n == 1 then "" else "s" in
            let response = Printf.sprintf
                "FA submission %d: %s by %s (%d view%s, %d fave%s)"
                sub.Fa.su_id
                sub.Fa.su_title
                sub.Fa.su_artist
                sub.Fa.su_views
                (plural sub.Fa.su_views)
                sub.Fa.su_faves
                (plural sub.Fa.su_faves) in
            Irc.send conn (Irc.MS_privmsg(target, response));
            true
        | _ -> false
    with _ -> false

let run fa_info conn_info =
    let sock = Irc.connect conn_info in
    try
        let handlers = [ Irc.Bot.base_handler; handle_fa_link fa_info ] in
        Std.finally (fun() -> Irc.close sock) (Irc.Bot.run sock) handlers
    with
    | Irc.Disconnected -> exit 1
    | Irc.Quit -> ()

let rot13 str =
    let transliterate ch =
        let addend =
            if (ch >= 'A' && ch < 'N') || (ch >= 'a' && ch < 'n') then
                13
            else if (ch >= 'N' && ch <= 'Z') || (ch >= 'N' && ch <= 'z') then
                -13
            else
                0
        in
        String.make 1 (Char.chr (addend + Char.code ch))
    in
    ExtString.String.replace_chars transliterate str

let main() =
    let optparser = OptParse.OptParser.make ~usage:"%prog config.ini" () in
    if Array.length Sys.argv < 2 then begin
        OptParse.OptParser.usage optparser ();
        exit 1
    end;

    let config = new Inifiles.inifile Sys.argv.(1) in

    let fa_info = {
        Fa.fa_cookiestore = config#getval "FA" "cookiestore";
        Fa.fa_username = config#getval "FA" "username";
        Fa.fa_password = rot13(config#getval "FA" "password")
    } in
    let conn_info = {
        Irc.ci_serverhost = config#getval "IRC" "serverhost";
        Irc.ci_serverport = int_of_string (config#getval "IRC" "serverport");
        Irc.ci_nick = config#getval "IRC" "nick";
        Irc.ci_username = config#getval "IRC" "username";
        Irc.ci_realname = config#getval "IRC" "realname";
        Irc.ci_autojoin =
            ExtString.String.nsplit (config#getval "IRC" "autojoin") " "
    } in

    run fa_info conn_info
;;

main()

