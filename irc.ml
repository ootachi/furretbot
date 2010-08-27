(* furretbot/irc.ml *)

exception Disconnected
exception Quit

type conn_info = {
    ci_serverhost: string;
    ci_serverport: int;
    ci_password: string option;
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
| MS_pass of string
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
        | MS_pass pass -> "PASS", [| pass |]
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
        | MS_custom(verb, args) ->
            Std.print("deparsing custom message", verb, args);
            verb, args
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
    Option.may (DynArray.add args) long_arg;
    try
        match verb with
        | "PASS" -> MS_pass (DynArray.get args 0)
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
    Netsys.really_write sock str 0 (String.length str)

let recv sock =
    let buf = Buffer.create 1 in
    let ch = String.create 1 in
    while ch <> "\n" do
        Netsys.really_read sock ch 0 1;
        Buffer.add_string buf ch
    done;
    let str = Buffer.contents buf in
    parse str

let close sock =
    send sock (MS_quit "Weaselbot");
    Unix.close sock

let connect conn_info =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let hostent = Unix.gethostbyname conn_info.ci_serverhost in
    let addr = hostent.Unix.h_addr_list.(0) in
    Unix.connect sock (Unix.ADDR_INET(addr, conn_info.ci_serverport));

    Unix.setsockopt_float sock Unix.SO_RCVTIMEO (60.0 *. 5.0);
    Unix.setsockopt_float sock Unix.SO_SNDTIMEO 60.0;

    ignore (recv sock);

    Option.may (fun pass -> send sock (MS_pass pass)) conn_info.ci_password;
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
                let try_handler f =
                    if
                        try
                            f sock msg
                        with e ->
                            prerr_endline (Printexc.to_string e);
                            false
                    then
                        raise Exit
                in
                List.iter try_handler handlers
            with Exit -> ()
        done

    let base_handler sock msg =
        match msg with
        | MS_ping cookie -> send sock (MS_pong cookie); true
        | _ -> false
end

