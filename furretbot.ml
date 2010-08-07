(* furretbot/furretbot.ml *)

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

