(* furretbot/furretbot.ml *)

let handle_art_sites fa pixiv conn msg =
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
        Std.print msg;
        Std.print(target, response);
        Irc.send conn (Irc.MS_privmsg(target, response));
        true
    | Irc.MS_privmsg(target, msg) when Pixiv.has_view_link msg ->
        let illust = Pixiv.get_illust pixiv (Pixiv.find_view_link msg) in
        let response = Printf.sprintf
            "Pixiv illust. %d: %s (%s) by %s (%s)"
            illust.Pixiv.il_id
            illust.Pixiv.il_title_ja
            illust.Pixiv.il_title_en
            illust.Pixiv.il_artist_ja
            illust.Pixiv.il_artist_en in
        Std.print msg;
        Std.print(target, response);
        Irc.send conn (Irc.MS_privmsg(target, response));
        true
    | _ -> false

let handle_youtube conn msg =
    match msg with
    | Irc.MS_privmsg(target, msg) when Youtube.has_view_link msg ->
        let video = Youtube.get_video (Youtube.find_view_link msg) in
        let plural n = if n == 1 then "" else "s" in
        let response = Printf.sprintf
            "YouTube video %s: %s by %s (%d view%s, %d favorite%s)"
            video.Youtube.vi_id
            video.Youtube.vi_title
            video.Youtube.vi_author
            video.Youtube.vi_views
            (plural video.Youtube.vi_views)
            video.Youtube.vi_favorites
            (plural video.Youtube.vi_favorites) in
        Std.print msg;
        Std.print(target, response);
        Irc.send conn (Irc.MS_privmsg(target, response));
        true
    | _ -> false

let run fa_info pixiv_info conn_info =
    let sock = Irc.connect conn_info in
    try
        let handlers = [
            Irc.Bot.base_handler;
            handle_art_sites fa_info pixiv_info;
            handle_youtube
        ] in
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
    let pixiv_info = {
        Pixiv.pi_cookiestore = config#getval "Pixiv" "cookiestore";
        Pixiv.pi_username = config#getval "Pixiv" "username";
        Pixiv.pi_password = rot13(config#getval "Pixiv" "password")
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

    run fa_info pixiv_info conn_info
;;

main()

