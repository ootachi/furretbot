(* furretbot/furretbot.ml *)

let truncate lyst max =
    let rec recur lyst n =
        match lyst with
        | [] -> []
        | _ when n == max -> [ "..." ]
        | first::rest -> first::(recur rest (n + 1))
    in
    recur lyst 0

let plural n = if n == 1 then "" else "s"

let handle_art_sites fa_creds conn msg =
    let handle_danbooru domain site_name target msg =
        let id = Danbooru.find_view_link domain msg in
        let post = Danbooru.get_post domain id in
        let response = Printf.sprintf "%s post %d: %s (%d point%s)"
            site_name
            id
            (String.concat ", " (truncate post.Danbooru.po_tags 15))
            post.Danbooru.po_score
            (plural post.Danbooru.po_score)
        in
        Irc.send conn (Irc.MS_privmsg(target, response))
    in
    match msg with
    | Irc.MS_privmsg(target, msg) when Fa.has_view_link msg ->
        let sub = Fa.get_submission fa_creds (Fa.find_view_link msg) in
        let rating =
            match sub.Fa.su_rating with
            | Fa.RA_general -> "safe"
            | Fa.RA_mature -> "questionable"
            | Fa.RA_explicit -> "explicit"
        in
        let response = Printf.sprintf
            "FA submission %d: %s by %s (%s, %d view%s, %d fave%s)"
            sub.Fa.su_id
            sub.Fa.su_title
            sub.Fa.su_artist
            rating
            sub.Fa.su_views
            (plural sub.Fa.su_views)
            sub.Fa.su_faves
            (plural sub.Fa.su_faves) in
        Irc.send conn (Irc.MS_privmsg(target, response));
        true
    | Irc.MS_privmsg(target, msg) when Fa.has_journal_link msg ->
        let journal = Fa.get_journal (Fa.find_journal_link msg) in
        let response = Printf.sprintf
            "FA journal %d: %s by %s (%d comment%s)"
            journal.Fa.jo_id
            journal.Fa.jo_title
            journal.Fa.jo_author
            journal.Fa.jo_comments
            (plural journal.Fa.jo_comments) in
        Irc.send conn (Irc.MS_privmsg(target, response));
        true
    | Irc.MS_privmsg(target, msg) when Pixiv.has_view_link msg ->
        let illust = Pixiv.get_illust (Pixiv.find_view_link msg) in
        let response = Printf.sprintf
            "Pixiv illust. %d: %s (%s) by %s (%s)"
            illust.Pixiv.il_id
            (fst illust.Pixiv.il_title)
            (snd illust.Pixiv.il_title)
            (fst illust.Pixiv.il_artist)
            (snd illust.Pixiv.il_artist) in
        Irc.send conn (Irc.MS_privmsg(target, response));
        true
    | Irc.MS_privmsg(target, msg)
            when Danbooru.has_view_link "wildcritters.ws" msg ->
        handle_danbooru "wildcritters.ws" "WildCritters" target msg; true
    | _ -> false

let handle_youtube conn msg =
    match msg with
    | Irc.MS_privmsg(target, msg) when Youtube.has_view_link msg ->
        let video = Youtube.get_video (Youtube.find_view_link msg) in
        let response = Printf.sprintf
            "YouTube video %s: %s by %s (%d view%s, %d favorite%s)"
            video.Youtube.vi_id
            video.Youtube.vi_title
            video.Youtube.vi_author
            video.Youtube.vi_views
            (plural video.Youtube.vi_views)
            video.Youtube.vi_favorites
            (plural video.Youtube.vi_favorites) in
        Irc.send conn (Irc.MS_privmsg(target, response));
        true
    | _ -> false

let run fa_info conn_info =
    let sock = Irc.connect conn_info in
    try
        let handlers = [
            Irc.Bot.base_handler;
            handle_art_sites fa_info;
            handle_youtube
        ] in
        Std.finally (fun() -> Irc.close sock) (Irc.Bot.run sock) handlers
    with
    | Irc.Disconnected -> exit 1
    | Irc.Quit -> ()

let main() =
    let optparser = OptParse.OptParser.make ~usage:"%prog config.ini" () in
    if Array.length Sys.argv < 2 then begin
        OptParse.OptParser.usage optparser ();
        exit 1
    end;

    let config = new Inifiles.inifile Sys.argv.(1) in

    let cred_file = config#getval "FA" "credentialfile" in
    let cred_file = open_in_bin cred_file in
    let (fa_creds:Fa.credentials) = Std.finally (fun() -> close_in cred_file)
        Marshal.from_channel cred_file in

    let conn_info = {
        Irc.ci_serverhost = config#getval "IRC" "serverhost";
        Irc.ci_serverport = int_of_string (config#getval "IRC" "serverport");
        Irc.ci_nick = config#getval "IRC" "nick";
        Irc.ci_username = config#getval "IRC" "username";
        Irc.ci_realname = config#getval "IRC" "realname";
        Irc.ci_autojoin =
            ExtString.String.nsplit (config#getval "IRC" "autojoin") " "
    } in

    run fa_creds conn_info
;;

main()

