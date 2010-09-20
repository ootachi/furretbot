(* furretbot/furretbot.ml *)

let shuffle arr =
    let len = Array.length arr in
    let swap i j =
        let tmp = arr.(i) in
        arr.(i) <- arr.(j);
        arr.(j) <- tmp
    in
    let rand_between lo hi = lo + Random.int (hi - lo) in
    for i = 0 to len - 2 do
        swap i (rand_between (i + 1) len)
    done

let plural n = if n == 1 then "" else "s"

(* TODO: multiple links in one message *)
let url_re = lazy(Str.regexp "http://[ \t\n]+")
let get_url str =
    try
        ignore(Str.search_forward (Lazy.force url_re) str 0);
        Some(Str.matched_string str)
    with Not_found -> None

let handle_art_sites fa_creds da conn msg =
    let handle_danbooru domain site_name target msg =
        let id = Danbooru.find_view_link domain msg in
        let post = Danbooru.get_post domain id in
        let tags = Array.of_list post.Danbooru.po_tags in
        let tags, suffix =
            if Array.length tags > 15 then
                (Array.sub tags 0 15), ", ..."
            else
                tags, ""
        in
        shuffle tags;
        Array.fast_sort String.compare tags;
        let response = Printf.sprintf "%s post %d: %s%s (%d point%s)"
            site_name
            id
            (String.concat ", " (Array.to_list tags))
            suffix
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
    | Irc.MS_privmsg(target, msg) ->
        begin
            match get_url msg with
            | Some url when Oembed.schemes_match da#schemes url ->
                Irc.send conn (Irc.MS_privmsg(target, da#describe url));
                true
            | _ -> false
        end
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
    let da = new Deviantart.t in
    let sock = Irc.connect conn_info in
    try
        let handlers = [
            Irc.Bot.base_handler;
            handle_art_sites fa_info da;
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
        Irc.ci_password =
            begin
                try
                    Some (config#getval "IRC" "password")
                with
                    Inifiles.Invalid_element _ -> None
            end;
        Irc.ci_nick = config#getval "IRC" "nick";
        Irc.ci_username = config#getval "IRC" "username";
        Irc.ci_realname = config#getval "IRC" "realname";
        Irc.ci_autojoin =
            ExtString.String.nsplit (config#getval "IRC" "autojoin") " "
    } in

    run fa_creds conn_info
;;

main()

