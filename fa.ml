(* furretbot/fa.ml *)

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
    lazy (Str.regexp ".*furaffinity\\.net/\\(view\\|full\\)/\\([0-9]+\\)")
let has_view_link str = Str.string_match (Lazy.force view_link_re) str 0
let find_view_link str =
    ignore (Str.string_match (Lazy.force view_link_re) str 0);
    int_of_string (Str.matched_group 2 str)

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

