(* furretbot/fa.ml *)

type credentials = {
    cr_a: string;
    cr_b: string
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

type journal = {
    jo_id: int;
    jo_url: string;
    jo_title: string;
    jo_author: string;
    jo_comments: int
}

let user_agent = "Mozilla/4.0 (compatible; U; MSIE 6.0; Windows NT 5.1)"

let login username password =
    let credential_a, credential_b = Global.empty "", Global.empty "" in

    let req = Curl.init() in
    Std.finally (fun() -> Curl.cleanup req) begin fun() ->
        Curl.set_url req "https://www.furaffinity.net/login/?url=/";
        Curl.set_post req true;
        Curl.set_verbose req true;
        Curl.set_useragent req user_agent;
        Curl.set_referer req "https://www.furaffinity.net/login/";

        let find_cookie line =
            let line_lc = String.lowercase line in
            if ExtString.String.starts_with line_lc "set-cookie:" then begin
                let _, value = ExtString.String.split line ":" in
                let value = ExtString.String.strip value in
                let key, value = ExtString.String.split value "=" in
                let set_credential credential =
                    let value, _ = ExtString.String.split value ";" in
                    Global.set credential value
                in
                match key with
                | "a" -> set_credential credential_a
                | "b" -> set_credential credential_b
                | _ -> ()
            end;
            String.length line
        in
        Curl.set_headerfunction req find_cookie;

        let post =
            Netencoding.Url.mk_url_encoded_parameters [
                ("action", "login");
                ("retard_protection", "1");
                ("name", username);
                ("pass", password);
                ("login", "Login to&nbsp;FurAffinity")
            ]
        in
        Curl.set_postfields req post;
        Curl.perform req
    end ();
    { cr_a = Global.get credential_a; cr_b = Global.get credential_b }

let view_link_re =
    lazy (Str.regexp ".*furaffinity\\.net/\\(view\\|full\\)/\\([0-9]+\\)")
let journal_link_re =
    lazy (Str.regexp ".*furaffinity\\.net/journal/\\([0-9]+\\)")

let has_view_link str = Str.string_match (Lazy.force view_link_re) str 0
let has_journal_link str = Str.string_match (Lazy.force journal_link_re) str 0

let find_view_link str =
    ignore(Str.string_match (Lazy.force view_link_re) str 0);
    int_of_string(Str.matched_group 2 str)
let find_journal_link str =
    ignore(Str.string_match (Lazy.force journal_link_re) str 0);
    int_of_string(Str.matched_group 1 str)

let url_of_submission id =
    Printf.sprintf "http://www.furaffinity.net/view/%d/" id
let url_of_journal id =
    Printf.sprintf "http://www.furaffinity.net/journal/%d/" id

let get_submission cred id =
    let url = url_of_submission id in

    let call = new Http_client.get url in
    let headers = call#request_header `Base in
    headers#update_field "User-agent" user_agent;
    let cookies = Printf.sprintf "a=%s; b=%s" cred.cr_a cred.cr_b in
    headers#update_field "Cookie" cookies;

    let pipeline = new Http_client.pipeline in
    pipeline#add call;
    pipeline#run();

    assert(call#status = `Successful);

    let body = call#response_body#value in
    let html = Nethtml.parse (new Netchannels.input_string body) in
    let html_q() = ExtList.List.enum html in

    let html_title = Query.tag "title" (html_q()) in
    let html_title = Nethtml.decode [ (Query.get ~index:0 html_title) ] in
    let html_title = Query.text (Query.of_node (List.hd html_title)) in

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

let get_journal id =
    let url = url_of_journal id in

    let body = Http_client.Convenience.http_get url in
    let html = Nethtml.parse (new Netchannels.input_string body) in
    let html = Nethtml.decode html in
    let html_q() = ExtList.List.enum html in

    let html_title = Query.text(Query.tag "title" (html_q())) in
    let title_re = lazy(Str.regexp
        "\\(.*\\) -- \\([^']+\\)'s Journal -- Fur Affinity \\[dot\\] net") in
    assert(Str.string_match (Lazy.force title_re) html_title 0);
    let title = Str.matched_group 1 html_title in
    let author = Str.matched_group 2 html_title in

    let cats = Query.klass "cat" (html_q()) in
    let comment_count_box = Query.get ~index:1 cats in
    
    let num_re = lazy(Str.regexp "[0-9]+") in
    let comment_count_str = Query.text(Query.of_node comment_count_box) in
    ignore(Str.search_forward (Lazy.force num_re) comment_count_str 0);
    let comment_count = int_of_string(Str.matched_string comment_count_str) in

    {
        jo_id = id;
        jo_url = url;
        jo_title = title;
        jo_author = author;
        jo_comments = comment_count
    }

