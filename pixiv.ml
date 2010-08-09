(* furretbot/pixiv.ml *)

type t = {
    pi_cookiestore: string;
    pi_username: string;
    pi_password: string
}

type illust = {
    il_id: int;
    il_artist_ja: string;
    il_artist_en: string;
    il_title_ja: string;
    il_title_en: string
}

let with_curl_request pixiv f =
    let req = Curl.init() in
    Std.finally (fun() -> Curl.cleanup req) begin fun() ->
        Curl.set_cookiefile req pixiv.pi_cookiestore;
        Curl.set_cookiejar req pixiv.pi_cookiestore;
        Curl.set_verbose req true;
        f req
    end ()

let login pixiv =
    with_curl_request pixiv begin fun req ->
        Curl.set_url req "http://www.pixiv.net/index.php";
        Curl.set_post req true;
        Curl.set_referer req "http://www.pixiv.net/index.php";
        let post =
            Netencoding.Url.mk_url_encoded_parameters [
                ("mode", "login");
                ("pixiv_id", pixiv.pi_username);
                ("pass", pixiv.pi_password)
            ]
        in
        Curl.set_postfields req post;
        Curl.perform req
    end

let login_if_necessary pixiv =
    if not (Sys.file_exists pixiv.pi_cookiestore) then login pixiv

let view_link_re =
    lazy (Str.regexp
        ".*pixiv\\.net/member_illust\\.php\\?.*illust_id=\\([0-9]+\\)")

let has_view_link str = Str.string_match(Lazy.force view_link_re) str 0

let find_view_link str =
    ignore(Str.string_match (Lazy.force view_link_re) str 0);
    int_of_string(Str.matched_group 1 str)

let url_of_illust id =
    Printf.sprintf
        "http://www.pixiv.net/member_illust.php?mode=medium&illust_id=%d" id

let get_illust pixiv id =
    login_if_necessary pixiv;

    let url = url_of_illust id in

    let buf = Buffer.create 0 in
    let write_to_buf s = Buffer.add_string buf s; String.length s in

    with_curl_request pixiv begin fun req ->
        Curl.set_url req url;
        Curl.set_writefunction req write_to_buf;
        Curl.perform req
    end;

    let body = Buffer.contents buf in
    let html = Nethtml.parse (new Netchannels.input_string body) in
    let html_q() = ExtList.List.enum html in

    print_endline body;

    let artist_ja = Query.text (Query.klass "avatar_m" (html_q())) in
    let title_ja = Query.text (Query.tag "h3" (html_q())) in

    let artist_en =
        Googlelang.translate
            ~from_lang:"ja"
            ~to_lang:"en"
            ~referrer:"mimiga.net"
            artist_ja in
    let title_en =
        Googlelang.translate
            ~from_lang:"ja"
            ~to_lang:"en"
            ~referrer:"mimiga.net"
            title_ja in

    {
        il_id = id;
        il_artist_ja = artist_ja;
        il_artist_en = artist_en;
        il_title_ja = title_ja;
        il_title_en = title_en
    }

