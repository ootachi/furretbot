(* furretbot/pixiv.ml *)

type illust = {
    il_id: int;
    il_artist: string * string;
    il_title: string * string;
}

let referrer = "mimiga.net"

let view_link_re =
    lazy (Str.regexp
        ".*pixiv\\.net/member_illust\\.php\\?.*illust_id=\\([0-9]+\\)")
let qstring_re = lazy (Str.regexp " *\"\\([^\"]*\\)\"")

let has_view_link str = Str.string_match(Lazy.force view_link_re) str 0

let find_view_link str =
    ignore(Str.string_match (Lazy.force view_link_re) str 0);
    int_of_string(Str.matched_group 1 str)

let api_url_of_illust id =
    Printf.sprintf "http://iphone.pxv.jp/iphone/illust.php?illust_id=%d" id

let quoted_strings str =
    let result = DynArray.create() in
    let rec recur idx =
        if idx == String.length str then
            ()
        else if Str.string_match (Lazy.force qstring_re) str idx then
            DynArray.add result (Str.matched_group 1 str)
        else
            ()
    in
    recur 0;
    DynArray.to_list result

let get_illust id =
    let body = Http_client.Convenience.http_get (api_url_of_illust id) in
    let fields = ExtString.String.nsplit body "," in
    let data = List.map quoted_strings fields in

    let title_ja = List.hd(List.nth data 3) in
    let artist_ja = List.hd(List.nth data 5) in

    let translate str =
        Googlelang.translate
            ~from_lang:"ja"
            ~to_lang:"en"
            ~referrer:referrer 
            str
    in
    let title_en, artist_en = translate title_ja, translate artist_ja in

    {
        il_id = id;
        il_artist = artist_ja, artist_en;
        il_title = title_ja, title_en
    }

