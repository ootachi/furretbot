(* pixiv.ml *)

let buffer_writefunction buf s = Buffer.add_string buf s; String.length s

module Googlelang = struct
    let translate
            ~from_lang:from_lang
            ~to_lang:to_lang
            ~referrer:referrer
            text =
        let buf = Buffer.create 0 in
        let req = Curl.init() in
        Std.finally (fun() -> Curl.cleanup req) begin fun() ->
            Curl.set_referer req referrer;
            Curl.set_verbose req true;
            Curl.set_url req
                (Printf.sprintf
                    "http://ajax.googleapis.com/ajax/services/language/\
                        translate?v=1.0&q=%s&langpair=%s%%7C%s"
                    (Curl.escape text)
                    (Curl.escape from_lang)
                    (Curl.escape to_lang));
            Curl.set_writefunction req (buffer_writefunction buf);
            Curl.perform req
        end();
        let body = Json_io.json_of_string(Buffer.contents buf) in
        match body with
        | Json_type.Object obj ->
            begin
                match List.assoc "responseData" obj with
                | Json_type.Object obj ->
                    begin
                        match List.assoc "translatedText" obj with
                        | Json_type.String str -> str
                        | _ -> "Translated text is not a string"
                    end
                | _ -> failwith "Response data is not an object"
            end
        | _ -> failwith "Response is not an object"
end

type illust = {
    il_artist_ja: string;
    il_artist_en: string;
    il_title_ja: string;
    il_title_en: string
}

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

let get_illust id =
    let url = url_of_illust id in

    let buf = Buffer.create 0 in
    let req = Curl.init() in
    Std.finally (fun() -> Curl.cleanup req) begin fun() ->
        Curl.set_verbose req true;
        Curl.set_url req url;
        Curl.setopt req (Curl.CURLOPT_USERAGENT
            "Mozilla/4.0 (compatible; U; MSIE 6.0; Windows NT 5.1)");
        Curl.set_writefunction req (buffer_writefunction buf);
        Curl.perform req
    end();

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
        il_artist_ja = artist_ja;
        il_artist_en = artist_en;
        il_title_ja = title_ja;
        il_title_en = title_en
    }

