(* furretbot/embedly.ml *)

module B = Json_type.Browse

exception Malformed

type url = URL of string
type html = HTML of string

type 'a type_info = {
    ti_src: 'a;
    ti_width: int;
    ti_height: int;
}

type resource_type =
| RT_photo of url type_info
| RT_video of html type_info
| RT_rich of html type_info
| RT_link

type response = {
    re_type: resource_type;
    re_title: string option;
    re_author_name: string option;
    re_author_url: string option;
    re_provider_name: string option;
    re_provider_url: string option;
    re_cache_age: int option;
    re_thumbnail_url: string option;
    re_thumbnail_width: int option;
    re_thumbnail_height: int option;
    re_description: string;
}

let url(URL str) = str
let html(HTML str) = str

let get ?max_width:max_width ?max_height:max_height url =
    let params = DynArray.create() in
    DynArray.add params ("url", url);
    DynArray.add params ("format", "json");
    Option.may (fun n -> DynArray.add params ("maxwidth", (string_of_int n)))
        max_width;
    Option.may (fun n -> DynArray.add params ("maxheight", (string_of_int n)))
        max_height;
    let query_string = Netencoding.Url.mk_url_encoded_parameters
        (DynArray.to_list params) in
    let body = Http_client.Convenience.http_get
        ("http://api.embed.ly/v1/api/oembed?" ^ query_string) in

    let json = Json_io.json_of_string body in
    try
        let root = B.make_table (B.objekt json) in
        let resource_type =
            match B.string (B.field root "type") with
            | "photo" ->
                RT_photo {
                    ti_src = URL(B.string (B.field root "url"));
                    ti_width = B.int (B.field root "width");
                    ti_height = B.int (B.field root "height")
                }
            | "video" ->
                RT_video {
                    ti_src = HTML(B.string (B.field root "html"));
                    ti_width = B.int (B.field root "width");
                    ti_height = B.int (B.field root "height")
                }
            | "link" -> RT_link
            | "rich" ->
                RT_rich {
                    ti_src = HTML(B.string (B.field root "html"));
                    ti_width = B.int (B.field root "width");
                    ti_height = B.int (B.field root "height")
                }
            | _ -> raise Malformed
        in
        {
            re_type = resource_type;
            re_title = B.optional B.string (B.fieldx root "title");
            re_author_name = B.optional B.string (B.fieldx root "author_name");
            re_author_url = B.optional B.string (B.fieldx root "author_url");
            re_provider_name =
                B.optional B.string (B.fieldx root "provider_name");
            re_provider_url =
                B.optional B.string (B.fieldx root "provider_url");
            re_cache_age = B.optional B.int (B.fieldx root "cache_age");
            re_thumbnail_url =
                B.optional B.string (B.fieldx root "thumbnail_url");
            re_thumbnail_width =
                B.optional B.int (B.fieldx root "thumbnail_width");
            re_thumbnail_height =
                B.optional B.int (B.fieldx root "thumbnail_height");
            re_description = B.string (B.field root "description")
        }
    with Json_type.Json_error _ | Not_found -> raise Malformed

