(* furretbot/danbooru.ml *)

type post = {
    po_id: int;
    po_score: int;
    po_tags: string list;
}

let view_link_re domain =
    Str.regexp (".*" ^ (Str.quote domain) ^ "/post/show/\\([0-9]+\\)")

let has_view_link domain str = Str.string_match(view_link_re domain) str 0

let find_view_link domain str =
    ignore(Str.string_match (view_link_re domain) str 0);
    int_of_string(Str.matched_group 1 str)

let api_url_of_post domain id =
    Printf.sprintf "http://%s/post/index.json?tags=id:%d" domain id

let get_post domain id =
    let body = Http_client.Convenience.http_get (api_url_of_post domain id) in
    let data = Json_io.json_of_string ~big_int_mode:true body in
    let result = Json_type.Browse.array data in
    let fields = Json_type.Browse.objekt (List.hd result) in
    let tag_field = Json_type.Browse.string (List.assoc "tags" fields) in
    {
        po_id = id;
        po_score = Json_type.Browse.int (List.assoc "score" fields);
        po_tags = ExtString.String.nsplit tag_field " "
    }

