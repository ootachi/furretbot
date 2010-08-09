(* furretbot/youtube.ml *)

let view_link_re = lazy (Str.regexp ".*youtube\\.com/watch\\?\\([^ \t\013\n]+\\)")

type video_info = {
    vi_id: string;
    vi_title: string;
    vi_author: string;
    vi_favorites: int;
    vi_views: int
}

let find_view_link str =
    if not(Str.string_match (Lazy.force view_link_re) str 0) then
        raise Not_found;
    let query = Str.matched_group 1 str in
    let params = Netencoding.Url.dest_url_encoded_parameters query in
    List.assoc "v" params

let has_view_link str =
    try ignore (find_view_link str); true
    with Not_found -> false

let get_video id =
    let info_url = "http://gdata.youtube.com/feeds/api/videos/" ^ id in
    let body = Http_client.Convenience.http_get info_url in
    let xml = Xml.parse_string body in

    let title, author = ref None, ref None in
    let faves, views = ref None, ref None in

    begin
        match xml with
        | Xml.Element("entry", _, children) ->
            List.iter begin fun child ->
                match child with
                | Xml.Element("title", _, content::_) ->
                    title := Some (Xml.pcdata content)
                | Xml.Element("author", _, children) ->
                    List.iter begin fun child ->
                        match child with
                        | Xml.Element("name", _, content::_) ->
                            author := Some (Xml.pcdata content)
                        | _ -> ()
                    end children
                | Xml.Element("yt:statistics", attrs, _) ->
                    let faves_str = List.assoc "favoriteCount" attrs in
                    let views_str = List.assoc "viewCount" attrs in
                    faves := Some (int_of_string faves_str);
                    views := Some (int_of_string views_str)
                | _ -> ()
            end children
        | _ -> failwith "Unexpected YouTube response"
    end;

    {
        vi_id = id;
        vi_title = Option.get (!title);
        vi_author = Option.get (!author);
        vi_favorites = Option.get (!faves);
        vi_views = Option.get (!views)
    }

