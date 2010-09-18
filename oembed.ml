(* furretbot/oembed.ml *)

module B = Json_type.Browse
module Bu = Json_type.Build

exception Malformed
exception No_provider

type url = string
type html = string

type dimensions = {
    di_width: int;
    di_height: int;
}

type resource_type =
| RT_photo of url * dimensions
| RT_video of html * dimensions
| RT_rich of html * dimensions
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

type provider = {
    pr_schemes: string list;
    pr_endpoint: string;
}

let backslash_star = lazy(Str.regexp_string "\\*")

let embedly = [
    { pr_schemes = [ "*" ]; pr_endpoint = "http://api.embed.ly/v1/api/oembed" }
]

(**
 * Returns the provider for the given URL. Raises [No_provider] if none of the
 * URLs matched the provider.
 *)
let provider_for_url ?providers:(providers=embedly) url =
    try
        List.find begin fun provider ->
            List.exists begin fun scheme ->
                let re = Str.global_replace (Lazy.force backslash_star) ".*"
                    (Str.quote scheme) in
                Str.string_match (Str.regexp re) url 0
            end provider.pr_schemes
        end providers
    with Not_found -> raise No_provider

(**
 * Performs an oEmbed request (using [Http_client.Convenience.http_get]) to
 * retrieve the embedded content for [url] and returns a [response] record.
 *)
let get ?providers:(providers=embedly) ?max_width:max_width
        ?max_height:max_height url =
    let provider = provider_for_url ~providers:providers url in

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
        (provider.pr_endpoint ^ "?" ^ query_string) in

    let json = Json_io.json_of_string body in
    try
        let root = B.make_table (B.objekt json) in
        let get_dimensions() =
            {
                di_width = B.int (B.field root "width");
                di_height = B.int (B.field root "height")
            }
        in
        let resource_type =
            match B.string (B.field root "type") with
            | "photo" ->
                RT_photo(B.string (B.field root "url"), get_dimensions())
            | "video" ->
                RT_video(B.string (B.field root "html"), get_dimensions())
            | "link" -> RT_link
            | "rich" ->
                RT_rich(B.string (B.field root "html"), get_dimensions())
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

(** Returns the given oEmbed response as a JSON string. *)
let put response =
    let result = DynArray.create() in
    let add_dimensions dimensions =
        DynArray.add result ("width", Bu.int dimensions.di_width);
        DynArray.add result ("height", Bu.int dimensions.di_height)
    in
    let ty =
        match response.re_type with
        | RT_photo(url, dims) ->
            add_dimensions dims;
            DynArray.add result ("url", (Bu.string url));
            "photo"
        | RT_video(html, dims) ->
            add_dimensions dims;
            DynArray.add result ("html", (Bu.string html));
            "video"
        | RT_rich(html, dims) ->
            add_dimensions dims;
            DynArray.add result ("html", (Bu.string html));
            "rich"
        | RT_link -> "link" in
    DynArray.add result ("type", (Bu.string ty));
   
    let add_string name =
        Option.may (fun s -> DynArray.add result (name, (Bu.string s)))
    in
    let add_int name =
        Option.may (fun n -> DynArray.add result (name, (Bu.int n)))
    in

    add_string "title" response.re_title;
    add_string "author_name" response.re_author_name;
    add_string "author_url" response.re_author_url;
    add_string "provider_name" response.re_provider_name;
    add_string "provider_url" response.re_provider_url;
    add_int "cache_age" response.re_cache_age;
    add_string "thumbnail_url" response.re_thumbnail_url;
    add_int "thumbnail_width" response.re_thumbnail_width;
    add_int "thumbnail_height" response.re_thumbnail_height;
    DynArray.add result ("description", (Bu.string response.re_description));

    Json_io.string_of_json (Bu.objekt (DynArray.to_list result))

