(* furretbot/googlelang.ml *)

let buffer_writefunction buf s = Buffer.add_string buf s; String.length s

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

