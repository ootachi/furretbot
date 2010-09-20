(* furretbot/deviantart.ml *)

class t = object(self)
    method schemes = [ "http://*.deviantart.com/art/*" ]
    method describe url =
        let resp = Oembed.get url in
        Printf.sprintf "deviantART deviation: %s"
            (Option.get resp.Oembed.re_title)
end

