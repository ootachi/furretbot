(* furretbot/query.ml *)

type axis =
| AX_self
| AX_descendant_or_self
| AX_children

let rec iter_over_axis f axis nodes =
    Enum.iter begin fun node ->
        begin
            match axis with
            | AX_self | AX_descendant_or_self -> f node
            | AX_children -> ()
        end;
        match axis, node with
        | AX_descendant_or_self, Nethtml.Element (_, _, children) ->
            iter_over_axis f AX_descendant_or_self
                (ExtList.List.enum children)
        | AX_children, Nethtml.Element (_, _, children) ->
            iter_over_axis f AX_self (ExtList.List.enum children)
        | AX_self, _ | _, Nethtml.Data _ -> ()
    end nodes

let tag ?axis:(axis=AX_descendant_or_self) tagname nodes =
    let result = DynArray.create() in
    iter_over_axis begin fun node ->
        match node with
        | Nethtml.Element (tagname', _, _) when tagname = tagname' ->
            DynArray.add result node
        | _ -> ()
    end axis nodes;
    DynArray.enum result

let attr_predicate ?axis:(axis=AX_descendant_or_self) attr pred nodes =
    let result = DynArray.create() in
    iter_over_axis begin fun node ->
        match node with
        | Nethtml.Element (_, attrs, _) -> begin
                try
                    let value = List.assoc attr attrs in
                    if pred value then DynArray.add result node
                with
                    Not_found -> ()
            end
        | _ -> ()
    end axis nodes;
    DynArray.enum result

let klass ?axis:(axis=AX_descendant_or_self) classname nodes =
    let has_class classes =
        List.exists ((=) classname) (ExtString.String.nsplit classes " ")
    in
    attr_predicate ~axis:axis "class" has_class nodes

let attr ?axis:(axis=AX_descendant_or_self) attr value nodes =
    attr_predicate ~axis:axis attr ((=) value) nodes

let id ?axis:(axis=AX_descendant_or_self) idname nodes =
    attr ~axis:axis "id" idname nodes

let text ?axis:(axis=AX_descendant_or_self) nodes =
    let result = Buffer.create 0 in
    iter_over_axis begin fun node ->
        match node with
        | Nethtml.Data str -> Buffer.add_string result str
        | _ -> ()
    end axis nodes;
    Buffer.contents result

let rec get ?index:(i=0) nodes =
    if i == 0 then
        Option.get (Enum.get nodes)
    else begin
        Enum.junk nodes;
        get ~index:(i - 1) nodes
    end

let of_node node = ExtArray.Array.enum [| node |]

