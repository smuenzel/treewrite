open! Core
open! Util

module Hierarchical(Key : Hashable.S_plain) = struct
  module Path = struct
    type t = Key.t list
  end

  module rec Item : sig
    type 'a t =
      | Layer of { name : Key.t; contents : 'a Layer.t }
      | Base of 'a
  end = Item
  and Layer : sig
    type 'a t =
      { sub_layers : 'a Layer.t Key.Table.t
      ; items : 'a Item.t Queue.t
      }
  end = Layer

  let create () : _ Layer.t =
    { sub_layers = Key.Table.create ()
    ; items = Queue.create ()
    }

  let rec layer_at_path (t : _ Layer.t) path =
    match path with
    | [] -> t
    | this :: rest ->
      match Hashtbl.find t.sub_layers this with
      | Some layer -> layer_at_path layer rest
      | None ->
        let layer = create () in
        Queue.enqueue t.items (Layer { name = this; contents = layer });
        layer_at_path layer rest

  let insert t path data =
    let layer = layer_at_path t path in
    Queue.enqueue layer.items (Base data)

end

module External_type_id = Unique_id.Int()
module Defined_type_id = Unique_id.Int()

module Type_id = struct
  type t =
    | External of External_type_id.t
    | Defined of Defined_type_id.t
  [@@deriving sexp]
end

module External_type_descriptor = struct
  type t =
    { id : External_type_id.t
    ; parameter_count : int
    ; fully_qualified_name : Longident.t
    } [@@deriving sexp]

  let to_fun_name t =
    match Longident.flatten_exn t.fully_qualified_name with
    | [] -> assert false
    | single_component :: [] -> String.lowercase single_component
    | multi ->
      let useful_name_components_rev =
        match List.rev multi with
        | "t" :: rest -> rest
        | multi -> multi
      in
      String.concat
        ~sep:"__" (List.rev_map ~f:String.lowercase useful_name_components_rev)
end

module Module_name = struct
  include String_id.Make(struct let module_name = "Module_name" end)()
end

module Defined_type_name = struct
  module T = struct
    type t = Module_name.t Nonempty_list.t [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)
end

module Type_instance = struct
  type t =
    { id : Type_id.t
    ; parameters : t list (* Parameters must be instantiated for now *)
    } [@@deriving sexp]
end

module Record_label = struct
  include String_id.Make(struct let module_name = "Record_label" end)()
end

module Constructor_name = struct
  include String_id.Make(struct let module_name = "Constructor_name" end)()
end

module Type_shape = struct
  type t =
    | Record of (Record_label.t * Type_instance.t) list
    | Variant of (Constructor_name.t * Type_instance.t) list
  [@@deriving sexp]
end
  
module Defined_type_description = struct
  type t =
    { id : Defined_type_id.t
    ; name : Defined_type_name.t
    ; shape : Type_shape.t
    } [@@deriving sexp]
end

module Language_name = struct
  include String_id.Make(struct let module_name = "Language_name" end)()
end

module Language_definition = struct
  type t =
    { name : Language_name.t
    ; types_by_name : Defined_type_id.t Defined_type_name.Map.t
    ; types_by_id : Type_shape.t Defined_type_id.Map.t
    } [@@deriving sexp]

  let all_constructors
end
