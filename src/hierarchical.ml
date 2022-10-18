open! Core

module Make(Key : Hashable.S_plain) = struct
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
        Hashtbl.add_exn t.sub_layers ~key:this ~data:layer;
        Queue.enqueue t.items (Layer { name = this; contents = layer });
        layer_at_path layer rest

  let insert t path data =
    let layer = layer_at_path t path in
    Queue.enqueue layer.items (Base data)

  let rec materialize
      (t : _ Layer.t)
      ~path_rev
      ~map_element
      ~map_layer
    =
    Queue.fold t.items ~init:[]
      ~f:(fun acc item ->
          match item with
          | Base element ->
            (map_element ~path_rev element) :: acc
          | Layer { name; contents } ->
            let path_rev = name :: path_rev in
            let result = materialize ~path_rev ~map_element ~map_layer contents in
            (map_layer ~path_rev result) :: acc
        )
    |> List.rev
    

end

