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

module External_type_id = Unique_id.Int()
module Defined_type_id = Unique_id.Int()

module Type_id = struct
  type t =
    | External of External_type_id.t
    | Defined of Defined_type_id.t
  [@@deriving sexp]

  let all_external_types = function
    | External id -> External_type_id.Set.singleton id
    | Defined _ -> External_type_id.Set.empty
end

module External_type_descriptor = struct
  module T = struct
    type t =
      { parameter_count : int
      ; fully_qualified_name : Longident.t
      } [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make_plain(T)

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

  let to_ident t = t.fully_qualified_name
end

module Module_name = struct
  include String_id.Make(struct let module_name = "Module_name" end)()

  let of_string s = of_string (String.capitalize s)

  let constructor_module = of_string "Constructors"
end

module Language_name = struct
  include String_id.Make(struct let module_name = "Language_name" end)()

  let to_variable_name t = String.lowercase (to_string t)
  let to_module_name t = Module_name.of_string (String.capitalize (to_string t))
end

module Defined_type_name = struct
  module T = struct
    type t = Module_name.t Nonempty_list.t [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)

  let of_module_path p = Nonempty_list.of_list_exn p

  let ident_in_language t ~language_name =
    let module_path_rev =
      Nonempty_list.to_list t
      |> List.rev_map ~f:(Module_name.to_string)
    in
    "t"
    ::
    (Language_name.to_module_name language_name
     |> Module_name.to_string)
    :: module_path_rev
    |> Longident.of_list_rev

  let to_module_path t = Nonempty_list.to_list t
end

module Type_instance = struct
  type t =
    { id : Type_id.t
    ; parameters : t list (* Parameters must be instantiated for now *)
    } [@@deriving sexp]

  let all_external_types t = Type_id.all_external_types t.id
end

module Record_label = struct
  include String_id.Make(struct let module_name = "Record_label" end)()
end

module Constructor_name = struct
  include String_id.Make(struct let module_name = "Constructor_name" end)()

  let variant_type t =
    let open Ast_builder in
    ptyp_variant
      [ rtag (to_string t) false []
      ]
      Closed
      None
end

module Type_shape = struct
  type t =
    | Record of (Record_label.t * Type_instance.t) list
    | Variant of (Constructor_name.t * Type_instance.t list) list
  [@@deriving sexp]

  let all_external_types t =
    match t with
    | Record r ->
      let gather =
        List.fold ~init:External_type_id.Set.empty
          ~f:(fun acc (_,instance) ->
              Set.union acc (Type_instance.all_external_types instance)
            )
      in
      gather r
    | Variant v ->
      let gather =
        List.fold ~init:External_type_id.Set.empty
          ~f:(fun acc (_,instances) ->
              List.fold ~init:acc instances
                ~f:(fun acc instance ->
                    Set.union acc (Type_instance.all_external_types instance)
                  )
            )
      in
      gather v
end
  
module Defined_type_description = struct
  type t =
    { id : Defined_type_id.t
    ; name : Defined_type_name.t
    ; language : Language_name.t
    ; shape : Type_shape.t
    } [@@deriving sexp]

  let to_ident t =
    Defined_type_name.ident_in_language t.name ~language_name:t.language
end

module All_types = struct
  type t =
    { external_types : External_type_descriptor.t External_type_id.Map.t
    ; defined_types : Defined_type_description.t Defined_type_id.Map.t
    } [@@deriving sexp]

  let rec to_type (t : t) { Type_instance. id; parameters } =
    let open Ast_builder in
    let parameters = List.map parameters ~f:(to_type t) in
    let name =
      match id with
      | External id ->
        Map.find_exn t.external_types id
        |> External_type_descriptor.to_ident
      | Defined id ->
        Map.find_exn t.defined_types id
        |> Defined_type_description.to_ident
    in
    ptyp_constr
      name
      parameters

end

module Language_definition = struct
  type t =
    { name : Language_name.t
    ; types_by_name : Defined_type_id.t Defined_type_name.Map.t
    ; types_by_id : Type_shape.t Defined_type_id.Map.t
    } [@@deriving sexp, fields]

  let all_external_types t : External_type_id.Set.t =
    Fields.Direct.fold t
      ~init:External_type_id.Set.empty
      ~name:(fun acc _ _ _ -> acc)
      ~types_by_name:(fun acc _ _ _ -> acc)
      ~types_by_id:(fun acc _ _ defined_types ->
          Map.fold ~init:acc defined_types
            ~f:(fun ~key:_ ~data:type_shape acc ->
                Set.union acc (Type_shape.all_external_types type_shape)
              )
        )

  let all_constructor_descriptions t =
    Map.filter_map t.types_by_name
      ~f:(fun id ->
          match Map.find_exn t.types_by_id id with
          | Record _ -> None
          | Variant constructors -> 
            Some (Constructor_name.Map.of_alist_exn constructors)
        )

  let all_defined_types t : Defined_type_description.t Defined_type_id.Map.t =
    Map.fold t.types_by_name ~init:Defined_type_id.Map.empty
      ~f:(fun ~key:name ~data:id acc ->
          let shape = Map.find_exn t.types_by_id id in
          let data =
            { Defined_type_description.
              id
            ; name
            ; language = t.name
            ; shape
            }
          in
          Map.add_exn acc ~key:id ~data
        )
end

module Transposer = struct

  let map2 (type ka kb v ca cb)
      ~empty
      (x : (ka, (kb,v,cb) Map.t, ca) Map.t) 
    : (kb, (ka,v,ca) Map.t, cb) Map.t
    =
    let comparator_outer = Map.comparator_s x in
    let comparator_inner = Map.comparator_s empty in
    Map.fold x
      ~init:(Map.empty comparator_inner)
      ~f:(fun ~key:key_outer ~data acc ->
          Map.fold data ~init:acc ~f:(fun ~key:key_inner ~data acc ->
              let new_map =
                match Map.find acc key_inner with
                | None -> Map.singleton comparator_outer key_outer data
                | Some map -> Map.add_exn map ~key:key_outer ~data
              in
              Map.set acc ~key:key_inner ~data:new_map
            )
        )

  type (_,_) sel3 =
    | S1 : (('a * _ * _), 'a) sel3
    | S2 : ((_ * 'a * _), 'a) sel3
    | S3 : ((_ * _ * 'a), 'a) sel3

  let sel3_fst
    (type a b c r x0 x1 x2 r')
    (selector : ((a * x0) * (b * x1) * (c * x2), (r * r')) sel3)
    ((a,b,c) : (a * b * c))
    : r
    =
    match selector with
    | S1 -> a
    | S2 -> b
    | S3 -> c

  let sel3_cmp
      (type a b c r a' b' c' r')
      (selector : ((a * a') * (b * b') * (c * c'), (r * r')) sel3)
      ((a,b,c) :
         ( (a, a') Core.Map.comparator
           * (b, b') Core.Map.comparator
           * (c, c') Core.Map.comparator
         ))
    : (r, r') Core.Map.comparator
    =
    match selector with
    | S1 -> a
    | S2 -> b
    | S3 -> c

  let tranpose3_gen
      (type k1 k1' k2 k2' k3 k3' v c1 c1' c2 c2' c3 c3')
      (selector :
         (
           ((k1 * c1) * (k2 * c2) * (k3 * c3), (k1' * c1')) sel3
           * ((k1 * c1) * (k2 * c2) * (k3 * c3), (k2' * c2')) sel3
           * ((k1 * c1) * (k2 * c2) * (k3 * c3), (k3' * c3')) sel3
         )
      )
      ~empty2
      ~empty3
      (x : (k1, (k2,(k3,v,c3) Map.t,c2) Map.t, c1) Map.t)
    : (k1', (k2',(k3',v,c3') Map.t,c2') Map.t, c1') Map.t
    =
    let s1, s2, s3 = selector in
    let comparator_i3 = Map.comparator_s empty3 in
    let comparator_i2 = Map.comparator_s empty2 in
    let comparator_i1 = Map.comparator_s x in
    let c = comparator_i1, comparator_i2, comparator_i3 in
    let comparator_o3 = sel3_cmp s3 c in
    let comparator_o2 = sel3_cmp s2 c in
    let comparator_o1 = sel3_cmp s1 c in
    Map.fold x
      ~init:(Map.empty comparator_o1)
      ~f:(fun ~key:key_i1 ~data acc ->
          Map.fold data ~init:acc ~f:(fun ~key:key_i2 ~data acc ->
              Map.fold data ~init:acc ~f:(fun ~key:key_i3 ~data acc ->
                  let keys = key_i1, key_i2, key_i3 in
                  let key_o1 = sel3_fst s1 keys in
                  let key_o2 = sel3_fst s2 keys in
                  let key_o3 = sel3_fst s3 keys in
                  let new_map =
                    match Map.find acc key_o1 with
                    | None ->
                      let data = Map.singleton comparator_o3 key_o3 data in
                      Map.singleton comparator_o2 key_o2 data
                    | Some map ->
                      let new_map =
                        match Map.find map key_o2 with
                        | None -> Map.singleton comparator_o3 key_o3 data
                        | Some map -> Map.add_exn map ~key:key_o3 ~data
                      in
                      Map.set map ~key:key_o2 ~data:new_map
                  in
                  Map.set acc ~key:key_o1 ~data:new_map
                )
            )
        )

  let transpose_312 ~empty2 ~empty3 x = tranpose3_gen (S3,S1,S2) ~empty2 ~empty3 x
  let transpose_231 ~empty2 ~empty3 x = tranpose3_gen (S2,S3,S1) ~empty2 ~empty3 x

end

module Hierarchical_module = Hierarchical(Module_name)

module Language_group = struct
  type t =
    { languages : Language_definition.t Language_name.Map.t
    ; external_types : External_type_descriptor.t External_type_id.Map.t
    }

  let all_types (_ : t) =
    assert false

  let maybe_tuple ctl =
    match ctl with
    | [ x ] -> x
    | xs -> Ast_builder.ptyp_tuple xs

  let all_constructor_descriptions t 
    : Type_instance.t list Constructor_name.Map.t Defined_type_name.Map.t Language_name.Map.t =
    Map.map t.languages ~f:Language_definition.all_constructor_descriptions

  let constructor_membership_object
      all_types
      (constructor : Type_instance.t list Language_name.Map.t)
    =
    let open Ast_builder in
    let members =
      Map.fold ~init:[] constructor
        ~f:(fun ~key:language_name ~data:type_instances acc ->
            otag
              (Language_name.to_variable_name language_name)
              (maybe_tuple (List.map ~f:(All_types.to_type all_types) type_instances))
            :: acc
          )
    in
    ptyp_object
      members
      Closed

  let constructor_type
      all_types
      (constructors : Type_instance.t list Language_name.Map.t Constructor_name.Map.t)
    =
    let open Ast_builder in
    let params =
      [ ptyp_any, (Asttypes.NoVariance,Asttypes.NoInjectivity)
      ; ptyp_any, (Asttypes.NoVariance,Asttypes.NoInjectivity)
      ]
    in
    let constructors =
      Map.fold ~init:[] constructors
        ~f:(fun ~key:name ~data:languages acc ->
            let res =
              ptyp_constr
                (Lident "t")
                [ constructor_membership_object all_types languages
                ; Constructor_name.variant_type name
                ]
            in
            constructor_declaration
              ~res
              ~args:(Pcstr_tuple [])
              (Constructor_name.to_string name)
            :: acc
          )
    in
    let kind = Parsetree.Ptype_variant constructors in
    type_declaration "t"
      ~kind
      ~params

  let constructors_by_defined_type t 
    : Type_instance.t list Language_name.Map.t Constructor_name.Map.t Defined_type_name.Map.t
    =
    let cd = all_constructor_descriptions t in
    Transposer.transpose_231
      ~empty2:Defined_type_name.Map.empty
      ~empty3:Constructor_name.Map.empty
      cd
end

module Synthesize = struct

  let make_hierarchy (t : Language_group.t) =
    let all_types = Language_group.all_types t in
    let hier = Hierarchical_module.create () in
    let constructors_by_defined_type =
      Language_group.constructors_by_defined_type t
    in
    Map.iteri constructors_by_defined_type
      ~f:(fun ~key:defined_type_name ~data:constructors ->
          let defined_type_module =
            Hierarchical_module.layer_at_path hier
              (Defined_type_name.to_module_path defined_type_name)
          in
          let constructor_type = Language_group.constructor_type all_types constructors in
          Hierarchical_module.insert
            defined_type_module
            [Module_name.constructor_module ]
            (`Type constructor_type)
        )
    ;
    hier

  let types_module (hier) =
    let open Ast_builder in
    let map_element ~path_rev:_ element =
      match element with
      | `Type ty -> psig_type Recursive [ ty ]
    in
    let map_layer ~path_rev layer =
      let name = Option.map ~f:Module_name.to_string (List.hd path_rev) in
      let type_ = pmty_signature layer in
      psig_module (module_declaration ~name ~type_)
    in
    let mat = 
      Hierarchical_module.materialize hier
        ~path_rev:[]
        ~map_element
        ~map_layer
      |> pmty_signature
    in
    let expr =
      pmod_constraint
        (pmod_ident (Lident "Types"))
        mat
    in
    pstr_recmodule [ module_binding ~name:(Some "Types") ~expr ]

  let synth t =
    make_hierarchy t
    |> types_module
end

