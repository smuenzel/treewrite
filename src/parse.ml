open! Core
open! Util
open! Compile

module Ast = Ppxlib_ast.Ast

class defined_type_substitute = object
  inherit [Longident.t Longident.Map.t] Ast.map_with_context

  method string _ s = s
  method option map_contents ctx s = Option.map ~f:(map_contents ctx) s
  method list map_contents ctx s = List.map ~f:(map_contents ctx) s
  method int _ s = s
  method char _ s = s
  method bool _ s = s

  method! longident subst ident =
    match Map.find subst ident with
    | None -> ident
    | Some defined -> defined
end

let parse_record ~path name (entries : Ast.label_declaration list) =
  let entries =
    List.map entries
      ~f:(function
          | { pld_name
            ; pld_mutable = Mutable
            ; _
            } ->
            raise_s [%message "record cannot have mutable fields"
                name
                pld_name.txt
            ]
          | { pld_name
            ; pld_type
            ; _
            } -> 
            Record_label.of_string pld_name.txt
          , pld_type
        )
  in
  let name = Module_name.of_string name in
  [ path @ [ name ], `Record entries ]

let parse_variant ~path name (constructors : Ast.constructor_declaration list) =
  let name = Module_name.of_string name in
  let extra_types, constructors =
    List.fold_map ~init:[] constructors
      ~f:(fun acc constructor ->
          match constructor with
          | { pcd_name
            ; pcd_res = Some _
            ; _
            } ->
            raise_s [%message "GADT syntax is not supported"
                (name : Module_name.t)
                pcd_name.txt
            ]
          | { pcd_name
            ; pcd_args = Pcstr_tuple tuple
            ; _
            } ->
            let constructor_name = Constructor_name.of_string pcd_name.txt in
            acc, (constructor_name, `Tuple tuple)
          | { pcd_name
            ; pcd_args = Pcstr_record record
            ; _
            } ->
            let local_path = path @ [ name ] in
            let record_name = pcd_name.txt in
            let constructor_name = Constructor_name.of_string pcd_name.txt in
            let record = parse_record ~path:local_path record_name record in
            let alias_name = local_path @ [ Module_name.of_string record_name ] in
            (record @ acc), (constructor_name, `Alias alias_name)
        )
  in
  (path @ [ name ], `Variant constructors) :: extra_types

let substitute_of_type_declaration ~path ~language_name (tdecl : Ast.type_declaration) =
  match tdecl with
  | { ptype_name; _ } ->
    let pathname = path @ [ Module_name.of_string ptype_name.txt ] in
    let typename =
      Defined_type_name.of_module_path pathname
      |> Defined_type_name.ident_in_language ~language_name
    in
    Longident.Map.singleton (Lident ptype_name.txt) typename

let merge_substitute a b =
  Map.merge a b
    ~f:(fun ~key data ->
        match data with
        | `Left x | `Right x -> Some x
        | `Both (x,y) ->
          if Longident.equal x y
          then Some x
          else raise_s [%message "cannot merge"
                (key : Longident.t)
                (x : Longident.t)
                (y : Longident.t)
            ]
      )

let parse_type_declaration ~path (tdecl : Ast.type_declaration) =
  match tdecl with
  | { ptype_params = _ :: _; ptype_name; _ } ->
    raise_s [%message "type cannot have parameters"
        (ptype_name.txt)
    ]
  | { ptype_manifest = Some _; ptype_name; _ } ->
    raise_s [%message "manifest types not supported"
        (ptype_name.txt)
    ]
  | { ptype_kind = Ptype_variant constructors
    ; ptype_name
    ; _
    } -> 
    parse_variant ~path ptype_name.txt constructors
  | { ptype_kind = Ptype_record entries
    ; ptype_name
    ; _
    } ->
    parse_record ~path ptype_name.txt entries
  | { ptype_name; _ } ->
    raise_s [%message "only variant and record types supported"
        (ptype_name.txt)
    ]

let parse_signature_item ~language_name ~path ({ psig_desc; _ } : Ast.signature_item) =
  match psig_desc with
  | Psig_type (Recursive, list) ->
    let substitutes =
      List.fold ~init:Longident.Map.empty list
        ~f:(fun acc tdecl ->
            merge_substitute
              (substitute_of_type_declaration ~language_name ~path tdecl) acc
          )
    in
    let d = new defined_type_substitute in
    let list = List.map ~f:(d#type_declaration substitutes) list in
    List.concat_map ~f:(parse_type_declaration ~path) list
  | Psig_type (Nonrecursive, _) ->
    raise_s [%message "expected recursive type"]
  | _ ->
    raise_s [%message "expected type"]

let rec all_typenames (ct : Ast.core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr (typename, parameters) ->
    List.fold ~init:(External_type_descriptor.Set.singleton
                       { fully_qualified_name = typename.txt
                       ; parameter_count = List.length parameters
                       })
        ~f:(fun acc p -> Set.union acc (all_typenames p))
        parameters
  | Ptyp_any
  | Ptyp_var _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_variant _
  | Ptyp_package _
  | Ptyp_extension _
  | Ptyp_poly _
  | Ptyp_arrow _
  | Ptyp_object _
    -> External_type_descriptor.Set.empty
  | Ptyp_tuple list ->
    List.fold ~init:External_type_descriptor.Set.empty
      ~f:(fun acc p -> Set.union acc (all_typenames p))
      list

module Pre_type_instance = struct
  type t =
    { typename : Longident.t
    ; parameters : t list
    }

  let rec of_coretype_exn (ct : Ast.core_type) =
    match ct.ptyp_desc with
    | Ptyp_any
    | Ptyp_var _
    | Ptyp_class _
    | Ptyp_alias _
    | Ptyp_variant _
    | Ptyp_package _
    | Ptyp_extension _
    | Ptyp_poly _
      -> assert false
    | Ptyp_arrow _
    | Ptyp_object _
      -> assert false
    | Ptyp_tuple [ _ ]
    | Ptyp_tuple []
    | Ptyp_tuple _ -> assert false
    | Ptyp_constr (typename, parameters) ->
      let parameters = List.map ~f:of_coretype_exn parameters in
      { typename = typename.txt
      ; parameters
      }
end

let gather_types ~language_name signature_types =
  let defined_types =
    List.map signature_types
      ~f:(fun (tname, _) ->
          Defined_type_name.of_module_path tname
        , Defined_type_id.create ()
        )
    |> Defined_type_name.Map.of_alist_exn
  in
  let coretypes =
    List.fold signature_types ~init:External_type_descriptor.Set.empty
      ~f:(fun acc (_tname, kind) ->
          match kind with
          | `Record entries ->
            List.fold ~init:acc entries
              ~f:(fun acc (_,t) ->
                  Set.union (all_typenames t) acc
                )
          | `Variant const ->
            List.fold ~init:acc const ~f:(fun acc (_,t) ->
                match t with
                | `Alias _ -> acc
                | `Tuple types ->
                  List.fold ~init:acc ~f:(fun acc t -> Set.union acc (all_typenames t))
                    types
              )
        )
  in
  ()
  (*
    List.fold signature_types ~init:[]
      ~f:(fun acc (_tname, kind) ->
          match kind with
          | `Record entries ->
            List.fold ~init:acc entries ~f:(fun acc (_,t) -> t :: acc)
          | `Variant const ->
            List.fold ~init:acc const ~f:(fun acc (_,t) ->
                match t with
                | `Alias _ -> acc
                | `Tuple types -> types @ acc
              )
        )
    |> List.map ~f:Pre_type_instance.of_coretype_exn
  in
  ()
     *)

let x ~path ~language_name sigi =
  parse_signature_item ~language_name ~path sigi
  |> gather_types ~language_name
