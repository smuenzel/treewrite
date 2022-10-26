open! Core
open! Util
open! Compile


let linstance_open ?(data_var="data") language_name =
  let open Ast_builder in
  let data_var = ptyp_var data_var in
  ptyp_object
    [ otag
        (Language_name.to_variable_name language_name)
        data_var
    ]
    Open

let make_constructor_packed_type ~name ~has_param language_name =
  let open Ast_builder in
  let constructor_tag_var = "tag" in
  let data_var = "data" in
  let constructor_type =
    ptyp_constr
      (Longident.of_list [ "Constructors"; "t"])
      [ linstance_open ~data_var language_name
      ; ptyp_var constructor_tag_var
      ]
  in
  let kind =
    let res_param =
      if has_param
      then [ ptyp_var constructor_tag_var ]
      else []
    in
    let instance =
      ptyp_constr
        (Longident.of_list [ "Linstance"; "t" ])
        [ constructor_type
        ; ptyp_var data_var
        ]
    in
    Parsetree.Ptype_variant
      [ constructor_declaration "L"
          ~res:(ptyp_constr (Lident name) res_param)
          ~args:(Pcstr_tuple [ instance ])
      ]
  in
  let params =
    if has_param
    then [ ptyp_var constructor_tag_var, (Asttypes.NoVariance,Asttypes.NoInjectivity) ]
    else []
  in
  let tdecl =
    type_declaration
      ~kind
      ~params
      name
  in
  unbox_type tdecl

let make_named_type language_name =
  make_constructor_packed_type ~name:"named" ~has_param:true language_name

let make_unnamed_type language_name =
  make_constructor_packed_type ~name:"t" ~has_param:false language_name

let make_record_type ?prefix ~all_types entries =
  let open Ast_builder in
  let entries =
    List.map entries
      ~f:(fun (label, instance) ->
          label_declaration
            ~type_:(All_types.With_shared.to_type ?prefix all_types instance)
            (Record_label.to_string label)
        )
  in
  type_declaration
    "t"
    ~kind:(Parsetree.Ptype_record entries)

let make_shared_type_for_language ?prefix ~all_types ~language_name shared =
  let open Ast_builder in
  let manifest =
    All_types.With_shared.instantiate_shared_type ?prefix all_types shared language_name
  in
  type_declaration
    "t"
    ~manifest

let make_shared_type ?prefix ~all_types (shared : Defined_type_description.Shared.t) =
  let open Ast_builder in
  let ({ id = _
       ; name = _
       ; shape
       ; type_instance_parameters
       } : Defined_type_description.Shared.t) = shared
  in
  let type_instance_parameters =
    Map.map ~f:(fun i -> ptyp_var (Printf.sprintf "v%i" i)) type_instance_parameters
  in
  let params = 
    Map.length type_instance_parameters
    (* CR smuenzel: fix *)
    |> List.init
      ~f:(fun i ->
          ptyp_var (Printf.sprintf "v%i" i)
        , (Asttypes.NoVariance,Asttypes.NoInjectivity)
        )
  in
  let entries =
    match shape with
    | Record entries ->
      List.map entries
        ~f:(fun (label, instance) ->
            label_declaration
              ~type_:(All_types.With_shared.shared_to_type ?prefix all_types type_instance_parameters instance)
              (Record_label.to_string label)
          )
    | _ -> assert false
  in
  type_declaration
    ~params
    ~kind:(Parsetree.Ptype_record entries)
    "t"

let make_hierarchy ?prefix (t : Language_group.With_shared.t) =
  let all_types = Language_group.With_shared.all_types t in
  let hier = Hierarchical_module.create () in
  let constructors_by_defined_type =
    Language_group.With_shared.constructors_by_defined_type t
  in
  Map.iteri constructors_by_defined_type
    ~f:(fun ~key:defined_type_name ~data:constructors ->
        let defined_type_module =
          Hierarchical_module.layer_at_path hier
            (Defined_type_name.to_module_path defined_type_name)
        in
        let constructor_type =
          Language_group.constructor_type ?prefix all_types constructors
        in
        Hierarchical_module.insert
          defined_type_module
          [ Module_name.constructor_module ]
          (`Type constructor_type)
      )
  ;
  let variant_types_and_languages =
    Map.map
      constructors_by_defined_type
      ~f:(Map.fold
            ~init:Language_name.Set.empty
            ~f:(fun ~key:_ ~data acc ->
                Set.union acc (Map.key_set data)
              )
         )
  in
  Map.iteri variant_types_and_languages
    ~f:(fun ~key:defined_type_name ~data:languages ->
        let defined_type_module =
          Hierarchical_module.layer_at_path hier
            (Defined_type_name.to_module_path defined_type_name)
        in
        Set.iter languages
          ~f:(fun language ->
              let insert = 
                Hierarchical_module.insert
                  defined_type_module
                  [ Language_name.to_module_name language ]
              in
              insert (`Type (make_named_type language));
              insert (`Type (make_unnamed_type language));
            )
      );
  let records_by_defined_type = Language_group.With_shared.records_by_defined_type t in
  Map.iteri records_by_defined_type
    ~f:(fun ~key:defined_type_name ~data:records_by_language ->
        let defined_type_module =
          Hierarchical_module.layer_at_path hier
            (Defined_type_name.to_module_path defined_type_name)
        in
        Map.iteri records_by_language
          ~f:(fun ~key:language_name ~data:record ->
              Hierarchical_module.insert
                defined_type_module
                [ Language_name.to_module_name language_name ]
                (`Type (make_record_type ?prefix ~all_types record))
            )
      )
  ;
  let shared_by_defined_type = Language_group.With_shared.shared_by_defined_type t in
  Map.iteri shared_by_defined_type
    ~f:(fun ~key:defined_type_name ~data:shared_by_language ->
        let defined_type_module =
          Hierarchical_module.layer_at_path hier
            (Defined_type_name.to_module_path defined_type_name)
        in
        Map.iteri shared_by_language
          ~f:(fun ~key:language_name ~data:shared ->
              Hierarchical_module.insert
                defined_type_module
                [ Language_name.to_module_name language_name ]
                (`Type (make_shared_type_for_language ?prefix ~all_types ~language_name shared))
            )
      )
  ;
  let shared_types = Language_group.With_shared.shared_types t in
  Map.iteri shared_types
    ~f:(fun ~key:id ~data:type_descr ->
        let t = make_shared_type ?prefix ~all_types type_descr in
        let path = Defined_type_name.to_shared_module_path type_descr.name id in
        Hierarchical_module.insert
          hier
          path
          (`Type t)
      )
  ;
  hier

let types_ident = Longident.Lident "Types" 

let types_module t =
  let open Ast_builder in
  let hier = make_hierarchy ~prefix:types_ident t in
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
      (pmod_ident types_ident)
      mat
  in
  pstr_recmodule [ module_binding ~name:(Some (Longident.name types_ident)) ~expr ]

let nonrec_module t =
  let open Ast_builder in
  let hier = make_hierarchy t in
  let has_include = Hash_set.Poly.create () in
  let map_element ~path_rev element =
    match element with
    | `Type { Parsetree. ptype_name = _ ; ptype_params = _; _ } ->
      if Hash_set.mem has_include path_rev
      then []
      else begin
        let inc =
          pstr_include
            (include_infos
               (pmod_ident
                  (Longident.of_list
                     (List.concat 
                        [ [ Longident.name types_ident ]
                        ; (List.rev_map ~f:Module_name.to_string path_rev)
                        ]))))
        in
        Hash_set.add has_include path_rev;
        [ inc ]
      end
      (* {[
           let ptype_name = ptype_name.txt in
           let params =
             List.mapi ptype_params ~f:(fun i _ -> ptyp_var (Printf.sprintf "v%i" i))
           in
           let manifest =
             ptyp_constr
               (Longident.of_list
                  (List.concat 
                     [ [ Longident.name types_ident ]
                     ; (List.rev_map ~f:Module_name.to_string path_rev)
                     ; [ ptype_name ]
                     ]
                  ))
               params
           in
           let tdecl : Parsetree.type_declaration =
             type_declaration
               ptype_name
               ~manifest
               ~params:(List.map params
                          ~f:(fun x -> x, (Asttypes.NoVariance,Asttypes.NoInjectivity)))
           in
           pstr_type Recursive [ tdecl ]
         ]} *)
  in
  let map_layer ~path_rev layer =
    let name = Option.map ~f:Module_name.to_string (List.hd path_rev) in
    let expr = pmod_structure (List.concat layer) in
    [ pstr_module (module_binding ~name ~expr) ]
  in
  let mat =
    Hierarchical_module.materialize hier
      ~path_rev:[]
      ~map_element
      ~map_layer
  in
  List.concat mat

module Arrow = struct
  type ('a, 'b) t =
    { source : 'a
    ; destination : 'b
    }

  type 'a t' = ('a, 'a) t
end

module Mapper_row = struct
  module Kind = struct
    type t =
      | Defined of Defined_type_id.t Arrow.t'
      | Constructor of (Defined_type_id.t * Constructor_name.t, Defined_type_id.t) Arrow.t
  end

  type t =
    { name : string
    ; kind : Kind.t
    ; core_type_mapping : Parsetree.core_type Arrow.t'
    } 

  let make (t : Language_group.With_shared.t) ~source ~destination =
    let open Ast_builder in
    let all_types = Language_group.With_shared.all_types t in
    let source = Map.find_exn t.languages source in
    let destination = Map.find_exn t.languages destination in
    let rows =
      let direct_correspondence =
        Map.merge source.types_by_name destination.types_by_name
          ~f:(fun ~key m ->
              match m with
              | `Left _ -> None
              | `Right _ -> None
              | `Both (t_source, t_destination) ->
                { name = Defined_type_name.to_fun_name key
                ; kind = Defined { source = t_source; destination = t_destination }
                ; core_type_mapping =
                    { source = All_types.With_shared.instantiate_defined_type all_types t_source
                    ; destination = All_types.With_shared.instantiate_defined_type all_types t_destination
                    }
                }
                |> Some
            )
      in
      let direct_correspondence_constructor =
        Map.merge source.types_by_name destination.types_by_name
          ~f:(fun ~key m ->
              match m with
              | `Left _ -> None
              | `Right _ -> None
              | `Both (t_source_id, t_destination) ->
                let t_source = Map.find_exn source.types_by_id t_source_id in
                let destination =
                  All_types.With_shared.instantiate_defined_type all_types t_destination
                in
                match t_source with
                | Record _ | Shared _ -> None
                | Variant constructors ->
                  List.map constructors
                    ~f:(fun (constructor, _) ->
                        let named_type =
                          (* CR smuenzel: need function for this *)
                          ptyp_constr
                            (Defined_type_name.ident_in_language
                               key
                               ~language_name:source.name
                               ~t_name:"named"
                            )
                            [ Constructor_name.variant_type constructor
                            ]
                        in
                        { name = Defined_type_name.to_fun_name ~constructor key
                        ; kind =
                            Constructor
                              { source = t_source_id, constructor
                              ; destination = t_destination
                              }
                        ; core_type_mapping =
                            { source = named_type
                            ; destination
                            }
                        }
                      )
                  |> Some
            )
      in
      List.concat
        [ Map.data direct_correspondence
        ; Map.data direct_correspondence_constructor |> List.concat
        ]
    in
    rows

end

let mapper_type (t : Language_group.With_shared.t) ~source ~destination =
  let rows = Mapper_row.make t ~source ~destination in
  let open Ast_builder in
  let self_name = "mapper" in
  let self_name_sealed = self_name ^ "_sealed" in
  let sealer_name = "seal_" ^ self_name in
  let self_type = ptyp_constr (Lident self_name) [] in
  let self_type_sealed = ptyp_constr (Lident self_name_sealed) [] in
  let make_record self_name self_type =
    let label_declarations =
      List.map rows
        ~f:(fun { name
                ; core_type_mapping = { source; destination }
                ; kind = _
                } ->
            let type_ =
              List.filter_opt
                [ self_type
                ; Some source
                ; Some destination
                ]
              |> ptyp_arrows
            in
            label_declaration
              ~type_
              name
          )
    in
    let tdecl =
      type_declaration
        self_name
        ~kind:(Ptype_record label_declarations)
    in
    tdecl
  in
  let sealer =
    let unsealed = Longident.Lident "unsealed" in
    let sealed = Longident.Lident "sealed" in
    let entries =
      List.map rows
        ~f:(fun { name
                ; core_type_mapping = { source; _ }
                ; kind = _
                } ->
            let field_name = Location.mknoloc (Longident.Lident name) in
            let eval_fun =
              pexp_apply
                (inlined_hint (pexp_field (pexp_construct unsealed None) field_name))
                [ Nolabel, pexp_construct sealed None
                ; Nolabel, pexp_construct (Lident "input") None
                ]
            in
            field_name
          , pexp_fun
              Nolabel
              None
              (ppat_constraint (ppat_construct (Lident "input") None) source)
              eval_fun
          )
    in
    let sealing_expr =
      pexp_constraint (pexp_record entries None) self_type_sealed
    in
    let sealing_expr =
      pexp_let Recursive
        [ value_binding ~pat:(ppat_construct sealed None) ~expr:sealing_expr
        ]
        (pexp_ident sealed)
    in
    let pat = ppat_construct (Lident sealer_name) None in
    let expr =
      pexp_fun 
        Nolabel
        None
        (ppat_constraint (ppat_construct unsealed None) self_type)
        (pexp_constraint sealing_expr self_type_sealed)
      |> inlined_always
    in
    pstr_value Nonrecursive
      [ value_binding ~pat ~expr
      ]
  in
  [ pstr_type
      Recursive
      [ make_record self_name (Some self_type_sealed)
      ; make_record self_name_sealed None
      ]
  ; sealer
  ]

let make_mappers t mappers =
  let open Ast_builder in
  List.map mappers
    ~f:(fun (source, destination) ->
        let name =
          Language_name.to_mapper_module_name source destination
          |> Module_name.to_string
        in
        let structure =
          List.concat
            [ [ pstr_attribute allow_duplicate_attribute ]
            ; mapper_type t ~source ~destination
            ]
        in
        let expr = pmod_structure structure in
        pstr_module
          (module_binding ~name:(Some name) ~expr)
      )

let synth t ~mappers =
  let t = Language_group.share_types t in
  List.concat
    [ [ types_module t ]
    ; nonrec_module t
    ; make_mappers t mappers
    ]
