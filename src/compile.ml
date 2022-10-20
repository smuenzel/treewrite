open! Core
open! Util

module External_type_id = Unique_id.Int()
module Defined_type_id = Unique_id.Int()
module Shared_type_id = Unique_id.Int()

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

  let ident_in_language ?prefix t ~language_name =
    let module_path_rev =
      Nonempty_list.to_list t
      |> List.rev_map ~f:(Module_name.to_string)
    in
    let n =
      "t"
      ::
      (Language_name.to_module_name language_name
       |> Module_name.to_string)
      :: module_path_rev
      |> Longident.of_list_rev
    in
    match prefix with
    | None -> n
    | Some prefix -> Longident.dot prefix n

  let to_module_path t = Nonempty_list.to_list t

  let to_shared_module_path t shared_id =
    to_module_path t
    @
    [ Printf.sprintf "Shared_%i" (Shared_type_id.to_int_exn shared_id)
      |> Module_name.of_string
    ]

  let shared_ident t shared_id =
    let module_path_rev =
      Nonempty_list.to_list t
      |> List.rev_map ~f:(Module_name.to_string)
    in
    "t"
    ::
    (Printf.sprintf "Shared_%i" (Shared_type_id.to_int_exn shared_id))
    :: module_path_rev
    |> Longident.of_list_rev


  let to_module_path t = Nonempty_list.to_list t
end


module Type_id = struct
  type t =
    | External of External_type_id.t
    | Defined of Defined_type_id.t
  [@@deriving sexp]

  (*
  let all_external_types = function
    | External id -> External_type_id.Set.singleton id
    | Defined _ -> External_type_id.Set.empty
     *)

  let replace_external_ids mapping t =
    match t with
    | Defined _ -> t
    | External eid ->
      match Map.find mapping eid with
      | None -> t
      | Some eid -> External eid

  module With_shared_instance = struct
    type t =
      | External of External_type_id.t
      | Defined of Defined_type_id.t
      | Shared_instance of Defined_type_name.t
    [@@deriving sexp]
  end

  module Defined_or_shared = struct
    type t =
      | Defined of Defined_type_id.t
      | Shared of Shared_type_id.t
    [@@deriving sexp]
  end
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

  (*
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
     *)

  let to_ident t = t.fully_qualified_name
end

module Type_instance = struct
  module General = struct
    type 'id t =
      { id : 'id
      ; parameters : 'id t list
      } [@@deriving sexp, compare]

    let rec map ~f { id; parameters} =
      let id = f id in
      let parameters = List.map parameters ~f:(map ~f) in
      { id
      ; parameters
      }
  end

  module With_shared = struct
    type t = Type_id.With_shared_instance.t General.t [@@deriving sexp]
  end

  type t = Type_id.t General.t [@@deriving sexp]
  (* Parameters must be instantiated for now *)

  (*
  let all_external_types (t : t) = Type_id.all_external_types t.id
     *)

  let rec replace_external_ids mapping (t : t) : t =
    let parameters = List.map t.parameters ~f:(replace_external_ids mapping) in
    let id = Type_id.replace_external_ids mapping t.id in
    { id
    ; parameters
    }
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
  module General = struct
    type 'instance t =
      | Record of (Record_label.t * 'instance) list
      | Variant of (Constructor_name.t * 'instance list) list
    [@@deriving sexp]

    (*
    let all_external_types t ~external_types_of_instance =
      match t with
      | Record r ->
        let gather =
          List.fold ~init:External_type_id.Set.empty
            ~f:(fun acc (_,instance) ->
                Set.union acc (external_types_of_instance instance)
              )
        in
        gather r
      | Variant v ->
        let gather =
          List.fold ~init:External_type_id.Set.empty
            ~f:(fun acc (_,instances) ->
                List.fold ~init:acc instances
                  ~f:(fun acc instance ->
                      Set.union acc (external_types_of_instance instance)
                    )
              )
        in
        gather v
       *)

  end

  module With_shared = struct
    type 'instance t =
      | Record of (Record_label.t * 'instance) list
      | Variant of (Constructor_name.t * 'instance list) list
      | Shared of Shared_type_id.t
    [@@deriving sexp]

    let of_general : _ General.t -> _ t= function
      | Record x -> Record x
      | Variant x -> Variant x
  end

  type t = Type_instance.t General.t
  [@@deriving sexp]

  (*
  let all_external_types t =
    General.all_external_types t
      ~external_types_of_instance:Type_instance.all_external_types
     *)

  let replace_external_ids (t : t) mapping : t =
    match t with
    | Record r ->
      Record (List.map r ~f:(Tuple2.map_snd ~f:(Type_instance.replace_external_ids mapping)))
    | Variant v ->
      Variant (List.map v ~f:(Tuple2.map_snd ~f:(List.map ~f:(Type_instance.replace_external_ids mapping))))

end
  
module Defined_type_description = struct
  type t =
    { id : Defined_type_id.t
    ; name : Defined_type_name.t
    ; language : Language_name.t
    ; shape : Type_shape.t
    } [@@deriving sexp]

  module Shared = struct
    type t =
      { id : Shared_type_id.t
      ; name : Defined_type_name.t
      ; shape : Type_instance.With_shared.t Type_shape.General.t
      ; type_instance_parameters : int Defined_type_name.Map.t
      } [@@deriving sexp]

    let parameters_in_order t =
      Map.to_alist t.type_instance_parameters
      |> List.sort ~compare:(fun (_,a) (_,b) -> Int.compare a b)
      |> List.map ~f:fst

    let to_ident ?prefix t =
      let n =
        Defined_type_name.shared_ident t.name t.id
      in
      match prefix with
      | None -> n
      | Some prefix -> Longident.dot prefix n
  end

  module With_shared = struct
    type t =
      { id : Defined_type_id.t
      ; name : Defined_type_name.t
      ; language : Language_name.t
      ; shape : Type_instance.t Type_shape.With_shared.t
      } [@@deriving sexp]

    let to_ident ?prefix t =
      Defined_type_name.ident_in_language ?prefix t.name ~language_name:t.language
  end
end

module All_types = struct
  type t =
    { external_types : External_type_descriptor.t External_type_id.Map.t
    ; defined_types : Defined_type_description.t Defined_type_id.Map.t
    } [@@deriving sexp]

  let defined_exn t id =
    Map.find_exn t.defined_types id

  module With_shared = struct
    type t =
      { external_types : External_type_descriptor.t External_type_id.Map.t
      ; defined_types : Defined_type_description.With_shared.t Defined_type_id.Map.t
      ; shared_types : Defined_type_description.Shared.t Shared_type_id.Map.t
      } [@@deriving sexp]

    let rec to_type ?prefix (t : t) ({ id; parameters } : Type_instance.t) =
      let open Ast_builder in
      let parameters = List.map parameters ~f:(to_type ?prefix t) in
      let name =
        match id with
        | External id ->
          Map.find_exn t.external_types id
          |> External_type_descriptor.to_ident
        | Defined id ->
          Map.find_exn t.defined_types id
          |> Defined_type_description.With_shared.to_ident ?prefix
      in
      ptyp_constr
        name
        parameters

    let rec shared_to_type
        ?prefix
        (t : t)
        instance_params
        ({ id; parameters} : Type_instance.With_shared.t)
      =
      let open Ast_builder in
      let parameters = List.map parameters ~f:(shared_to_type ?prefix t instance_params) in
      match id with
      | External id ->
        let name =
          Map.find_exn t.external_types id
          |> External_type_descriptor.to_ident
        in
        ptyp_constr
          name
          parameters
      | Defined id ->
        let name =
          Map.find_exn t.defined_types id
          |> Defined_type_description.With_shared.to_ident ?prefix
        in
        ptyp_constr
          name
          parameters
      | Shared_instance instance ->
        assert (List.is_empty parameters);
        Map.find_exn instance_params instance

    let instantiate_shared_type ?prefix (t : t) shared_type_id language_name=
      let open Ast_builder in
      let shared_type = Map.find_exn t.shared_types shared_type_id in
      let parameters =
        Defined_type_description.Shared.parameters_in_order shared_type
        |> List.map ~f:(Defined_type_name.ident_in_language ?prefix ~language_name)
        |> List.map ~f:(fun name -> ptyp_constr name [])
      in
      ptyp_constr
        (Defined_type_description.Shared.to_ident ?prefix shared_type)
        parameters
  end
end

module Language_definition = struct
  type t =
    { name : Language_name.t
    ; types_by_name : Defined_type_id.t Defined_type_name.Map.t
    ; types_by_id : Type_shape.t Defined_type_id.Map.t
    } [@@deriving sexp, fields]

  module With_shared = struct
    type t =
      { name : Language_name.t
      ; types_by_name : Defined_type_id.t Defined_type_name.Map.t
      ; types_by_id : Type_instance.t Type_shape.With_shared.t Defined_type_id.Map.t
      } [@@deriving sexp, fields]

    let all_defined_types t : Defined_type_description.With_shared.t Defined_type_id.Map.t =
      Map.fold t.types_by_name ~init:Defined_type_id.Map.empty
        ~f:(fun ~key:name ~data:id acc ->
            let shape = Map.find_exn t.types_by_id id in
            let data =
              { Defined_type_description.With_shared.
                id
              ; name
              ; language = t.name
              ; shape
              }
            in
            Map.add_exn acc ~key:id ~data
          )

    let all_constructor_descriptions t =
      Map.filter_map t.types_by_name
        ~f:(fun id ->
            match Map.find_exn t.types_by_id id with
            | Record _ -> None
            | Variant constructors -> 
              Some (Constructor_name.Map.of_alist_exn constructors)
            | Shared _ -> None
          )

    let all_record_descriptions t =
      Map.filter_map t.types_by_name
        ~f:(fun id ->
            match Map.find_exn t.types_by_id id with
            | Record entries -> Some entries
            | Variant _ -> None
            | Shared _ -> None
          )

    let all_shared_descriptions t =
      Map.filter_map t.types_by_name
        ~f:(fun id ->
            match Map.find_exn t.types_by_id id with
            | Record _ -> None
            | Variant _ -> None
            | Shared shared -> Some shared
          )
  end


  let replace_external_ids t mapping =
    let types_by_id =
      Map.map t.types_by_id
        ~f:(fun shape ->
            Type_shape.replace_external_ids shape mapping
          )
    in
    { t with types_by_id }

  (*
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
     *)

  let all_record_descriptions t =
    Map.filter_map t.types_by_name
      ~f:(fun id ->
          match Map.find_exn t.types_by_id id with
          | Record entries -> Some entries
          | Variant _ -> None
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


module Hierarchical_module = Hierarchical.Make(Module_name)

module Language_group = struct
  type t =
    { languages : Language_definition.t Language_name.Map.t
    ; external_types : External_type_descriptor.t External_type_id.Map.t
    }

  module With_shared = struct
    type t =
      { languages : Language_definition.With_shared.t Language_name.Map.t
      ; external_types : External_type_descriptor.t External_type_id.Map.t
      ; shared_types : Defined_type_description.Shared.t Shared_type_id.Map.t
      }

    let all_types ({ languages; external_types; shared_types } : t) =
      let defined_types =
        Map.fold languages
          ~init:Defined_type_id.Map.empty
          ~f:(fun ~key:_ ~data:l acc ->
              Language_definition.With_shared.all_defined_types l
              |> Map.merge acc
                ~f:(fun ~key:_ -> function
                    | `Left x | `Right x -> Some x
                    | `Both _ -> assert false
                  )
            )
      in
      { All_types.With_shared.
        external_types
      ; defined_types
      ; shared_types
      }

    let all_constructor_descriptions t 
      : Type_instance.t list Constructor_name.Map.t Defined_type_name.Map.t Language_name.Map.t =
      Map.map t.languages ~f:Language_definition.With_shared.all_constructor_descriptions

    let constructors_by_defined_type t 
      : Type_instance.t list Language_name.Map.t Constructor_name.Map.t Defined_type_name.Map.t
      =
      let cd = all_constructor_descriptions t in
      Transpose.transpose_231
        ~empty2:Defined_type_name.Map.empty
        ~empty3:Constructor_name.Map.empty
        cd

    let all_record_descriptions t =
      Map.map t.languages ~f:Language_definition.With_shared.all_record_descriptions

    let records_by_defined_type t 
      : (Record_label.t * Type_instance.t) list Language_name.Map.t Defined_type_name.Map.t =
      let rd = all_record_descriptions t in
      Transpose.map2
        ~empty:Defined_type_name.Map.empty
        rd

    let all_shared_descriptions t =
      Map.map t.languages ~f:Language_definition.With_shared.all_shared_descriptions

    let shared_by_defined_type t =
      let rd = all_shared_descriptions t in
      Transpose.map2
        ~empty:Defined_type_name.Map.empty
        rd

  end

  let merge t1 t2 =
    let gather =
      Map.fold
        ~f:(fun ~key:type_id ~data (subst_name, subst_id, merged) ->
            let { External_type_descriptor.fully_qualified_name; _ } = data in
            match Map.find subst_name fully_qualified_name with
            | Some canonical_id ->
              subst_name
            , Map.add_exn subst_id ~key:type_id ~data:canonical_id
            , merged
            | None ->
              Map.add_exn subst_name ~key:fully_qualified_name ~data:type_id
            , subst_id
            , Map.add_exn merged ~key:type_id ~data
          )
    in
    let init =
      gather
        ~init:(Longident.Map.empty, External_type_id.Map.empty, External_type_id.Map.empty)
        t1.external_types
    in
    let _subst_name, subst_id, merged_external_types = gather ~init t2.external_types in
    let languages =
    Map.merge t1.languages t2.languages
      ~f:(fun ~key elt ->
          match elt with
          | `Left x | `Right x ->
            Some (Language_definition.replace_external_ids x subst_id)
          | `Both _ ->
            raise_s [%message "Duplicate Language" (key : Language_name.t)]
        )
    in
    { languages
    ; external_types = merged_external_types
    }

  let all_types ({ languages; external_types} : t) =
    let defined_types =
      Map.fold languages
        ~init:Defined_type_id.Map.empty
        ~f:(fun ~key:_ ~data:l acc ->
            Language_definition.all_defined_types l
            |> Map.merge acc
              ~f:(fun ~key:_ -> function
                  | `Left x | `Right x -> Some x
                  | `Both _ -> assert false
                )
           )
    in
    { All_types.
      external_types
    ; defined_types
    }

  let maybe_tuple ctl =
    match ctl with
    | [ x ] -> x
    | xs -> Ast_builder.ptyp_tuple xs

  let all_record_descriptions t =
    Map.map t.languages ~f:Language_definition.all_record_descriptions

  let constructor_membership_object
      ?prefix
      all_types
      (constructor : Type_instance.t list Language_name.Map.t)
    =
    let open Ast_builder in
    let members =
      Map.fold ~init:[] constructor
        ~f:(fun ~key:language_name ~data:type_instances acc ->
            otag
              (Language_name.to_variable_name language_name)
              (maybe_tuple
                 (List.map ~f:(All_types.With_shared.to_type ?prefix all_types) type_instances))
            :: acc
          )
    in
    ptyp_object
      members
      Closed

  let constructor_type
      ?prefix
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
                [ constructor_membership_object ?prefix all_types languages
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

  let records_by_defined_type t 
    : (Record_label.t * Type_instance.t) list Language_name.Map.t Defined_type_name.Map.t =
    let rd = all_record_descriptions t in
    Transpose.map2
      ~empty:Defined_type_name.Map.empty
      rd

  let find_shared_records
      ~(all_types : All_types.t)
      (records_by_language : (Record_label.t * Type_instance.t) list Language_name.Map.t)
    =
    let open struct
      module Entry = struct
        module T = struct
          type t =
            [ `External of External_type_id.t
            | `Defined of Defined_type_id.t
            | `Instance_in_lang of Defined_type_name.t
            ] [@@deriving compare, sexp]
        end
        include T
        include Comparable.Make_plain(T)
      end
      module Lifted = struct
        module T = struct
          type t = (Record_label.t * Entry.t Type_instance.General.t) list 
          [@@deriving compare,sexp]
        end
        include T
        include Comparable.Make_plain(T)
      end
    end in
    let lift ~key record =
      let lift type_instance =
        Type_instance.General.map type_instance
          ~f:(fun id ->
              match id with
              | Type_id.External ext -> `External ext
              | Defined def ->
                let description = All_types.defined_exn all_types def in
                if Language_name.equal description.language key
                then `Instance_in_lang description.name
                else `Defined def
            )
      in
      List.map record ~f:(Tuple2.map_snd ~f:lift)
    in
    let mergables = find_mergables records_by_language ~lift (module Lifted) in
    let merged_by_language =
      Map.fold mergables ~init:Language_name.Map.empty
        ~f:(fun ~key ~data acc ->
            let shared_id = Shared_type_id.create () in
            List.fold ~init:acc data
              ~f:(fun acc language_name ->
                  Map.add_exn acc ~key:language_name ~data:(shared_id, key)
                )
          )
    in
    merged_by_language


  let share_types
      (t : t)
    : With_shared.t
    =
    let { languages
        ; external_types
        } = t
    in
    let records_by_defined_type = records_by_defined_type t in
    let all_types = all_types t in
    let shared =
      Map.map records_by_defined_type ~f:(find_shared_records ~all_types)
    in
    let languages =
      Map.map languages
        ~f:(fun language_definition ->
            let types_by_id =
              Map.map language_definition.types_by_id
                ~f:Type_shape.With_shared.of_general
            in
            let types_by_id =
              Map.fold
                ~init:types_by_id
                language_definition.types_by_name
                ~f:(fun ~key ~data:defined_type_id acc ->
                    match Map.find shared key with
                    | None -> acc
                    | Some by_language ->
                      match Map.find by_language language_definition.name with
                      | None -> acc
                      | Some (shared_id, _) ->
                        Map.set acc ~key:defined_type_id ~data:(Shared shared_id);
                  )
            in
            { Language_definition.With_shared.
              types_by_name = language_definition.types_by_name
            ; name = language_definition.name
            ; types_by_id
            }
          )
    in
    let shared_types =
      Map.fold shared ~init:Shared_type_id.Map.empty
        ~f:(fun ~key:defined_type_name ~data:by_language acc ->
            Map.fold by_language ~init:acc
              ~f:(fun ~key:_language ~data:(shared_type_id, record) acc ->
                  let type_instance_parameters = ref Defined_type_name.Set.empty in
                  let shape : _ Type_shape.General.t =
                    List.map record
                      ~f:(fun (label, contents) ->
                          label
                        , Type_instance.General.map contents
                            ~f:(function
                                | `Defined id -> Type_id.With_shared_instance.Defined id
                                | `External id -> External id
                                | `Instance_in_lang name ->
                                  type_instance_parameters :=
                                    Set.add !type_instance_parameters name;
                                  Shared_instance name
                              )
                        )
                    |> Type_shape.General.Record
                  in
                  let type_instance_parameters =
                    Set.to_list !type_instance_parameters
                    |> List.mapi ~f:(fun i p -> p, i)
                    |> Defined_type_name.Map.of_alist_exn
                  in
                  let type_description : Defined_type_description.Shared.t =
                    { id = shared_type_id
                    ; name = defined_type_name
                    ; shape
                    ; type_instance_parameters
                    }
                  in
                  Map.update acc shared_type_id
                    ~f:(function
                        | None -> type_description
                        | Some _ -> type_description
                      )
                )
          )
    in
    { languages
    ; external_types
    ; shared_types
    }

end

module Synthesize = struct

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
    Map.iteri t.shared_types
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

  let synth t ~mappers:_ =
    let t = Language_group.share_types t in
    List.concat
     [ [ types_module t ]
     ; nonrec_module t
     ]
end

