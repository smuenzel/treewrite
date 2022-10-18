open! Core
open! Ppxlib

module List = struct
  include List

  let fold_map2_exn t1 t2 ~init ~f =
    let acc = ref init in
    let result =
      map2_exn t1 t2
        ~f:(fun v1 v2 ->
            let new_acc, res = f !acc v1 v2 in
            acc := new_acc;
            res
          )
    in
    !acc, result

end

module Longident = struct
  module T = struct
    include Ppxlib.Longident

    let sexp_of_t t =
      flatten_exn t
      |> [%sexp_of: string list]

    let rec of_list_rev = function
      | [] -> assert false
      | [ x ] -> Lident x
      | x :: xs -> Ldot (of_list_rev xs, x)

    let of_list l = of_list_rev (List.rev l)

    let t_of_sexp sexp =
      [%of_sexp: string list] sexp
      |> of_list

    let dot (a : t) (b : t) =
      (flatten_exn a) @ (flatten_exn b)
      |> of_list
  end
  include T
  include Comparable.Make_plain(T)
end

module Ast_builder = struct
  include Ast_builder.Make(struct
    let loc = Location.none
  end)

  module Loc = struct
    include Loc
    let make x = make ~loc:Location.none x
  end

  let ptyp_constr name = ptyp_constr (Loc.make name)
  let wrap_name f ~name = f ~name:(Loc.make name)
  let label_declaration = wrap_name label_declaration
  let type_declaration = wrap_name type_declaration
  let module_declaration = wrap_name module_declaration
  let constructor_declaration = wrap_name constructor_declaration
  let module_binding = wrap_name module_binding
  let pmod_ident name = pmod_ident (Loc.make name)
  let class_infos ~name = class_infos ~name:(Loc.make name)
  let attribute = wrap_name attribute
  let otag name = otag (Loc.make name)

  let ppat_construct name = ppat_construct (Loc.make name)
  let ppat_var name = ppat_var (Loc.make name)
  let pexp_construct name = pexp_construct (Loc.make name)

  let pcf_method ?(private_=Public) name class_field_kind =
    pcf_method ((Loc.make name), private_, class_field_kind)

  let pexp_ident i = pexp_ident (Loc.make i)

  let rtag t = rtag (Loc.make t)

  let type_declaration
      ?(params=[]) ?(cstrs=[]) ?(private_=Public) ?manifest ?(kind=Ptype_abstract) name
    =
    type_declaration ~name ~params ~cstrs ~private_ ~manifest ~kind

  let label_declaration ?(mutable_=Immutable) ~type_ name =
    label_declaration ~name ~mutable_ ~type_

  let constructor_declaration ?res ~args name =
    constructor_declaration  ~name ~res ~args

  let rec ptyp_arrows = function
    | [] -> assert false
    | [ x ] -> x
    | x :: rest -> ptyp_arrow Nolabel x (ptyp_arrows rest)
end

let map_to_list_rev ~f map =
  Map.fold map
    ~init:[]
    ~f:(fun ~key ~data acc ->
        f ~key ~data :: acc
      )

let map_to_list ~f map = List.rev (map_to_list_rev ~f map)

let find_mergables
    (type k v lifted comparator_witness)
    (map : (k, v, _) Map.t)
    ~lift
    (module Lifted : Comparable.S_plain 
      with type t = lifted
       and type comparator_witness = comparator_witness)
  =
  Map.fold map
    ~init:Lifted.Map.empty
    ~f:(fun ~key ~data acc ->
        Map.add_multi acc ~key:(lift ~key data) ~data:key
      )
  |> Map.filter
    ~f:(function
        | [] | [ _ ] -> false
        | _ -> true
      )

(* Quadratic, could reduce to n log n, by lifting all elements, and adding an ordering
   to the lifted data structure. *)
let classify_mergables
    (type k v merged_v)
    (map : (k, v, _) Map.t)
    ~lift
    ~can_merge
  : ((k, _) Set.t * merged_v) list * (k, v, _) Map.t =
  let alist = Map.to_alist map in
  let comparator = Map.comparator map in
  let rec merge_first ~cannot_merge ~already_merged list =
    match list with
    | [] ->
      already_merged, Map.Using_comparator.of_alist_exn ~comparator cannot_merge
    | ((first_key, first) as first_elt) :: rest ->
      let first_merge = lift ~key:first_key first in
      let merged, unmerged = 
        List.partition_map rest
          ~f:(fun ((key, data) as elt) ->
              if can_merge first_merge data
              then First key
              else Second elt
            )
      in
      let cannot_merge, already_merged =
        if List.is_empty merged
        then first_elt::cannot_merge, already_merged
        else cannot_merge,
             ((Set.Using_comparator.of_list ~comparator merged), first_merge)
             :: already_merged
      in
      merge_first ~cannot_merge ~already_merged rest
  in
  merge_first ~cannot_merge:[] ~already_merged:[] alist
