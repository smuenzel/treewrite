open! Core
open! Ppxlib

module Longident = struct
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
