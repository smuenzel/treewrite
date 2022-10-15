open! Core

module Symbol = Unit
module Datum = Unit
module Primitive = Unit
module Constant = Unit

(*$
  open! Core
  open! Treewrite_lib

  let () = print_endline ""

  let () =
    Treewrite_test_definitions.Scheme_to_c.lsrc
    |> Compile.Synthesize.synth
    |> List.return
    |> Code_formatter.structure
    |> print_endline
*)
module rec Types : sig
  module T : sig
    module Constructors : sig
      type (_, _) t =
        | Symbol : (< lsrc : Symbol.t >, [ `Symbol ]) t
        | Setx : (< lsrc : Types.T.Setx.Lsrc.t >, [ `Setx ]) t
        | Quote : (< lsrc : Datum.t >, [ `Quote ]) t
        | Primitive : (< lsrc : Primitive.t >, [ `Primitive ]) t
        | Or : (< lsrc : Types.T.Lsrc.t list >, [ `Or ]) t
        | Not : (< lsrc : Types.T.Lsrc.t >, [ `Not ]) t
        | Letrec : (< lsrc : Types.T.Letrec.Lsrc.t >, [ `Letrec ]) t
        | Let : (< lsrc : Types.T.Let.Lsrc.t >, [ `Let ]) t
        | Lambda : (< lsrc : Types.T.Lambda.Lsrc.t >, [ `Lambda ]) t
        | If_2 : (< lsrc : Types.T.If_2.Lsrc.t >, [ `If_2 ]) t
        | If_1 : (< lsrc : Types.T.If_1.Lsrc.t >, [ `If_1 ]) t
        | Constant : (< lsrc : Constant.t >, [ `Constant ]) t
        | Begin : (< lsrc : Types.T.Lsrc.t Nonempty_list.t >, [ `Begin ]) t
        | And : (< lsrc : Types.T.Lsrc.t list >, [ `And ]) t
    end

    module Lsrc : sig
      type 'tag named =
        | T : (< lsrc : 'data ; .. >, 'tag) Constructors.t * 'data -> 'tag named

      type t = T : (< lsrc : 'data ; .. >, 'tag) Constructors.t * 'data -> t
    end

    module If_1 : sig
      module Lsrc : sig
        type t =
          { cond : Types.T.Lsrc.t
          ; ifso : Types.T.Lsrc.t
          }
      end
    end

    module If_2 : sig
      module Lsrc : sig
        type t =
          { cond : Types.T.Lsrc.t
          ; ifso : Types.T.Lsrc.t
          ; ifnot : Types.T.Lsrc.t
          }
      end
    end

    module Lambda : sig
      module Lsrc : sig
        type t =
          { params : Symbol.t Nonempty_list.t
          ; body : Types.T.Lsrc.t Nonempty_list.t
          }
      end
    end

    module Let : sig
      module Lsrc : sig
        type t =
          { definition : Types.Var_definition.Lsrc.t Nonempty_list.t
          ; body : Types.T.Lsrc.t Nonempty_list.t
          }
      end
    end

    module Letrec : sig
      module Lsrc : sig
        type t =
          { definition : Types.Var_definition.Lsrc.t Nonempty_list.t
          ; body : Types.T.Lsrc.t Nonempty_list.t
          }
      end
    end

    module Setx : sig
      module Lsrc : sig
        type t =
          { symbol : Symbol.t
          ; new_value : Types.T.Lsrc.t
          }
      end
    end
  end

  module Var_definition : sig
    module Lsrc : sig
      type t =
        { symbol : Symbol.t
        ; definition : Types.T.Lsrc.t
        }
    end
  end
end =
  Types

(*$*)
