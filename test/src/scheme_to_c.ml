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
    Treewrite_test_definitions.Scheme_to_c.all
    |> Compile.Synthesize.synth
    |> List.return
    |> Code_formatter.structure
    |> print_endline
*)
module rec Types : sig
  module T : sig
    module Constructors : sig
      type (_, _) t =
        | Symbol : (< lsrc : Symbol.t ; l1 : Symbol.t >, [ `Symbol ]) t
        | Setx : (< lsrc : Types.T.Setx.Lsrc.t ; l1 : Types.T.Setx.L1.t >, [ `Setx ]) t
        | Quote : (< lsrc : Datum.t ; l1 : Datum.t >, [ `Quote ]) t
        | Primitive : (< lsrc : Primitive.t ; l1 : Primitive.t >, [ `Primitive ]) t
        | Or : (< lsrc : Types.T.Lsrc.t list ; l1 : Types.T.L1.t list >, [ `Or ]) t
        | Not : (< lsrc : Types.T.Lsrc.t ; l1 : Types.T.L1.t >, [ `Not ]) t
        | Letrec
            : (< lsrc : Types.T.Letrec.Lsrc.t ; l1 : Types.T.Letrec.L1.t >, [ `Letrec ]) t
        | Let : (< lsrc : Types.T.Let.Lsrc.t ; l1 : Types.T.Let.L1.t >, [ `Let ]) t
        | Lambda
            : (< lsrc : Types.T.Lambda.Lsrc.t ; l1 : Types.T.Lambda.L1.t >, [ `Lambda ]) t
        | If_2 : (< lsrc : Types.T.If_2.Lsrc.t ; l1 : Types.T.If_2.L1.t >, [ `If_2 ]) t
        | If_1 : (< lsrc : Types.T.If_1.Lsrc.t >, [ `If_1 ]) t
        | Constant : (< lsrc : Constant.t ; l1 : Constant.t >, [ `Constant ]) t
        | Begin
            : ( < lsrc : Types.T.Lsrc.t Nonempty_list.t
                ; l1 : Types.T.L1.t Nonempty_list.t >
              , [ `Begin ] )
              t
        | And : (< lsrc : Types.T.Lsrc.t list ; l1 : Types.T.L1.t list >, [ `And ]) t
    end

    module L1 : sig
      type 'tag named =
        | L : (< l1 : 'data ; .. >, 'tag) Constructors.t * 'data -> 'tag named

      type t = L : (< l1 : 'data ; .. >, 'tag) Constructors.t * 'data -> t
    end

    module Lsrc : sig
      type 'tag named =
        | L : (< lsrc : 'data ; .. >, 'tag) Constructors.t * 'data -> 'tag named

      type t = L : (< lsrc : 'data ; .. >, 'tag) Constructors.t * 'data -> t
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
      module L1 : sig
        type t =
          { cond : Types.T.L1.t
          ; ifso : Types.T.L1.t
          ; ifnot : Types.T.L1.t
          }
      end

      module Lsrc : sig
        type t =
          { cond : Types.T.Lsrc.t
          ; ifso : Types.T.Lsrc.t
          ; ifnot : Types.T.Lsrc.t
          }
      end
    end

    module Lambda : sig
      module L1 : sig
        type t =
          { params : Symbol.t Nonempty_list.t
          ; body : Types.T.L1.t Nonempty_list.t
          }
      end

      module Lsrc : sig
        type t =
          { params : Symbol.t Nonempty_list.t
          ; body : Types.T.Lsrc.t Nonempty_list.t
          }
      end
    end

    module Let : sig
      module L1 : sig
        type t =
          { definition : Types.Var_definition.L1.t Nonempty_list.t
          ; body : Types.T.L1.t Nonempty_list.t
          }
      end

      module Lsrc : sig
        type t =
          { definition : Types.Var_definition.Lsrc.t Nonempty_list.t
          ; body : Types.T.Lsrc.t Nonempty_list.t
          }
      end
    end

    module Letrec : sig
      module L1 : sig
        type t =
          { definition : Types.Var_definition.L1.t Nonempty_list.t
          ; body : Types.T.L1.t Nonempty_list.t
          }
      end

      module Lsrc : sig
        type t =
          { definition : Types.Var_definition.Lsrc.t Nonempty_list.t
          ; body : Types.T.Lsrc.t Nonempty_list.t
          }
      end
    end

    module Setx : sig
      module L1 : sig
        type t =
          { symbol : Symbol.t
          ; new_value : Types.T.L1.t
          }
      end

      module Lsrc : sig
        type t =
          { symbol : Symbol.t
          ; new_value : Types.T.Lsrc.t
          }
      end
    end
  end

  module Var_definition : sig
    module L1 : sig
      type t =
        { symbol : Symbol.t
        ; definition : Types.T.L1.t
        }
    end

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
