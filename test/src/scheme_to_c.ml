open! Core

module Symbol = Unit
module Datum = Unit
module Primitive = Unit
module Constant = Unit

module Linstance = struct
  type ('a, 'b) t = 'a * 'b
end

(*$
  open! Core
  open! Treewrite_lib

  let () = print_endline ""

  let () =
    Treewrite_test_definitions.Scheme_to_c.all
    |> Compile.Synthesize.synth
      ~mappers:Treewrite_test_definitions.Scheme_to_c.mappers
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
        | L :
            ((< l1 : 'data ; .. >, 'tag) Constructors.t, 'data) Linstance.t
            -> 'tag named
      [@@ocaml.unboxed]

      type t = L : ((< l1 : 'data ; .. >, 'tag) Constructors.t, 'data) Linstance.t -> t
      [@@ocaml.unboxed]
    end

    module Lsrc : sig
      type 'tag named =
        | L :
            ((< lsrc : 'data ; .. >, 'tag) Constructors.t, 'data) Linstance.t
            -> 'tag named
      [@@ocaml.unboxed]

      type t =
        | L : ((< lsrc : 'data ; .. >, 'tag) Constructors.t, 'data) Linstance.t -> t
      [@@ocaml.unboxed]
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
        type t = Types.T.L1.t Types.T.If_2.Shared_0.t
      end

      module Lsrc : sig
        type t = Types.T.Lsrc.t Types.T.If_2.Shared_0.t
      end

      module Shared_0 : sig
        type 'v0 t =
          { cond : 'v0
          ; ifso : 'v0
          ; ifnot : 'v0
          }
      end
    end

    module Lambda : sig
      module L1 : sig
        type t = Types.T.L1.t Types.T.Lambda.Shared_1.t
      end

      module Lsrc : sig
        type t = Types.T.Lsrc.t Types.T.Lambda.Shared_1.t
      end

      module Shared_1 : sig
        type 'v0 t =
          { params : Symbol.t Nonempty_list.t
          ; body : 'v0 Nonempty_list.t
          }
      end
    end

    module Let : sig
      module L1 : sig
        type t = (Types.T.L1.t, Types.Var_definition.L1.t) Types.T.Let.Shared_2.t
      end

      module Lsrc : sig
        type t = (Types.T.Lsrc.t, Types.Var_definition.Lsrc.t) Types.T.Let.Shared_2.t
      end

      module Shared_2 : sig
        type ('v0, 'v1) t =
          { definition : 'v1 Nonempty_list.t
          ; body : 'v0 Nonempty_list.t
          }
      end
    end

    module Letrec : sig
      module L1 : sig
        type t = (Types.T.L1.t, Types.Var_definition.L1.t) Types.T.Letrec.Shared_3.t
      end

      module Lsrc : sig
        type t = (Types.T.Lsrc.t, Types.Var_definition.Lsrc.t) Types.T.Letrec.Shared_3.t
      end

      module Shared_3 : sig
        type ('v0, 'v1) t =
          { definition : 'v1 Nonempty_list.t
          ; body : 'v0 Nonempty_list.t
          }
      end
    end

    module Setx : sig
      module L1 : sig
        type t = Types.T.L1.t Types.T.Setx.Shared_4.t
      end

      module Lsrc : sig
        type t = Types.T.Lsrc.t Types.T.Setx.Shared_4.t
      end

      module Shared_4 : sig
        type 'v0 t =
          { symbol : Symbol.t
          ; new_value : 'v0
          }
      end
    end
  end

  module Var_definition : sig
    module L1 : sig
      type t = Types.T.L1.t Types.Var_definition.Shared_5.t
    end

    module Lsrc : sig
      type t = Types.T.Lsrc.t Types.Var_definition.Shared_5.t
    end

    module Shared_5 : sig
      type 'v0 t =
        { symbol : Symbol.t
        ; definition : 'v0
        }
    end
  end
end =
  Types

module T = struct
  module Constructors = struct
    include Types.T.Constructors
  end

  module L1 = struct
    include Types.T.L1
  end

  module Lsrc = struct
    include Types.T.Lsrc
  end

  module If_1 = struct
    module Lsrc = struct
      include Types.T.If_1.Lsrc
    end
  end

  module If_2 = struct
    module L1 = struct
      include Types.T.If_2.L1
    end

    module Lsrc = struct
      include Types.T.If_2.Lsrc
    end

    module Shared_0 = struct
      include Types.T.If_2.Shared_0
    end
  end

  module Lambda = struct
    module L1 = struct
      include Types.T.Lambda.L1
    end

    module Lsrc = struct
      include Types.T.Lambda.Lsrc
    end

    module Shared_1 = struct
      include Types.T.Lambda.Shared_1
    end
  end

  module Let = struct
    module L1 = struct
      include Types.T.Let.L1
    end

    module Lsrc = struct
      include Types.T.Let.Lsrc
    end

    module Shared_2 = struct
      include Types.T.Let.Shared_2
    end
  end

  module Letrec = struct
    module L1 = struct
      include Types.T.Letrec.L1
    end

    module Lsrc = struct
      include Types.T.Letrec.Lsrc
    end

    module Shared_3 = struct
      include Types.T.Letrec.Shared_3
    end
  end

  module Setx = struct
    module L1 = struct
      include Types.T.Setx.L1
    end

    module Lsrc = struct
      include Types.T.Setx.Lsrc
    end

    module Shared_4 = struct
      include Types.T.Setx.Shared_4
    end
  end
end

module Var_definition = struct
  module L1 = struct
    include Types.Var_definition.L1
  end

  module Lsrc = struct
    include Types.Var_definition.Lsrc
  end

  module Shared_5 = struct
    include Types.Var_definition.Shared_5
  end
end

module Map_Lsrc_L1 = struct
  type mapper =
    { t : mapper -> T.Lsrc.t -> T.L1.t
    ; t__if_2 : mapper -> T.If_2.Lsrc.t -> T.If_2.L1.t
    ; t__lambda : mapper -> T.Lambda.Lsrc.t -> T.Lambda.L1.t
    ; t__let : mapper -> T.Let.Lsrc.t -> T.Let.L1.t
    ; t__letrec : mapper -> T.Letrec.Lsrc.t -> T.Letrec.L1.t
    ; t__setx : mapper -> T.Setx.Lsrc.t -> T.Setx.L1.t
    ; var_definition : mapper -> Var_definition.Lsrc.t -> Var_definition.L1.t
    }
end

(*$*)
