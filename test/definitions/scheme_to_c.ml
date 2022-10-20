open! Core
open! Treewrite_lib

let lsrc =
  Parse.parse_language
    ~language_name:(Compile.Language_name.of_string "lsrc")
    [%sigi:
      type t =
        | Symbol of Symbol.t
        | Primitive of Primitive.t
        | Constant of Constant.t
        | Quote of Datum.t
        | If_1 of { cond : t; ifso : t }
        | If_2 of { cond : t; ifso : t; ifnot : t }
        | Or of t list
        | And of t list
        | Not of t
        | Begin of t Nonempty_list.t
        | Lambda of { params : Symbol.t Nonempty_list.t
                    ; body : t Nonempty_list.t
                    }
        | Let of
            { definition : var_definition Nonempty_list.t
            ; body : t Nonempty_list.t
            }
        | Letrec of
            { definition : var_definition Nonempty_list.t
            ; body : t Nonempty_list.t
            }
        | Setx of { symbol : Symbol.t
                  ; new_value : t
                  }
      and var_definition =
        { symbol : Symbol.t
        ; definition : t
        }
    ]

let l1 =
  Parse.parse_language
    ~language_name:(Compile.Language_name.of_string "l1")
    [%sigi:
      type t =
        | Symbol of Symbol.t
        | Primitive of Primitive.t
        | Constant of Constant.t
        | Quote of Datum.t
        | If_2 of { cond : t; ifso : t; ifnot : t }
        | Or of t list
        | And of t list
        | Not of t
        | Begin of t Nonempty_list.t
        | Lambda of { params : Symbol.t Nonempty_list.t
                    ; body : t Nonempty_list.t
                    }
        | Let of
            { definition : var_definition Nonempty_list.t
            ; body : t Nonempty_list.t
            }
        | Letrec of
            { definition : var_definition Nonempty_list.t
            ; body : t Nonempty_list.t
            }
        | Setx of { symbol : Symbol.t
                  ; new_value : t
                  }
      and var_definition =
        { symbol : Symbol.t
        ; definition : t
        }
    ]

let all =
  Compile.Language_group.merge lsrc l1

let mappers =
  [ "lsrc", "l1"
  ] |> List.map ~f:(Tuple2.map ~f:Compile.Language_name.of_string)
