open! Core

let (&&&) = Bool.Non_short_circuiting.(&&)

module Memberships = struct
  type 'a lsrc = < lsrc : 'a >
  type 'a l1 = < l1 : 'a >
end

module Symbol = Unit

module Linstance = struct
  type ('a, 'b) t = 'a * 'b
end

module rec T : sig

  module Constructors : sig
    type (_,_) t =
      | Symbol :
          (< lsrc : Symbol.t
           ; l1 : Symbol.t
           >, [`Symbol]) t
      | If_1 :
          (< lsrc : If_1.Lsrc.t
           >, [`If_1]) t
      | If_2 :
          (< lsrc : If_2.Lsrc.t
           ; l1 : If_2.L1.t
           >, [`If_2]) t
  end

  module Lsrc : sig
    type 'b named =
      | T : ((< lsrc : 'a; ..>, 'b) Constructors.t
            , 'a) Linstance.t
          -> 'b named
    [@@unboxed]

    type t =
      | T : ((< lsrc : 'a; ..>, 'b) Constructors.t
            , 'a) Linstance.t
          -> t
    [@@unboxed]
  end

  module L1 : sig
    type t =
      | T : ((< l1 : 'a; ..>, 'b) Constructors.t
            , 'a) Linstance.t
          -> t
    [@@unboxed]
  end
end = T

and If_1 : sig
  module Lsrc : sig
    type t =
      { cond : T.Lsrc.t
      ; ifso : T.Lsrc.t
      }
  end

end = If_1

and If_2 : sig
  module Shared_0 : sig
    type 't t =
      { cond : 't
      ; ifso : 't
      ; ifnot : 't
      }
  end

  module Lsrc : sig
    type t = T.Lsrc.t Shared_0.t
  end

  module L1 : sig
    type t = T.L1.t Shared_0.t
  end

end = If_2

let maybe_cons
    (t : _ If_2.Shared_0.t)
    ~cond
    ~ifso
    ~ifnot
  : _ If_2.Shared_0.t
  =
  if (phys_same t.cond cond)
  &&& (phys_same t.ifso ifso)
  &&& (phys_same t.ifnot ifnot)
  then Obj.magic t
  else { cond; ifso; ifnot }

let ss : T.Lsrc.t =
  T (Symbol, ())

let rec lsrc_to_l1 (T lsrc : T.Lsrc.t) : T.L1.t =
  match lsrc with
  | (Symbol, _) -> T lsrc
  | (If_2, if_2) ->
    let cond = lsrc_to_l1 if_2.cond in
    let ifso = lsrc_to_l1 if_2.ifso in
    let ifnot = lsrc_to_l1 if_2.ifnot in
    let if_2' = maybe_cons if_2 ~cond ~ifso ~ifnot in
    if phys_same if_2 if_2'
    then T (Obj.magic lsrc)
    else T.L1.T (If_2, if_2')
  | (_, _) -> assert false

let [@inline always] call self meth = (meth self) self

let [@inline always] (#.) self meth = call self meth

module Lsrc_to_l1 = struct
  type t =
    { symbol : t -> Symbol.t -> Symbol.t
    ; t'Symbol : t -> [`Symbol] T.Lsrc.named -> T.L1.t
    } [@@deriving fields]

  let default : t =
    { symbol = (fun _ s -> s)
    ; t'Symbol = (fun self (T ((Symbol, s) as inst)) ->
          let s' = self#.symbol s in
          if phys_same s s'
          then T.L1.T inst
          else T.L1.T (Symbol, s')
        )
    }

  let xxx s = default#.t'Symbol s
end
