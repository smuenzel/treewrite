open! Core
open! Util

module Defined_type_id : sig
  include Unique_id.Id
end

module External_type_id : sig
  include Unique_id.Id
end

module Type_id : sig
  type t =
    | External of External_type_id.t
    | Defined of Defined_type_id.t
  [@@deriving sexp]
end

module Record_label : sig
  include String_id.S
end

module Module_name : sig
  include String_id.S
end

module Constructor_name : sig
  include String_id.S
end

module Language_name : sig
  include String_id.S
end

module Defined_type_name : sig
  include Comparable.S_plain

  val ident_in_language
    : ?prefix:Longident.t
    -> t
    -> language_name:Language_name.t
    -> Longident.t

  val of_module_path
    :  Module_name.t list
    -> t
end

module External_type_descriptor : sig
  type t =
    { parameter_count : int
    ; fully_qualified_name : Longident.t
    }

  include Comparable.S_plain with type t := t
end

module Type_instance : sig
  module General : sig
    type 'id t =
      { id : 'id
      ; parameters : 'id t list
      } [@@deriving sexp, compare]
  end

  type t = Type_id.t General.t [@@deriving sexp]
end

module Type_shape : sig
  module General : sig
    type 'instance t =
      | Record of (Record_label.t * 'instance) list
      | Variant of (Constructor_name.t * 'instance list) list
    [@@deriving sexp]
  end

  type t = Type_instance.t General.t
  [@@deriving sexp]
end

module Language_definition : sig
  type t =
    { name : Language_name.t
    ; types_by_name : Defined_type_id.t Defined_type_name.Map.t
    ; types_by_id : Type_shape.t Defined_type_id.Map.t
    } [@@deriving sexp, fields]
end

module Language_group : sig
  type t =
    { languages : Language_definition.t Language_name.Map.t
    ; external_types : External_type_descriptor.t External_type_id.Map.t
    }

  val merge : t -> t -> t
end

module Synthesize : sig

  val synth : Language_group.t -> Parsetree.structure_item
end
