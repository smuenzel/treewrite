open! Core
open! Util

module Defined_type_id : sig
  include Unique_id.Id
end

module External_type_id : sig
  include Unique_id.Id
end

module Shared_type_id : sig
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

  val constructor_module : t
end

module Constructor_name : sig
  include String_id.S

  val variant_type : t -> Parsetree.core_type
end

module Language_name : sig
  include String_id.S

  val to_mapper_module_name : t -> t -> Module_name.t
  val to_module_name : t -> Module_name.t
  val to_variable_name : t -> string
end

module Defined_type_name : sig
  include Comparable.S_plain

  val ident_in_language
    : ?t_name:string
    -> ?prefix:Longident.t
    -> t
    -> language_name:Language_name.t
    -> Longident.t

  val of_module_path :  Module_name.t list -> t

  val to_module_path : t -> Module_name.t list

  val to_shared_module_path :  t -> Shared_type_id.t -> Module_name.t list

  val to_fun_name : ?constructor:Constructor_name.t -> t -> string
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

  module With_shared : sig
    type t
  end
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

  val all_external_types : t -> External_type_id.Set.t

  module With_shared : sig
    type 'instance t =
      | Record of (Record_label.t * 'instance) list
      | Variant of (Constructor_name.t * 'instance list) list
      | Shared of Shared_type_id.t
    [@@deriving sexp]
  end
end


module Defined_type_description : sig
  module Shared : sig
    type t =
      { id : Shared_type_id.t
      ; name : Defined_type_name.t
      ; shape : Type_instance.With_shared.t Type_shape.General.t
      ; type_instance_parameters : int Defined_type_name.Map.t
      } [@@deriving sexp]
  end
end


module Language_definition : sig
  type t =
    { name : Language_name.t
    ; types_by_name : Defined_type_id.t Defined_type_name.Map.t
    ; types_by_id : Type_shape.t Defined_type_id.Map.t
    } [@@deriving sexp, fields]

  module With_shared : sig
    type t =
      { name : Language_name.t
      ; types_by_name : Defined_type_id.t Defined_type_name.Map.t
      ; types_by_id : Type_instance.t Type_shape.With_shared.t Defined_type_id.Map.t
      } [@@deriving sexp, fields]

    val all_external_types : t -> External_type_id.Set.t
  end
end

module All_types : sig
  type t

  module With_shared : sig
    type t

    val shared_to_type
      : ?prefix:Longident.t
      -> t
      -> Parsetree.core_type Defined_type_name.Map.t
      -> Type_instance.With_shared.t
      -> Parsetree.core_type

    val to_type
      : ?prefix:Longident.t
      -> t
      -> Type_instance.t
      -> Parsetree.core_type

    val instantiate_shared_type
      : ?prefix:Longident.t -> t -> Shared_type_id.t -> Language_name.t -> Parsetree.core_type

    val instantiate_defined_type
      : ?prefix:Longident.t -> t -> Defined_type_id.t -> Parsetree.core_type
  end
end

module Language_group : sig
  type t =
    { languages : Language_definition.t Language_name.Map.t
    ; external_types : External_type_descriptor.t External_type_id.Map.t
    }

  val merge : t -> t -> t

  val all_types : t -> All_types.t

  module With_shared : sig
    type t =
      { languages : Language_definition.With_shared.t Language_name.Map.t
      ; external_types : External_type_descriptor.t External_type_id.Map.t
      ; shared_types : Defined_type_description.Shared.t Shared_type_id.Map.t
      }

    val all_types : t -> All_types.With_shared.t
    val shared_types : t -> Defined_type_description.Shared.t Shared_type_id.Map.t

    val constructors_by_defined_type : t -> Type_instance.t list Language_name.Map.t Constructor_name.Map.t Defined_type_name.Map.t
    val records_by_defined_type : t -> (Record_label.t * Type_instance.t) list Language_name.Map.t Defined_type_name.Map.t
    val shared_by_defined_type : t -> Shared_type_id.t Language_name.Map.t Defined_type_name.Map.t
  end

  val constructor_type
    : ?prefix:Longident.t
    -> All_types.With_shared.t
    -> Type_instance.t list Language_name.Map.t Constructor_name.Map.t
    -> Parsetree.type_declaration

  val share_types : t -> With_shared.t
end

module Hierarchical_module : module type of Hierarchical.Make(Module_name)
