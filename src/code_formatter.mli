open! Core

val ocamlformat : [ `Impl | `Intf ] -> string -> string

val signature : Parsetree.signature -> string
val signature_item : Parsetree.signature_item -> string
val structure : Parsetree.structure -> string
val structure_item : Parsetree.structure_item -> string
