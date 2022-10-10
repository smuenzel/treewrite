open! Core

let ocamlformat kind string =
  let kind = 
    match kind with
    | `Impl -> "--impl"
    | `Intf -> "--intf"
  in
  let process =
    Core_unix.create_process
      ~prog:"ocamlformat"
      ~args:[ "-p"; "janestreet"
            ; "--ocp-indent-compat"
            ; kind
            ; "--enable-outside-detected-project"
            ; "-" ]
  in
  let c = Core_unix.out_channel_of_descr process.stdin in
  Out_channel.output_string c string;
  Out_channel.close c;
  In_channel.input_all (Core_unix.in_channel_of_descr process.stdout)

let code_buffer = lazy begin Buffer.create 16384 end
let code_formatter = lazy begin Format.formatter_of_buffer (force code_buffer) end

let with_buffer kind f =
  let lazy code_buffer = code_buffer in
  let lazy code_formatter = code_formatter in
  Buffer.reset code_buffer;
  f code_formatter;
  Format.pp_print_flush code_formatter ();
  Buffer.contents code_buffer
  |> ocamlformat kind

let with_buffer1 kind f x = with_buffer kind (fun format -> f format x)

let signature = with_buffer1 `Intf Pprintast.signature
let signature_item = with_buffer1 `Intf Pprintast.signature_item
let structure = with_buffer1 `Impl Pprintast.structure
let structure_item = with_buffer1 `Impl Pprintast.structure_item
  
