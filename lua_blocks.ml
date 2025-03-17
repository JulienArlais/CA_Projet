open Lua_header

type constant = Nil | Boolean of bool | Number of float | String of string
type local = {
  name  : string;
  start0 : int;
  end0   : int;
}

(* Définir un type instruction *)
type instruction =
  | LOADK of int * int  
  | ADD of int * int * int  
  | PRINT of int  


type function_block = {
  source_name         : string;
  line_defined        : int;
  last_line_defined   : int;
  nb_upvalues         : int;
  nb_parameters       : int;
  is_varag_flag       : int;
  max_stack_size      : int;
  instruction_list    : instruction list;
  constants_list      : constant list;
  prototypes_list     : function_block list;
  line_positions_list : int list;
  locals_list         : local list;
  upvalues_list       : string list;
}


let parse_instructions bytecode index num_instructions =
  let rec aux index acc remaining =
    if remaining = 0 then
      (List.rev acc, index)
    else
      let instr, index = get_int32 index bytecode in
      aux index (instr :: acc) (remaining - 1)
  in
  aux index [] num_instructions

let parse_constants bytecode index num_constants =
  let rec aux index acc remaining =
    if remaining = 0 then
      (List.rev acc, index)
    else
      let type_const, index = get_byte index bytecode in
      let const, index = match type_const with
        | 0 -> (Nil, index)  (* Type nil *)
        | 1 -> let b, index = get_byte index bytecode in (Boolean (b <> 0), index) 
        | 3 -> let n, index = get_int32 index bytecode in (Number (Int32.to_float n), index)
        | 4 -> let len, index = get_int32 index bytecode in
                let s, index = get_string index bytecode len in (String s, index)  
        | _ -> failwith "Unknown constant type"
      in
      aux index (const :: acc) (remaining - 1)
  in
  aux index [] num_constants

let parse_locals bytecode index num_locals =
  let rec aux index acc remaining =
    if remaining = 0 then (List.rev acc, index)
    else
      let name_len, index = get_int32 index bytecode in
      let name, index = get_string index bytecode name_len in
      let start_pc, index = get_int32 index bytecode in
      let end_pc, index = get_int32 index bytecode in
      aux index ({ name; start_pc; end_pc } :: acc) (remaining - 1)
  in
  aux index [] num_locals
  
(* let rec parse_prototypes bytecode index num_prototypes =
  let rec aux index acc remaining =
    if remaining = 0 then
      (List.rev acc, index)
    else
      let proto, index = decode_function_block bytecode index in
      aux index (proto :: acc) (remaining - 1)
  in
  aux index [] num_prototypes

and decode_function_block bytecode index =
  let source_name_byte, index = get_byte index bytecode in
  let source_name,      index =
    if source_name_byte = 0 then
      ("", index)
    else
      get_string index bytecode source_name_byte in

  let line_defined,      index = get_int32 index bytecode in
  let last_line_defined, index = get_int32 index bytecode in

  let nb_upvalues,   index = get_byte index bytecode in
  let nb_parameters, index = get_byte index bytecode in
  let is_varag_flag,     index = get_int32 index bytecode in
  let max_stack_size,    index = get_byte index bytecode in


  let num_instructions, index = get_int32 index bytecode in
  let instructions, index = parse_instructions bytecode index num_instructions in

  let num_constants, index = get_int32 index bytecode in
  let constants, index = parse_constants bytecode index num_constants in

  let num_prototypes, index = get_int32 index bytecode in
  let prototypes, index = parse_prototypes bytecode index num_prototypes in

  let num_line_positions, index = get_int32 index bytecode in
  let line_positions, index = parse_line_positions bytecode index num_line_positions in

  let num_locals, index = get_int32 index bytecode in
  let locals, index = parse_locals bytecode index num_locals in

  let num_upvalues, index = get_int32 index bytecode in
  let upvalues, index = parse_upvalues bytecode index num_upvalues in

  {
    source_name;
    line_defined;
    last_line_defined;
    nb_upvalues;
    nb_parameters;
    is_varag_flag;
    max_stack_size;
    instruction_list = instructions;
    constants_list = constants;
    prototypes_list = prototypes;
    line_positions_list = line_positions;
    locals_list = locals;
    upvalues_list = upvalues;
  } *)

let rec decode_function_block bytecode index =
  let source_name_len, index = get_int32 index bytecode in
  let source_name, index =
    if source_name_len = 0 then ("", index) else get_string index bytecode source_name_len
  in
  let line_defined, index = get_int32 index bytecode in
  let last_line_defined, index = get_int32 index bytecode in
  let nb_upvalues, index = get_byte index bytecode in
  let nb_parameters, index = get_byte index bytecode in
  let is_vararg_flag, index = get_byte index bytecode in
  let max_stack_size, index = get_byte index bytecode in
  let num_instructions, index = get_int32 index bytecode in
  let instructions, index = parse_instructions bytecode index num_instructions in
  let num_constants, index = get_int32 index bytecode in
  let constants, index = parse_constants bytecode index num_constants in
  let num_locals, index = get_int32 index bytecode in
  let locals, index = parse_locals bytecode index num_locals in
  {
    source_name;
    line_defined;
    last_line_defined;
    nb_upvalues;
    nb_parameters;
    is_vararg_flag;
    max_stack_size;
    instruction_list = instructions;
    constants_list = constants;
    prototypes_list = [];
    line_positions_list = [];
    locals_list = locals;
    upvalues_list = [];
  }, index

let main nom_fichier =
let ic = open_in_bin nom_fichier in
let file_size = in_channel_length ic in
let bytecode = Bytes.create file_size in
really_input ic bytecode 0 file_size;
close_in ic;

let header, index = decode_header bytecode in
let function_block, index = decode_function_block bytecode index in

Printf.printf "Source name: %s\n" function_block.source_name;
Printf.printf "Line defined: %d\n" function_block.line_defined;
Printf.printf "Last line defined: %d\n" function_block.last_line_defined;
Printf.printf "Number of upvalues: %d\n" function_block.nb_upvalues;
Printf.printf "Number of parameters: %d\n" function_block.nb_parameters;
Printf.printf "Is vararg: %d\n" function_block.is_vararg_flag;
Printf.printf "Max stack size: %d\n" function_block.max_stack_size;

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <fichier.luac>\n" Sys.argv.(0)
  else
    main Sys.argv.(1)