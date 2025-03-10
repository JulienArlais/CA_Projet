type Cons = bool of c | Number of c | String of c
type constant = {
  type_const  : int;
  const       : Cons;
}
type local = {
  name  : string;
  start : int;
  end   : int;
}

(* Définir un type instruction *)

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

let get_byte index bytecode =
  let b = Bytes.get_uint8 bytecode index in
  (b, index + 1)

let get_int index bytecode =
  let b1, index = get_byte index bytecode in
  let b2, index = get_byte index bytecode in
  let b3, index = get_byte index bytecode in
  let b4, index = get_byte index bytecode in
  (* On met les bytes bout à bout pour former l'entier *)
  let result = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in
    (result, index)

let get_string index bytecode size =
  let s  = Byte.sub_string bytecode index size in
  (s, index + 1)

let decode_function_block bytecode index =
  let source_name_byte, index = get_byte index bytecode in
  let source_name,      index = get_string index bytecode 100 in (* complètement arbitraire *)

  let line_defined,      index = get_int index bytecode in
  let last_line_defined, index = get_int index bytecode in

  let nb_upvalues,   index = get_byte index bytecode in
  let nb_parameters, index = get_byte index bytecode in
  let is_varag_flag,     index = get_int index bytecode in
  let max_stack_size,    index = get_byte index bytecode in

  (* Ajouter les listes *)

  {
    source_name;
    line_defined;
    last_line_defined;
    nb_upvalues;
    nb_parameters;
    is_varag_flag;
    max_stack_size;
    instruction_list;
    constants_list;
    prototypes_list;
    line_positions_list;
    locals_list;
    upvalues_list;
  }