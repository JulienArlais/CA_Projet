type header = {
  signature       : string ;
  vm_version      : int;
  bytecode_format : int;
  big_endian      : bool;
  int_size        : int;
  size_t          : int;
  instr_size      : int;
  l_number_size   : int;
  integral_flag   : int;
}

let get_byte index bytecode = 
  let b = Bytes.get_uint8 bytecode index in
  (b, index + 1)

let get_string index bytecode size =
  let s  = Byte.sub_string bytecode index size in
  (s, index + 1)

let decode_header bytecode = 
  let index = 0 in

  (* On lit la signature, ESC + "Lua", 1 octet + 3 octet *)
  let signature_byte, index = get_byte index bytecode in
  let signature     , index = get_string index bytecode 3 in

  
  
