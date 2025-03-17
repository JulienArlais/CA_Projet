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
  let s  = Bytes.sub_string bytecode index size in
  (s, index + size)

let get_int32 index bytecode =
  let b1, index = get_byte index bytecode in
  let b2, index = get_byte index bytecode in
  let b3, index = get_byte index bytecode in
  let b4, index = get_byte index bytecode in
  let result = Int32.logor (Int32.shift_left (Int32.of_int b1) 24)
                (Int32.logor (Int32.shift_left (Int32.of_int b2) 16)
                  (Int32.logor (Int32.shift_left (Int32.of_int b3) 8)
                    (Int32.of_int b4))) in
  (Int32.to_int result, index)

let decode_header bytecode = 
  let index = 0 in

  (* On lit la signature, ESC + "Lua", 1 octet + 3 octet *)
  let signature_byte  , index = get_byte index bytecode in
  let signature       , index = get_string index bytecode 3 in
  if signature_byte <> 0x1B || signature <> "Lua" then
    failwith "Invalid Lua bytecode signature"
  else
  
    let vm_version      , index = get_byte index bytecode in
    let bytecode_format , index = get_byte index bytecode in
    let big_endian      , index = get_byte index bytecode in 
    let size_t          , index = get_byte index bytecode in
    let instr_size      , index = get_byte index bytecode in
    let l_number_size   , index = get_byte index bytecode in
    let integral_flag   , index = get_byte index bytecode in

    {
      signature = "\027Lua";
      vm_version;
      bytecode_format;
      big_endian = (big_endian = 0);
      int_size;
      size_t;
      instr_size;
      l_number_size;
      integral_flag;
    }, index



(* problème : les entiers en OCaml sont sur 31bits et non 32 -> fonction spéciale à utiliser *)


let main nom_fichier =
  let ic = open_in_bin nom_fichier in
  let file_size = in_channel_length ic in
  let bytecode = Bytes.create file_size in
  really_input ic bytecode 0 file_size;
  close_in ic;

  let header = decode_header bytecode in

  Printf.printf "Signature: 0x1B %s\n" (String.sub header.signature 1 3);
  Printf.printf "Lua VM version: 0x%x\n" header.vm_version;
  Printf.printf "Bytecode format: %d\n" header.bytecode_format;
  Printf.printf "Big Endian: %b\n" header.big_endian;
  Printf.printf "int_size: %d\n" header.int_size;
  Printf.printf "size_t: %d\n" header.size_t;
  Printf.printf "Instruction size: %d\n" header.instr_size;
  Printf.printf "Lua number size: %d\n" header.l_number_size;
  Printf.printf "Integral flag: %d\n" header.integral_flag


let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <fichier.luac>\n" Sys.argv.(0)
  else
    main Sys.argv.(1)
