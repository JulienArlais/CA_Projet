# CA_Projet   
export PATH=$PATH:$PWD dans src et le dossier projet   

ocamlc -o lua_header lua_header.ml   
./lua_header simple-bytecode/luac.out   

ocamlc -o lua_blocks lua_header.ml lua_blocks.ml   
