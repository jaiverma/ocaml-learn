all: async_file.out echo_server.out

uppercase_file.out:
	ocamlfind ocamlopt -o uppercase_file.out -linkpkg -package async -thread uppercase_file.ml

async_file.out:
	ocamlfind ocamlopt -o async_file.out -linkpkg -package async -thread async_file.ml

echo_server.out:
	ocamlfind ocamlopt -o echo_server.out -linkpkg -package async -thread echo_server.ml

clean:
	rm -f *.out *.cmi *.o *.cmx
