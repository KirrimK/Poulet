main.exe : backtrack.cmo main.cmo
		ocamlc -o main.exe  main.cmo backtrack.cmo

%.cmi : %.mli
		ocamlc $<
