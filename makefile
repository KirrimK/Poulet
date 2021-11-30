main.exe : backtrack.cmo main.cmo
		ocamlc -o main.exe  main.cmo 

%.cmi : %.mli
		ocamlc $<

cli.cmo : hypothese.cmi cli.mli
		ocamlc -c cli.mli

hypothese.cmo : hypothese.cmi hypothese.ml
		ocamlc -c hypothese.ml

strategies.cmo : strategies.cmi hypothese.cmi strategies.ml
		ocamlc -c strategies.ml

backtrack.cmo : backtrack.cmi strategies.cmi backtrack.ml
		ocamlc -c backtrack.ml