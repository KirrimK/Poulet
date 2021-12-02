main : backtrack.cmo main.cmo
		ocamlc -o poulet  main.cmo 

%.cmi : %.mli
		ocamlc $<

cli.cmo : hypothese.cmi cli.mli
		ocamlc -c cli.mli

hypothese.cmo : hypothese.cmi hypothese.ml
		ocamlc -c hypothese.ml

strategies.cmo : strategies.cmi hypothese.cmi strategies.ml
		ocamlc -c strategies.ml

backtrack.cmo : backtrack.cmi strategies.cmi backtrack.ml hypothese.cmi
		ocamlc -c backtrack.ml

main.cmo : main.ml backtrack.cmi
		ocamlc -c main.ml
