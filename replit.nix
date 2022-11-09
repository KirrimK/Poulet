{ pkgs }: {
    deps = [
        pkgs.ocaml
        pkgs.dune_2
        pkgs.ocamlPackages.menhir
    ];
}