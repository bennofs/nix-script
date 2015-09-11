{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "nix-script";
  src = ./nix-script.hs;
  phases = [ "buildPhase" "installPhase" "fixupPhase" ];
  buildPhase = ''mkdir -p $out/bin; ghc -O2 $src -o $out/bin/nix-script -odir $TMP'';
  installPhase = ''ln -s $out/bin/nix-script $out/bin/nix-scripti'';
  buildInputs = [ (pkgs.haskellPackages.ghcWithPackages (hs: with hs; [posix-escape])) ];
  meta = {
    homepage = https://github.com/bennofs/nix-script;
    description = "A shebang for running inside nix-shell.";
    license = pkgs.lib.licenses.bsd3;
    maintainers = [ pkgs.lib.maintainers.bennofs ];
    platforms = pkgs.haskellPackages.ghc.meta.platforms;
  };
}
