{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "nix-bang";
  src = ./nix-bang.hs;
  phases = [ "buildPhase" "installPhase" "fixupPhase" ];
  buildPhase = ''mkdir $out; ghc -O2 $src -o $out/nix-bang -odir $TMP'';
  installPhase = ''ln -s $out/nix-bang $out/nix-bangi'';
  buildInputs = [ (pkgs.haskellPackages.ghcWithPackages (hs: with hs; [lens text])) ];
  meta = {
    homepage = https://github.com/bennofs/nix-bang;
    description = "A shebang for running inside nix-shell.";
    license = pkgs.lib.licenses.bsd3;
    maintainers = [ pkgs.lib.maintainers.bennofs ];
    platforms = pkgs.haskellPackages.ghc.meta.platforms;
  };
}