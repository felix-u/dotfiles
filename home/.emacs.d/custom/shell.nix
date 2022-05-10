with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "emacsenv";
  nativeBuildInputs = [ cmake ];
  buildInputs = [ libvterm-neovim ];
}
