{ stdenv
, fetchgit
, curl
, expat
, gumbo
, ncurses
, sqlite
, yajl
, pkg-config
, scdoc
}:

stdenv.mkDerivation rec {
  pname = "newsraft";
  version = "0.21";

  src = fetchgit {
    url = "https://codeberg.org/grisha/newsraft.git";
    rev = "newsraft-${version}";
    sha256 = "sha256-vnLlozzPIk3F2U2ZvOClHnpmkXx4fc0pM1X4hFXM2Pg=";
  };

  nativeBuildInputs = [ pkg-config scdoc ]; # build-time dependencies

  buildInputs = [
    # runtime dependencies
    curl
    expat
    gumbo
    ncurses
    sqlite
    yajl
  ];

  makeFlags = [ "PREFIX=$(out)" ]; # this assumes the Makefile supports PREFIX

  installPhase = ''
    install -Dm755 newsraft $out/bin/newsraft
    make install-man PREFIX=$out
  '';
}
