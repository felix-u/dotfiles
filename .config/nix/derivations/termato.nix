{ stdenv
, fetchFromGitHub
, ncurses
}:

stdenv.mkDerivation rec {
  pname = "termato";
  version = "0.4";

  src = fetchFromGitHub {
    owner = "felix-u";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-+qu5rMlDQpBKoUhzDKa3fnQI/RlY+DN8FCESK8E1Urc=";
  };

  buildInputs = [ ncurses ];

  buildPhase = ''
    make release NAME=termato
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv ./termato $out/bin/
  '';
}
