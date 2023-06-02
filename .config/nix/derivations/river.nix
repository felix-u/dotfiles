{ lib
, stdenv
, fetchFromGitHub
, zig
, wayland
, pkg-config
, scdoc
, xwayland
, wayland-protocols
, wlroots_0_15
, libxkbcommon
, pixman
, pkgs
, udev
, libevdev
, libinput
, libGL
, libX11
, mesa
, xwaylandSupport ? true
}:

let 
mesa = pkgs.mesa.overrideAttrs (oldAttrs: rec { 
    galliumDrivers = oldAttrs.galliumDrivers ++ [ "zink" "radeonsi" "swrast" "i915" "iris" "auto" ];
    postInstall = oldAttrs.postInstall + ''
        ln -s -t $drivers/lib/ ${pkgs.vulkan-loader}/lib/lib*
    '';
});
in
stdenv.mkDerivation rec {
  pname = "river";
  version = "0.3.0-dev";

  src = fetchFromGitHub {
    owner = "riverwm";
    repo = pname;
    rev = "792d94253c191e653e4025a648d574d9f8ce99bf";
    sha256 = "sha256-4Gwi7PiITj6i41YnngecFWd/pt5UQwslOM71C7tUR4k=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ zig wayland xwayland scdoc pkg-config ];

  buildInputs = [
    wayland-protocols
    wlroots_0_15
    libxkbcommon
    mesa
    pixman
    udev
    libevdev
    libinput
    libGL
  ] ++ lib.optional xwaylandSupport libX11;

  dontConfigure = true;

  preBuild = ''
    export HOME=$TMPDIR
  '';

  installPhase = ''
    runHook preInstall
    zig build -Drelease-safe -Dcpu=baseline ${lib.optionalString xwaylandSupport "-Dxwayland"} -Dman-pages --prefix $out install
    install contrib/river.desktop -Dt $out/share/wayland-sessions
    runHook postInstall
  '';

  /* Builder patch install dir into river to get default config
    When installFlags is removed, river becomes half broken.
    See https://github.com/riverwm/river/blob/7ffa2f4b9e7abf7d152134f555373c2b63ccfc1d/river/main.zig#L56
  */
  installFlags = [ "DESTDIR=$(out)" ];

  passthru.providedSessions = ["river"];

  meta = with lib; {
    changelog = "https://github.com/ifreund/river/releases/tag/v${version}";
    homepage = "https://github.com/ifreund/river";
    description = "A dynamic tiling wayland compositor";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
    maintainers = with maintainers; [ fortuneteller2k adamcstephens rodrgz ];
  };
}
