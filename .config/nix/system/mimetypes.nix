{ ... }:

let
  editor = "nvim.desktop";
  wordProcs = [ "writer.desktop" ];
  imageViewers = [ "org.gnome.Loupe.desktop" "gimp.desktop" ];
in
{
  xdg.mime = {
    enable = true;
    defaultApplications = {

      "application/pdf" = [ "org.gnome.Evince.desktop" "zathura.desktop" ];

      "inode/directory" = [ "org.gnome.Nautilus.desktop" ];

      "text/plain" = [ "${editor}" ];
      "text/markdown" = [ "${editor}" ];
      "text/x-lua" = [ "${editor}" ];
      "text/css" = [ "${editor}" ];
      "text/x-matlab" = [ "${editor}" ];
      "text/x-csrc" = [ "${editor}" ];
      "text/x-makefile" = [ "${editor}" ];
      "application/x-shellscript" = [ "${editor}" ];
      "application/javascript" = [ "${editor}" ];
      "application/x-yaml" = [ "${editor}" ];

      "application/octet-stream" = wordProcs;
      "application/zip" = wordProcs;

      "image/svg+xml" = [ "inkscape.desktop" ];
      "image/x-aseprite" = [ "aseprite.desktop" "libresprite.desktop" ];
      "image/jpeg" = imageViewers;
      "image/png" = imageViewers;
    };
    removedAssociations = {
      "application/pdf" = [ "calibre-ebook-viewer.desktop" "firefox.desktop" ];
    };
  };
}
