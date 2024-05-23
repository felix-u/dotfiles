{ config }:

let
  theme = (import ../../system/theme.nix) { config = config; };
in
''
  /* greyscale tabbar icons */
  .tab-icon-image { filter: grayscale( 100%) !important;}

  /* Mostly from https://github.com/MrOtherGuy/firefox-csshacks */

  /* Set blank page background-color */
  #tabbrowser-tabpanels{
    background-color: #${theme.cbg} !important;
  }

  /* hide "PLAYING" text, which increases bar height */
  .tab-icon-sound-label {
      display: none !important;
  }

  /* Show tab number before tab text*/
  .tab-text::before{
      content: counter(nth-tab) " ";
      counter-increment: nth-tab;
      font-family: ${theme.fontsans} !important;
      font-weight: normal !important;
      color: #${theme.cfg};
      border-radius: 16px;
      padding-left: 8px;
      margin-right: 25px;
  }
  .tabbrowser-tab:first-child{ counter-reset: nth-tab 0 } 
  .tabbrowser-tab[selected] .tab-text::before{
      content: counter(nth-tab) " ";
      counter-increment: nth-tab;
      font-family: ${theme.fontsans} !important;
      font-weight: bold !important;
      color: #${theme.cfg}; 
      border-radius: 16px;
      padding-left: 8px;
      margin-right: 25px;
  }
''
