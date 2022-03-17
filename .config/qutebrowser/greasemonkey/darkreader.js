// ==UserScript==
// @name          Dark Reader (Unofficial)
// @icon          https://darkreader.org/images/darkreader-icon-256x256.png
// @namespace     DarkReader
// @description	  Inverts the brightness of pages to reduce eye strain
// @version       4.7.15
// @author        https://github.com/darkreader/darkreader#contributors
// @homepageURL   https://darkreader.org/ | https://github.com/darkreader/darkreader
// @run-at        document-end
// @grant         none
// @include       http*
// @exclude       https://discord.com/*
// @exclude       https://app.element.io/*
// @exclude       https://*duckduckgo.com/*
// @exclude       http://127.0.0.1:*
// @require       https://cdn.jsdelivr.net/npm/darkreader/darkreader.min.js
// @noframes
// ==/UserScript==

DarkReader.enable({
	brightness: 100,
	contrast: 100,
	sepia: 0,
    darkSchemeBackgroundColor: "#002b36",
    darkSchemeTextColor: "#93a1a1",
    // darkColorScheme: "Solarized",
    lightSchemeBackgroundColor: "#eee8d5",
    lightSchemeTextColor: "#586e75",
    // lightColorScheme: "Solarized"
});
