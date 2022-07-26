// ==UserScript==
// @name          Discord theme
// @run-at        document-end
// @grant         none
// @include       https://discord.com/*
// @noframes
// ==/UserScript==

function GM_addStyle (cssStr) {
    var D               = document;
    var newNode         = D.createElement ('style');
    newNode.textContent = cssStr;

    var targ    = D.getElementsByTagName ('head')[0] || D.body || D.documentElement;
    targ.appendChild (newNode);
}

GM_addStyle ( `
:root {
    --fg: #bcbcdb;
    --bg: #222432;
    --black: #323140;
    --xcolor8: #4d4c5d;
    --red: #dc6f7a;
    --green: #7cbe8c;
    --orange: #c48a7f;
    --yellow: #a8a384;
    --blue: #589ec6;
    --magenta: #929be5;
    --cyan: #59b6b6;
    --grey: #767687;
    --xcolor15: #9ea3c0;

    --sans: ;
    --mono: ;

    --br: 2em;
}

div {
    font-family: var(--sans) !important;
    font-weight: 400;
}

body {
    font-family: var(--sans) !important;
    font-weight: 400;
}

code {
    font-family: var(--mono) !important;
    font-weight: 400;
}

.theme-dark,
.theme-light,
:root {
    --background-primary: var(--bg) !important;
    --background-secondary: #2a2a39 !important;
    --background-secondary-alt: var(--black) !important;
    --background-tertiary: var(--bg) !important;
    --background-accent: var(--bg) !important;

    --channeltextarea-background: var(--background-accent) !important;

    --channels-default: var(--grey) !important;
    --text-muted: var(--grey) !important;
    --text-normal: var(--fg) !important;
    --interactive-normal: var(--grey) !important;
    --interactive-hover: var(--fg) !important;
    --interactive-active: var(--fg) !important;
    --interactive-muted: var(--grey) !important;

    --header-primary: var(--xcolor15) !important;
    --header-secondary: var(--fg) !important;
    --toast-background: var(--background-primary) !important;
    --toast-header: var(--background-tertiary) !important;
    --toast-contents: var(--background-secondary) !important;
    --toast-border: var(--background-tertiary) !important;
}

div#online-tab {
    background-color: var(--bg) !important;
}
div.inner-2pOSmK {
    background-color: var(--black) !important;
}

/* hide server bar */
nav[aria-label="Servers sidebar"] {
    width: 10px;
    transition: width 0.4s;
}
nav[aria-label="Servers sidebar"]:hover {
    width: 70px;
}

/* white server buttons */
nav[aria-label="Servers sidebar"] span.item-2LIpTv {
    transition: background-color 0.4s linear;
    background-color: transparent;
}
nav[aria-label="Servers sidebar"]:hover span.item-2LIpTv {
    background-color: var(--grey);
}

/* server sidebar background colour */
nav[aria-label="Servers sidebar"] div.scroller-3X7KbA.scrollerBase-_bVAAt {
    /* background-color: #2a2a39 !important; */
    background-color: var(--black) !important;
    transition: background-color 0.2s linear;
}
nav[aria-label="Servers sidebar"]:hover div.scroller-3X7KbA.scrollerBase-_bVAAt {
    background-color: var(--bg) !important;
}


/* thing at the bottom of server bar with personal settings */
div.container-YkUktl {
    background-color: #2a2a39 ;
}

/* server bar green buttons */
div.circleIconButton-1VxDrg {
    color: var(--green) !important;
    background-color: transparent !important;
}
div.circleIconButton-1VxDrg:hover {
    color: var(--bg) !important;
    background-color: var(--green) !important;
}

/* server folders */
div.folderIconWrapper-1oRIZr {
    background-color: var(--black) !important;
}
div.expandedFolderIconWrapper-3RwQpD svg path[fill="currentColor"] {
    color: var(--blue) !important;
}

/* discord icon at the top */
div.childWrapper-1j_1ub:hover {
    background-color: var(--blue) !important;
    color: var(--bg) !important;
}


/* channel bar */
div.sidebar-1tnWFu {
    width: 10px;
    transition: width 0.4s;
}
div.sidebar-1tnWFu:hover {
    width: 250px;
}
div.sidebar-1tnWFu div.unread-36eUEm {
    background-color: transparent !important;
    color: transparent !important;
    transition: background-color 0.4s linear;
}
div.sidebar-1tnWFu:hover div.unread-36eUEm {
    background-color: var(--fg) !important;
    color: var(--fg) !important;
}
/* personal avatar at the bottom of server bar */
div.sidebar-1tnWFu .container-YkUktl .avatar-1EWyVD {
    opacity: 0 !important;
    transition: opacity 0.2s linear;
}
div.sidebar-1tnWFu:hover .container-YkUktl .avatar-1EWyVD {
    opacity: 1 !important;
}
/* header */
div.sidebar-1tnWFu header.header-3OsQeK {
    background-color: #2a2a39 !important;
    transition: background-color 0.2s linear;
    /* color: #2a2a39 !important; */
}
div.sidebar-1tnWFu:hover header.header-3OsQeK {
    background-color: transparent !important;
}


/* main view */
path.attachButtonPlay-1ATmb5 {
    color: var(--green) !important;
}
/* hide unnecessary buttons */
div.buttonWrapper-3YFQGJ {
    color: transparent !important;
    background-color: transparent !important;
}
.iconBadge-3Mmg92 {
    background-color: var(--red) !important;
}
/* buttons at top */
[aria-label="Help"], [aria-label="Inbox"], [aria-label="Notification settings"] {
    opacity: 0 !important;
}
.iconMentionText-1_WCtN {
    background-color: var(--xcolor0) !important;
    color: var(--fg) !important;
}
.topic-11NuQZ {
    color: var(--xcolor7) !important;
    opacity: 0.3 !important;
}

/* popout */
div#popout_44 div {
    background-color: var(--bg) !important;
    color: var(--fg) !important;
}
div[role="option"], div.base-2v-uc0, div[role="listbox"] {
    background-color: var(--bg) !important;
    transition: background-color 0.1s linear !important;
    color: var(--fg) !important;
}
div[role="option"]:hover, div.base-2v-uc0:hover, div[role="listbox"]:hover {
    background-color: var(--black) !important;
}
input[aria-label="Quick switcher"] {
    background-color: var(--black) !important;
}
div.quickswitcher-pKcM9U {
    background-color: var(--bg) !important;
}
h3.pro-3GwLMX {
    color: var(--green) !important;
}

/* avatars */
img.clickable-31pE3P {
    opacity: 0.9 !important;
    filter: saturate(1.5);
    transform: scale(0.8, 0.8);
}

/* only show timestamps on hover */
.cozy-VmLDNB .timestamp-p1Df1m {
    opacity: 0 !important;
    transition: opacity 0.1s linear;
}
.cozy-VmLDNB:hover .timestamp-p1Df1m {
    opacity: 1 !important;
}

/* hide some buttons unless hovering */
.iconWrapper-2awDjA {
    opacity: 0 !important;
    transition: opacity 0.1s linear;
}
.iconWrapper-2awDjA:hover {
    opacity: 1 !important;
}


/*+-+-+-+-+**+-+-+-+-+-*/
/*|i|don't|really|know|*/
/*+-+-+-+-+**+-+-+-+-+-*/
/**/
/**/
/**/
::-webkit-scrollbar,
::-webkit-scrollbar-track,
::-webkit-scrollbar-track-piece {
    background: transparent !important;
    -webkit-box-shadow: none !important;
    -moz-box-shadow: none !important;
    box-shadow: none !important;
    border: none !important;
    width:5px !important;
}
::-webkit-scrollbar-thumb {
    background: var(--background-primary) !important;
    width:5px !important;
    border:none !important;
    border-radius: 2px !important;
}

.searchBar-3dMhjb {
    width: 28px;
    transition: width 0.1s ease-in-out;
}

.scroller-2TZvBN{
    border-width: 3px;
}
` )
