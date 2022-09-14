// ==UserScript==
// @name          Element theme
// @run-at        document-end
// @grant         none
// @include       https://app.element.io/*
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
    --fg: #224750;
    --bg: #eee8d5;
    --black: #e2dac1;
    --red: #dc322f;
    --green: #859900;
    --orange: #ca6721;
    --yellow: #b58900;
    --blue: #268bd2;
    --magenta: #6c71c4;
    --cyan: #2aa198;
    --grey: #657b83;
    --xcolor15: #839496;

    --sans: FreeSans;
    --mono: Iosevka;

    --br: 2em;
}

/* message page */
ol {
    background-color: var(--bg) !important;
    color: var(--fg) !important;
}
ol a {
    color: var(--blue) !important;
}
div.mx_AccessibleButton {
    color: var(--cyan) !important;
    background-color: transparent !important;
}

/* pre-formatted text (code) */
pre {
    background-color: var(--black) !important;
}
code {
    background-color: transparent !important;
}

/* messages */
.mx_EventTile:hover .mx_EventTile_line {
    background-color: var(--black) !important;
}
.mx_EventTile_line:hover {
    background-color: var(--black) !important;
}
.mx_MessageTimestamp {
    color: var(--yellow) !important;
    font-family: var(--mono) !important;
}
.mx_UserPill {
    color: var(--fg) !important;
    background-color: var(--black) !important;
}
.mx_LinkPreviewWidget {
    color: var(--fg);
    background-color: var(--black) !important;
}

/* buttons */
.mx_MessageActionBar {
    background-color: var(--black) !important;
}

/* header */
.mx_RoomHeader {
    background-color: var(--bg) !important;
}
.mx_RoomHeader_wrapper {
    background-color: var(--bg) !important;
}
.mx_RoomHeader_wrapper a{
    color: var(--blue) !important;
}
.mx_RoomHeader_wrapper a:hover {
    color: var(--blue) !important;
}
.mx_RoomHeader_buttons {
    background-color: transparent !important;
}
.mx_RoomHeader_name {
    background-color: var(--black) !important;
    color: var(--yellow) !important;
}
.mx_RoomHeader_topic {
    color: var(--fg) !important;
}

/* general stuff, idk */
.mx_RoomView_body {
    background-color: var(--bg) !important;
}
.mx_RoomView_MessageList {
    background-color: var(--bg) !important;
}

/* sidebar */
.mx_LeftPanel_roomListContainer {
    background-color: var(--bg) !important;
    color: var(--fg) !important;
}
.mx_RoomTile_selected {
    background-color: var(--black) !important;
    color: var(--cyan) !important;
}
.mx_RoomSearch {
    background-color: var(--bg) !important;
}
.mx_LeftPanel_exploreButton {
    background-color: var(--bg) !important;
}
.mx_RoomSublist_auxButton {
    background-color: transparent !important;
}

/* leftmost pane */
.mx_GroupFilterPanel_scroller {
    background-color: var(--bg) !important;
}
.mx_RoleButton {
    background-color: var(--black) !important;
}


/* send bar */
div.mx_MessageComposer_wrapper {
    background-color: var(--bg) !important;
    color: var(--fg) !important;

}

/* username colours */
.mx_Username_color1 {
    color: var(--blue) !important;
}
.mx_Username_color2 {
    color: var(--magenta) !important;
}
.mx_Username_color3 {
    color: var(--cyan) !important;
}
.mx_Username_color4 {
    color: var(--red) !important;
}
.mx_Username_color5 {
    color: var(--orange) !important;
}
.mx_Username_color7 {
    color: var(--magenta) !important;
}
.mx_Username_color8 {
    color: var(--green) !important;
}

/*dialogues such as "upload files"*/
div.mx_Dialog {
    background-color: var(--bg) !important;
    color: var(--green) !important;
}
div.mx_Dialog_title {
    color: var(--fg) !important;
}
.mx_Dialog_cancelButton {
    background-color: var(--red) !important;
}
button.mx_Dialog_primary {
    background-color: var(--green) !important;
    color: var(--bg) !important;
}

/*box that sometimes appears above send space*/
.mx_RoomView_statusAreaBox {
    background-color: var(--bg) !important;
}
` )
