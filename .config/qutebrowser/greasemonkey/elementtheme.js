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
    --fg: #93a1a1;
    --bg: #002b36;
    --black: #073642;
    --red: #dc322f;
    --green: #859900;
    --orange: #ca6721;
    --yellow: #b58900;
    --blue: #268bd2;
    --magenta: #6c71c4;
    --cyan: #2aa198;
    --grey: #657b83;
    --xcolor15: #839496;

    --sans: Fira Sans;
    --mono: IosevkaCustom;

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
}

/* pre-formatted text (code) */
pre {
    background-color: var(--black) !important;
    border-radius: var(--br) !important;
    padding: 1.6em !important;
    box-shadow: inset 9px 9px 20px #2a2a2a,
            inset -9px -9px 20px #444444 !important;
    border: 0px solid transparent !important;
}
code {
    background-color: transparent !important;
}

/* messages */
.mx_EventTile:hover .mx_EventTile_line {
    background-color: var(--black) !important;
    box-shadow:  9px 9px 14px #161616,
             -9px -9px 14px #242424 !important;
}
.mx_EventTile_line {
    border-radius: var(--br) !important;
    padding: 0.4em 4em !important;
    border: 0px solid transparent !important;
}
.mx_EventTile_line:hover {
    background-color: var(--black) !important;
    box-shadow:  9px 9px 14px #161616,
             -9px -9px 14px #242424 !important;
}
.mx_MessageTimestamp {
    color: var(--yellow) !important;
    font-family: var(--mono) !important;
    padding: 0.1em 1em !important;
}
.mx_UserPill {
    color: var(--fg) !important;
    background-color: var(--black) !important;
    padding: 0.3em 0.9em !important;
    margin: -0.2em 0.4em 0em
}
.mx_LinkPreviewWidget {
    color: var(--fg);
    background-color: var(--black) !important;
    border-radius: var(--br) !important;
    padding: 2em 1.6em 2em 0em !important;
    box-shadow:  9px 9px 14px #161616,
             -9px -9px 14px #242424 !important;
    border: 0px solid transparent !important;
}
.mx_LinkPreviewGroup {
    padding: 0.4em 2em !important;
}
.mx_SenderProfile {
    margin-bottom: 0.6em !important;
}

/* buttons */
.mx_MessageActionBar {
    background-color: var(--black) !important;
    border: 0px solid transparent !important;
    margin-top: 1em !important;
    border-radius: var(--br) !important;
    box-shadow:  9px 9px 14px #161616,
             -9px -9px 14px #242424 !important;
}

/* header */
.mx_RoomHeader_wrapper {
    background-color: var(--bg) !important;
    border-bottom: 0.16em solid var(--black) !important;
    padding: 0.4em !important;
}
.mx_RoomHeader_wrapper a{
    color: var(--blue) !important;
}
.mx_RoomHeader_wrapper a:hover {
    color: var(--blue) !important;
}
.mx_RoomHeader_buttons {
    background-color: var(--black) !important;
    border-radius: var(--br) !important;
    padding: 0em 0.4em !important;
    box-shadow:  9px 9px 14px #161616,
             -9px -9px 14px #242424 !important;
}
.mx_RoomHeader_name {
    background-color: var(--black) !important;
    border-radius: var(--br) !important;
    padding: 0.4em 0.9em !important;
    box-shadow:  9px 9px 14px #161616,
             -9px -9px 14px #242424 !important;
    color: var(--yellow) !important;
}
.mx_RoomHeader_topic {
    color: var(--fg) !important;
}

/* sidebar */
.mx_LeftPanel_roomListContainer {
    background-color: var(--bg) !important;
    color: var(--fg) !important;
}
.mx_RoomTile_selected {
    background-color: var(--black) !important;
    color: var(--cyan) !important;
    box-shadow:  9px 9px 14px #161616,
             -9px -9px 14px #242424 !important;
    border-radius: var(--br) !important;
}
.mx_RoomSearch {
    background-color: var(--bg) !important;
    box-shadow:  9px 9px 14px #161616,
             -9px -9px 14px #242424 !important;
    border-radius: var(--br) !important;
}
.mx_LeftPanel_exploreButton {
    background-color: var(--bg) !important;
    box-shadow:  9px 9px 14px #161616,
             -9px -9px 14px #242424 !important;
    border-radius: var(--br) !important;
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
    box-shadow:  9px 9px 14px #161616,
             -9px -9px 14px #242424 !important;
    border-radius: var(--br) !important;
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
    border: 0px !important;
    border-radius: var(--br) !important;
    background-color: var(--bg) !important;
    color: var(--green) !important;
    box-shadow: none !important;
}
div.mx_Dialog_title {
    color: var(--fg) !important;
}
.mx_Dialog_cancelButton {
    background-color: var(--red) !important;
}
button.mx_Dialog_primary {
    border-radius: var(--br) !important;
    background-color: var(--green) !important;
    color: var(--bg) !important;
    box-shadow:  9px 9px 14px #181818,
             -9px -9px 14px #272727 !important;
    border: 0px !important;
}
.mx_UploadConfirmDialog_imagePreview {
    border-radius: var(--br) !important;
    border: 0px !important;
    box-shadow:  9px 9px 14px #181818,
             -9px -9px 14px #272727 !important;
    margin: 0em 0em 1em !important;
}

/*box that sometimes appears above send space*/
.mx_RoomView_statusAreaBox {
    background-color: var(--bg) !important;
}
` )
