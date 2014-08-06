#!/usr/bin/env bash
# Install script for Dark Pastel Theme for Gnome Terminal
# This script uses the values from Dark Pastel theme from KDE 4
# This script is a modification of script found here:
# https://github.com/chriskempson/base16-gnome-terminal/blob/master/base16-3024.dark.sh

[[ -z "$PROFILE_NAME" ]] && PROFILE_NAME="Dark Pastels"
[[ -z "$PROFILE_SLUG" ]] && PROFILE_SLUG="dark-pastels"
[[ -z "$DCONF" ]] && DCONF=dconf
[[ -z "$UUIDGEN" ]] && UUIDGEN=uuidgen

dset() {
    local key="$1"; shift
    local val="$1"; shift

    if [[ "$type" == "string" ]]; then
        val="'$val'"
    fi

    "$DCONF" write "$PROFILE_KEY/$key" "$val"
}

# because dconf still doesn't have "append"
dlist_append() {
    local key="$1"; shift
    local val="$1"; shift

    local entries="$(
        {
            "$DCONF" read "$key" | tr -d '[]' | tr , "\n" | fgrep -v "$val"
            echo "'$val'"
        } | head -c-1 | tr "\n" ,
    )"

    "$DCONF" write "$key" "[$entries]"
}

# Newest versions of gnome-terminal use dconf
if which "$DCONF" > /dev/null 2>&1; then
    [[ -z "$BASE_KEY_NEW" ]] && BASE_KEY_NEW=/org/gnome/terminal/legacy/profiles:

    if [[ -n "`$DCONF list $BASE_KEY_NEW/`" ]]; then
        if which "$UUIDGEN" > /dev/null 2>&1; then
            PROFILE_SLUG=`uuidgen`
        fi

        if [[ -n "`$DCONF read $BASE_KEY_NEW/default`" ]]; then
            DEFAULT_SLUG=`$DCONF read $BASE_KEY_NEW/default | tr -d \'`
        else
            DEFAULT_SLUG=`$DCONF list $BASE_KEY_NEW/ | grep '^:' | head -n1 | tr -d :/`
        fi

        DEFAULT_KEY="$BASE_KEY_NEW/:$DEFAULT_SLUG"
        PROFILE_KEY="$BASE_KEY_NEW/:$PROFILE_SLUG"

        # copy existing settings from default profile
        $DCONF dump "$DEFAULT_KEY/" | $DCONF load "$PROFILE_KEY/"

        # add new copy to list of profiles
        dlist_append $BASE_KEY_NEW/list "$PROFILE_SLUG"

        # update profile values with theme options
        dset visible-name "'$PROFILE_NAME'"
        dset palette "['rgb(63,63,63)', 'rgb(112,80,80)', 'rgb(96,180,138)', 'rgb(223,175,143)', 'rgb(154,184,215)', 'rgb(220,140,195)', 'rgb(140,208,211)', 'rgb(220,220,204)', 'rgb(112,144,128)', 'rgb(220,163,163)', 'rgb(114,213,163)', 'rgb(240,223,175)', 'rgb(148,191,243)', 'rgb(236,147,211)', 'rgb(147,224,227)', 'rgb(238,238,236)']"
        dset background-color "'rgb(44,44,44)'"
        dset foreground-color "'rgb(220,220,204)'"
        dset bold-color-same-as-fg "true"
        dset use-theme-colors "false"
        dset use-theme-background "false"
        dset scrollback-unlimited "true"
        dset scrollbar-policy "'never'"
        dset allow-bold "false"
        dset audible-bell "false"

        unset PROFILE_NAME
        unset PROFILE_SLUG
        unset DCONF
        unset UUIDGEN
        exit 0
    fi

    echo "Failed to create profile. Try creating a dummy profile first"

fi
echo "Incompatible gnome version"
