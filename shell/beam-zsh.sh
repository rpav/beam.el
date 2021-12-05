#!/bin/zsh
# ^ This is mostly for editors; you need to source this file

_beam_file="$HOME/.emacs.d/beam-projects.txt"

function _beam_file_test {
    if [ ! -r "$_beam_file" ]; then
        if [ "$1" ]; then
            echo >&2
        fi

        echo "No ~/.emacs.d/beam-projects.el found; run M-x beam-write-projects-txt in emacs."
        return 1
    fi

    return 0
}

function cdp {
    _beam_file_test || return

    LINE=($(grep "^$1 " $HOME/.emacs.d/beam-projects.txt))
    cd "${LINE[2]}"
}

function _beam_completion {
    _beam_file_test 1 || return

    lines=(${(@f)"$(< $HOME/.emacs.d/beam-projects.txt)"})

    for line in ${lines[@]}; do
        reply+=(${${(s/ /)line}[1]})
    done
}

compctl -K _beam_completion cdp
