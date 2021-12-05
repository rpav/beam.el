#!/bin/bash
# ^ This is mostly for editors; you need to source this file

_beam_file="$HOME/.emacs.d/beam_projects.txt"

_beam_file_test() {
    if [ ! -r "$_beam_file" ]; then
        if [ "$1" ]; then
           echo >&2
        fi

        echo "No ~/.emacs.d/beam-projects.el found; run M-x beam-write-projects-txt in emacs." >&2
        return 1
    fi

    return 0
}

cdp() {
    _beam_file_test || return

    LINE=($(grep "^$1 " $HOME/.emacs.d/beam-projects.txt))
    cd "${LINE[1]}"
}

_beam_completion() {
    count="${#COMP_WORDS[@]}"
    input="${COMP_WORDS[1]}"

    _beam_file_test 1 || return

    if [ "$count" -ne 2 ]; then
        return
    fi

    declare -a words

    while read -a line; do
        words+=(${line[0]})
    done < $HOME/.emacs.d/beam-projects.txt


    if [ "$input" ]; then
        COMPREPLY=($(compgen -W "${words[*]}" -- $input))
    else
        COMPREPLY=(${words[@]})
    fi
}

complete -F _beam_completion cdp
