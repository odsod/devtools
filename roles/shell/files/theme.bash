#!/usr/bin/env bash

bg_black='\[\e[40m\]'
bold_dark_grey='\[\e[32;1m\]'
bold_light_grey='\[\e[36;1m\]'
bold_grey='\[\e[33;1m\]'
bold_red='\[\e[31;1m\]'
yellow='\[\e[0;33m\]'
normal='\[\e[0m\]'

function git_dirty_state {
  local status="$(git status -bs --porcelain 2> /dev/null)"
  grep -vq '^#' <<< "${status}" && echo ' *'
}

function git_branch {
  local ref=$(git symbolic-ref -q --short HEAD 2> /dev/null)
  if [[ -n "$ref" ]]; then
    echo "$ref"
  else
    git describe --tags --exact-match 2> /dev/null
  fi
}

function git_prompt_info {
  if [[ -f .git/HEAD ]] \
     || which git &> /dev/null \
        && [[ -n "$(git symbolic-ref HEAD 2> /dev/null)" ]]; then
    echo -e "${bold_grey}[${bold_light_grey}$(git_branch)${bold_red}$(git_dirty_state)${bold_grey}]${normal}"
  fi
}

function prompt_command() {
  PS1=$(cat << EOF

${bg_black}$(printf "%${COLUMNS}s" ' ')\r\
${bold_dark_grey}\u${bold_light_grey}@\h\
 ${yellow}${bg_black}\w\
 $(git_prompt_info)${normal}
> 
EOF
)
}

export PROMPT_COMMAND=prompt_command
