#!/usr/bin/env bash

red='\[\e[31m\]'
yellow='\[\e[0;33m\]'
normal='\[\e[0m\]'

git_dirty_state() {
  (git status -bs --porcelain 2> /dev/null | grep -vq '^#') && echo '*'
}

git_branch() {
  local symbolic_ref=$(git symbolic-ref -q HEAD)
  if [[ -n "$symbolic_ref" ]]; then
    echo "${symbolic_ref#refs/heads/}"
  else
    git name-rev --name-only --no-undefined --always HEAD \
      | sed 's#tags/##' | sed 's#remotes/##'
  fi
}

git_prompt_info() {
  git_branch=$(which git &> /dev/null && git_branch 2>/dev/null)
  if [[ -n $git_branch ]]; then
    echo -e "${normal}[${git_branch}${red}$(git_dirty_state)${normal}]"
  fi
}

prompt_command() {
  PS1="\n ${yellow}\w $(git_prompt_info)${normal}\n \$ "
}

export PROMPT_COMMAND=prompt_command
