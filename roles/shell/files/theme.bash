#!/usr/bin/env bash

git_dirty_state() {
  (git status -bs --porcelain 2> /dev/null | grep -vq '^#') && echo '*'
}

color() {
  echo '\[\e[0;'"$1"'m\]'
}

bold() {
  echo '\[\e[1;'"$1"'m\]'
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

git_prompt() {
  local git_branch=$(which git &> /dev/null && git_branch 2>/dev/null)
  if [[ -n $git_branch ]]; then
    echo -e "$(bold 92)[$(color 96)${git_branch}$(color 91)$(git_dirty_state)$(bold 92)]"
  fi
}

prompt_command() {
  PS1="\n $(color 33)\w $(git_prompt)$(color 0)\n \$ "
}

export PROMPT_COMMAND=prompt_command
