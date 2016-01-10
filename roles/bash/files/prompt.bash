#!/usr/bin/env bash

color() {
  echo '\[\e[0;'"$1"'m\]'
}

git_dirty_state() {
  dirty_files=$(git status -bs --porcelain 2>/dev/null | grep -v '^#')
  [[ -n $dirty_files ]] && echo "*"
}

git_ahead_state() {
  num_commits_ahead=$(git status -bs --porcelain 2>/dev/null \
    | head -1 | grep ahead | sed 's/.*ahead \([[:digit:]]*\).*/\1/')
  [[ -n $num_commits_ahead ]] && echo -n "+$num_commits_ahead"
  num_commits_behind=$(git status -bs --porcelain 2>/dev/null \
    | head -1 | grep behind | sed 's/.*behind \([[:digit:]]*\).*/\1/')
  [[ -n $num_commits_behind ]] && echo -n "-$num_commits_behind"
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
    echo -e "$(color 92)[$(color 96)${git_branch}$(git_ahead_state)$(color 91)$(git_dirty_state)$(color 92)]"
  fi
}

prompt_command() {
  PS1="\n $(color 33)\w $(git_prompt)$(color 0)\n \$ "
}

export PROMPT_COMMAND=prompt_command
