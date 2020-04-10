call plug#begin('~/.local/share/nvim/plugged')
Plug 'morhetz/gruvbox'
Plug 'sheerun/vim-polyglot', { 'do': './build' }
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-fugitive'
Plug 'Valloric/ListToggle'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'godlygeek/tabular'
Plug 'dense-analysis/ale'
Plug 'bufbuild/vim-buf'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
call plug#end()

filetype plugin indent on

" Disable mouse integration
set mouse=

set background=dark
let g:gruvbox_contrast_dark='soft'
let g:gruvbox_invert_selection=0
colorscheme gruvbox

" Statusline
set statusline=[%f]\ %r%=[%l,%c]\ %y\ [%{strlen(&fenc)?&fenc:'none'}]\ [%p%%]

" Config
set cursorline
set expandtab
set hidden
set hlsearch
set ignorecase
set nobackup
set nowritebackup
set number
set shiftwidth=2
set shortmess=atI
set smartcase
set smartindent
set tabstop=2
set title
set wildignorecase
set wildmode=list:longest,full

" Keymaps
set timeoutlen=400
imap <silent> uu <Esc><Esc>:w<CR>
nnoremap <C-n> :GFiles<CR>
nmap <silent> <C-h> :wincmd h<CR>
nmap <silent> <C-k> :wincmd k<CR>
nmap <silent> <C-j> :wincmd j<CR>
nmap <silent> <C-l> :wincmd l<CR>

" Leader keymaps
nnoremap <Space> <Nop>
let mapleader = ' '
" Leader: Top row
let g:lt_quickfix_list_toggle_map = '<Leader>f'
" Leader: Home row
nnoremap <Leader>a :wqa<CR>
nnoremap <Leader>i gg=G<C-O><C-O>
nnoremap <Leader>t :NERDTreeToggle<CR>
nnoremap <Leader>n :nohlsearch<CR>
" Leader: Bottom row
nnoremap <Leader>q :q<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>z :qa!<CR>
" Leader: U
nnoremap <Leader>ut :TableFormat<CR>

" Autocommands
augroup filetype_python
  autocmd!
  autocmd FileType python set tabstop=4 expandtab shiftwidth=4 softtabstop=4
augroup END

augroup filetype_conf
  autocmd!
  autocmd FileType conf set tabstop=2 expandtab shiftwidth=2 softtabstop=2
augroup END

" Tag manipulation
augroup xml
  autocmd!
  autocmd FileType html,xml,javascript.jsx inoremap <buffer> <C-t> <ESC>viw"tyea><ESC>bi<<ESC>lela</<ESC>"tpa><ESC>T>i
  autocmd FileType html,xml,javascript.jsx inoremap <buffer> <C-n> <CR><CR><ESC>ka<Tab>
augroup END

" Markdown
let g:vim_markdown_folding_disabled=1
let g:vim_markdown_frontmatter=1
augroup filetype_markdown
  autocmd!
  autocmd FileType markdown setlocal textwidth=74
augroup END

" Go
let g:go_fmt_command='goimports'

" NERDTree
let g:NERDTreeMinimalUI=1
let g:NERDTreeDirArrows=0
let NERDTreeShowHidden=1
let NERDTreeIgnore = ['\.pyc$', '.git', '\.o$']

" Ale
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters_explicit = 1
let g:ale_completion_enabled = 1
let g:ale_linters = {
\   'sh': ['shellcheck'],
\   'go': ['golangci-lint'],
\   'proto': ['buf-check-lint'],
\}
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['eslint'],
\   'python': ['black'],
\   'json': ['prettier'],
\   'markdown': ['prettier'],
\   'sh': ['shfmt'],
\   'go': ['goimports'],
\   'cpp': ['clang-format'],
\   'haskell': ['hfmt'],
\   'yaml': ['prettier'],
\   'yaml.ansible': ['prettier'],
\}
let g:ale_fix_on_save = 1

" fzf
let g:fzf_colors =
\ { 'fg':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'bg+':     ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Directory'],
  \ 'hl+':     ['fg', 'Directory'],
  \ 'info':    ['fg', 'Normal'],
  \ 'prompt':  ['fg', 'Normal'],
  \ 'pointer': ['fg', 'Normal'],
  \ 'marker':  ['fg', 'Normal'],
  \ 'spinner': ['fg', 'Normal'],
  \ 'header':  ['fg', 'Normal'] }
augroup fzf
  autocmd!
  autocmd User FzfStatusLine setlocal statusline=Finding..
augroup END
