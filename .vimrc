set nocompatible
filetype plugin on
filetype indent on

set autoindent
set smartindent

set expandtab
set smarttab
set softtabstop=4
set shiftwidth=4

set showmatch
set vb t_vb=
set incsearch

set number
set ruler
set cursorline
set colorcolumn=80

set hlsearch
set nohidden

set nobackup
set noswapfile

set encoding=utf-8

if !has("gui_running") && &term =~ '^\%(screen\|tmux\)'
  let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
  let &t_9b="\<Esc>[48;2;%lu;%lu;%lum"
endif

syntax enable
set termguicolors
colorscheme gruvbox

set mouse=a
if !has("nvim")
  set ttymouse=sgr
endif

" Leader
let mapleader = ","
let maplocalleader = ";"

" Shortcut to rapidly toggle `set list`
nmap <leader>l :set list!<CR>
" set list!

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

" Invisible character colors
highlight NonText guifg=#4a4a59
highlight SpecialKey guifg=#4a4a59

if has("gui_running")
    set guioptions-=T
endif

if has("gui_macvim")
    set guifont=Menlo:h14
endif

au BufNewFile,BufRead *.json set filetype=javascript
au BufNewFile,BufRead *.ejs set filetype=html
au BufNewFile,BufRead *.cljs set filetype=clojure
au FileType javascript,html,css,less,json setlocal tabstop=2 shiftwidth=2

