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

set backup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp
set noswapfile

set encoding=utf-8

execute pathogen#infect()

syntax enable
set background=dark
let g:solarized_termtrans=1
colorscheme solarized

set mouse=a

" Leader
let mapleader = ","
let maplocalleader = ";"

" Shortcut to rapidly toggle `set list`
nmap <leader>l :set list!<CR>
set list!

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

" Invisible character colors
highlight NonText guifg=#4a4a59
highlight SpecialKey guifg=#4a4a59

if has("gui_running")
    set guioptions-=T
    set guifont=Menlo:h14
endif

au BufNewFile,BufRead *.json set filetype=javascript
au BufNewFile,BufRead *.ejs set filetype=html
au BufNewFile,BufRead *.cljs set filetype=clojure
au FileType javascript,html,css,less,json setlocal tabstop=2 shiftwidth=2

" Settings for VimClojure
let vimclojure#FuzzyIndent=1
let vimclojure#HighlightBuiltins=1
let vimclojure#HighlightContrib=1
let vimclojure#DynamicHighlighting=1
let vimclojure#ParenRainbow=1
let vimclojure#WantNailgun=1

" ctrlp.vim
set runtimepath^=~/.vim/bundle/ctrlp.vim

" Clang_Complete
let g:clang_close_preview = 1
let g:clang_auto_select = 1
if has("macunix")
    let g:clang_library_path = "/Library/Developer/CommandLineTools/usr/lib"
endif
