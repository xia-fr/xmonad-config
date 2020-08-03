" -----------------------------------------------------------------------------
"  GENERAL SETTINGS FOR EVERYONE
"  ----------------------------------------------------------------------------
filetype plugin indent on
set nocompatible
set autoindent
set nomodeline " disable modeline vulnerability

" text encoding
set encoding=utf8

" use 4 spaces for tabs
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround

set backspace =indent,eol,start
set hidden
set laststatus =2

" Set linenumbers
set number
set wrap

" column ruler at 100
set ruler
set colorcolumn=0

" Highlight searching
set incsearch
set showmatch
set hlsearch
set ignorecase
set smartcase

set autoread " autoread files
set mouse=a " use mouse for scroll or window size

" Uses system clipboard by default, yy to copy and p to paste
set clipboard=unnamedplus
let mapleader = ','

" -----------------------------------------------------------------------------
"  PLUGIN SETUP
"  ----------------------------------------------------------------------------
" Autoload vim plug if not already there

if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" All the plugins are listed here
call plug#begin('~/.vim/plugged')

" Productivity
Plug 'junegunn/vim-plug'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-unimpaired'
Plug 'scrooloose/nerdtree', { 'on' : 'NERDTreeToggle' }

" Commenting
Plug 'scrooloose/nerdcommenter'
let g:NERDSpaceDelims = 1
let g:NERDDefaultAlign = 'left'

Plug 'rafi/awesome-vim-colorschemes'

" Programming plugins
Plug 'lervag/vimtex'

call plug#end() " start all the plugins above
" -----------------------------------------------------------------------------
"  VIMTEX OPTIONS
"  ----------------------------------------------------------------------------
if has('unix')
    let g:latex_view_general_viewer = "zathura"
    let g:vimtex_view_method = "zathura"
endif

let g:tex_flavor = "latex"
let g:vimtex_quickfix_open_on_warning = 0
let g:vimtex_quickfix_mode = 2

" One of the neosnippet plugins will conceal symbols in LaTeX which is
" confusing
let g:tex_conceal = ""

" Can hide specifc warning messages from the quickfix window
" Quickfix with Neovim is broken or something
" https://github.com/lervag/vimtex/issues/773
let g:vimtex_quickfix_latexlog = {
            \ 'default' : 1,
            \ 'fix_paths' : 0,
            \ 'general' : 1,
            \ 'references' : 1,
            \ 'overfull' : 1,
            \ 'underfull' : 1,
            \ 'font' : 1,
            \ 'packages' : {
            \   'default' : 1,
            \   'natbib' : 1,
            \   'biblatex' : 1,
            \   'babel' : 1,
            \   'hyperref' : 1,
            \   'scrreprt' : 1,
            \   'fixltx2e' : 1,
            \   'titlesec' : 1,
            \ },
            \}

" -----------------------------------------------------------------------------
"  APPEARANCE
"  ----------------------------------------------------------------------------
syntax on

let g:sierra_Twilight = 1
colorscheme sierra

" -----------------------------------------------------------------------------
"  KEY BINDINGS
"  ----------------------------------------------------------------------------

" Allows easy clearing of search highlighting
nnoremap <silent> ,<space> :nohlsearch<CR>







