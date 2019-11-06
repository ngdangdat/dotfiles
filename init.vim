set termguicolors
call plug#begin(stdpath('data') . '/plugged')

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTree Git plugin' }
Plug 'tpope/vim-fugitive', { 'on': ['Glog', 'Gstatus'] }
Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ajmwagar/vim-deus'
" LSP support
" Use release branch (Recommend)
Plug 'neoclide/coc.nvim', { 'branch': 'release' }

call plug#end()

" General vim configuration
set nocompatible
syntax on
set nowrap
set encoding=utf8

" Show special characters
set list
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<

set laststatus=2
set cursorline
set background=dark
set t_Co=256

" Show line number
set number
set ruler

" Indentation
set tabstop=2
set shiftwidth=2
set smarttab
set expandtab

let base16colorspace=256  " Access colors present in 256 colorspace

" NERDTree configuration begin
autocmd vimenter * NERDTree

" >>> Set the file active
au VimEnter * wincmd l

autocmd StdinReadPre * let s:std_in=1
" >>> How can I open a NERDTree automatically when vim starts up if no files were specified?
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" >>> How can I open NERDTree automatically when vim starts up on opening a directory?
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
" >>> Close if the last window is closed
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <C-n> :NERDTreeToggle<CR>
" NERDTree configuration end

let g:airline_theme='simple'
colors deus
