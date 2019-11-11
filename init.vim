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
Plug 'tyrannicaltoucan/vim-quantum'
Plug 'ctrlpvim/ctrlp.vim'
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
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:·
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

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

" Tweak for Markdown mode
autocmd FileType markdown call s:markdown_mode_setup()
function! s:markdown_mode_setup()
  set wrap
  set nonumber
  set textwidth=80
  set formatoptions+=t
  CocDisable
endfunction

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
" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
" NERDTree configuration end

let g:airline_theme='simple'
let g:quantum_italics=1

" CtrlP mapping
let g:ctrlp_map = '<c-p>'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_user_command = 'find %s -type f'

colorscheme quantum
