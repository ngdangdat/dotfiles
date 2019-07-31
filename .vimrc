" Allan MacGregor Vimrc configuration based
" https://dev.to/allanmacgregor/vim-is-the-perfect-ide-e80

" Set run time path to include plugins
" >> NERDTree
set runtimepath^=~/.vim/bundle/nerdtree
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

" >> NERDTree Git
set runtimepath^=~/.vim/bundle/nerdtree-git-plugin
nmap <F2> :NERDTreeFind<CR>
" >> tagbar
set runtimepath^=~/.vim/bundle/tagbar
nmap <F8> :TagbarToggle<CR>

" >> vim-airline
set runtimepath^=~/.vim/bundle/vim-airline
" >> vim-airline-themes
set runtimepath^=~/.vim/bundle/vim-airline-themes
let g:airline_theme='simple'

" >> vim-fugitive
set runtimepath^=~/.vim/bundle/vim-fugitive

" >> vim-deus color
colors deus

set nocompatible
syntax on
set nowrap
set encoding=utf8

" Show linenumbers
set number
set ruler

" Set Proper Tabs
set tabstop=2
set shiftwidth=2
set smarttab
set expandtab

" Show special characters
set list
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<

" Always display the status line
set laststatus=2

" Enable highlighting of the current line
set cursorline

" Theme and Styling 
set t_Co=256
set background=dark

if (has("termguicolors"))
  set termguicolors
endif

let base16colorspace=256  " Access colors present in 256 colorspace

