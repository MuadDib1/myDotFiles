"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Sets how many lines of history VIM has to remember
set history=500

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread
au FocusGained,BufEnter * checktime

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","

" Fast saving
nmap <leader>w :w!<cr>

language en_AU.UTF-8
set nocompatible
set number relativenumber
set encoding=utf-8
set fileencodings=iso-2022-jp,euc-jp,sjis,utf-8
set fileformats=unix,dos,mac
set background=dark
set ruler
set showcmd

" Enable Windows clipboard copy/paste in INSERT mode
" source $VIMRUNTIME/mswin.vim


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git etc. anyway...
set nobackup
set nowb
set noswapfile

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set smarttab

" Default indent
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab

augroup indent_config
  autocmd!
  autocmd FileType vue,html,javascript setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
augroup END

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /
map <C-space> ?

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l


""""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""
" Always show the status line
set laststatus=2


""""""""""""""""""""""""""""""
" => VimPlug
""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

" The NERD tree
Plug 'scrooloose/nerdtree'

" Git wrapper
Plug 'tpope/vim-fugitive'

" Emmet
Plug 'mattn/emmet-vim'

" gruvbox color scheme
Plug 'morhetz/gruvbox'

" Airline
Plug 'vim-airline/vim-airline' 

" Airline themes
Plug 'vim-airline/vim-airline-themes'

" Vue support
Plug 'posva/vim-vue'

" Blade support
Plug 'jwalton512/vim-blade'

" fzf
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

call plug#end()

colorscheme gruvbox

let g:airline_theme='simple'
let g:airline_powerline_fonts = 1

" NEEDTree
nnoremap <leader>nn :NERDTreeToggle<CR>
nnoremap <leader>nb :NERDTreeFromBookmark
let NERDTreeShowBookmarks=1
let NERDTreeShowHidden=1

