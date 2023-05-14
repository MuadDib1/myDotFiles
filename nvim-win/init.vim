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

language en_US.utf8
set nocompatible
set number
set encoding=utf-8
set fileencodings=iso-2022-jp,euc-jp,sjis,utf-8
set fileformats=unix,dos,mac
set termencoding=utf-8
set background=dark
set ruler
set showcmd

" Enable Windows clipboard copy/paste in INSERT mode
source $VIMRUNTIME/mswin.vim


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
" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

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

" Format the status line
" set statusline=\ %F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c

""""""""""""""""""""""""""""""
" => Python Provider
""""""""""""""""""""""""""""""
" Python3 Provider
let g:python3_host_prog = 'C:\Users\sheri\.pyenv\pyenv-win\versions\3.10.11\python.exe'

" Disable Python2 Provider
let g:loaded_python_provider = 0

""""""""""""""""""""""""""""""
" => VimPlug
""""""""""""""""""""""""""""""
call plug#begin('C:\Users\sheri\AppData\Local\nvim\plugged')
" The default plugin directory will be as follows:
"   - Neovim (Linux/macOS/Windows): stdpath('data') . '/plugged'
" Make sure you use single quotes

" The NERD tree
Plug 'scrooloose/nerdtree'

" Git wrapper
Plug 'tpope/vim-fugitive'

" Emmet
Plug 'mattn/emmet-vim'

" gruvbox color scheme
Plug 'morhetz/gruvbox'

" Airline
Plug 'bling/vim-airline'

" Airline themes
Plug 'vim-airline/vim-airline-themes'

" Initialize plugin system
call plug#end()

colorscheme gruvbox

let g:airline_theme='simple'
let g:airline_powerline_fonts = 1

" NEEDTree
nnoremap <leader>nn :NERDTreeToggle<CR>
nnoremap <leader>nb :NERDTreeFromBookmark
let NERDTreeShowBookmarks=1
let NERDTreeShowHidden=1

