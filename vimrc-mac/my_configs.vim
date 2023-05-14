" Whitespace
set number
set tabstop=4
set shiftwidth=4
set expandtab
" Folding
set nofoldenable


" Smart way to move between windows (<ctrl>j etc.):
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Open ack.vim for fast search:
map <leader>g :Ack

" NERD Tree mappings:
map <leader>nn :NERDTreeToggle<cr>
map <leader>nb :NERDTreeFromBookmark 
map <leader>nf :NERDTreeFind<cr>

" Copy/paste from clipboard
map <C-c> "+y
map <C-v> "+p

" Color scheme
set t_Co=256
set background=dark
