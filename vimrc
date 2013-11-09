"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" VIM configuration file
"   ~/.vimrc
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Forget being compatible with good ol' vi
set nocompatible

" Set lines of command line history
set history=100

" Enable filetype plugins
filetype on
filetype plugin on
filetype indent on

" Spell checking
"setlocal spell spelllang=en

" Set to read automatically when a file is changed from the outside
set autoread

" Set UTF-8 as standard encoding
set encoding=utf8

" Use unix as the standard file type
set ffs=unix,dos,mac

" C-n / C-p completion. The order of places to scan for completion:
" current buffer, buffers from other windows, other loaded buffers, tags
set complete=.,w,b,t

" Detect modelines in the first five or the last five lines of each file
set modelines=5

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Show line numbers
set number

" Highlighting of the current line
set cul
hi CursorLine term=none cterm=none ctermbg=16
hi CursorLineNr term=bold ctermfg=Yellow gui=bold guifg=Yellow

" Ignore case when searching
set ignorecase
  
" Hightlight search results
set hlsearch

" Incremental search
set incsearch

" Command-line completion in enhanced mode
set wildmenu

" Show information about current command
set showcmd

" Put a dollar sign at the end of the thing being changed with the c command
set cpoptions+=$

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colors and fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable

" Set 256 colors (terminal independent way)
set t_Co=256

" Set colorscheme using CSApprox script
" (Make gvim-only colorschemes work transparently in terminal vim)
let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
colorscheme wombat

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tabs and keep them 2 spaces wide by default
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab

set ai      " Auto indent
set si      " Smart indent
set wrap    " Wrap lines

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Shortcuts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <F5> :make<CR>

" The_NERD_tree - A really handy tree-structured filebrowser.
map <F3> :NERDTreeToggle <CR>

" Turn off highlight search
nmap <silent> ,n :nohls<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" LaTeX
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set textwidth=80 when opening tex files
autocmd FileType plaintex,tex set tw=80

" Mappings
"map \ll :!pdflatex %<CR>
"map \lv :!zathura %

autocmd FileType plaintex,tex map \ll :!texBuild.sh<CR>
autocmd FileType plaintex,tex let pdfname = substitute(expand('%:p'), "tex", "pdf", "")
autocmd FileType plaintex,tex map \lv :execute '!zathura ' . pdfname

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SML
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType sml set makeprg=rlwrap\ mosml\ -P\ full\ '%'
autocmd FileType sml set colorcolumn=80
autocmd FileType sml highlight ColorColumn ctermbg=black
autocmd BufEnter,BufNew *.sig set ft=sml

"map ,t :ConqueTermVSplit mosml -P full %<CR>

"autocmd FileType sml map ,ml :call OpenInMosML()<CR>

"func! OpenInMosML()
"  " Expand the current file name to full path
"  let currentFile = expand("%:p")
"  :ConqueTermVSplit mosml -P full %:p
"endfunc

"autocmd FileType sml setlocal makeprg=mosml\ -P\ full\ '%'

autocmd FileType sml setlocal splitbelow
autocmd FileType sml map <F4> :call OpenInMosML()<CR>

func! OpenInMosML()
    " Expland the current file name to full path, the result is a String.
    let currentFile = expand("%:p")
    :new
    :call conque_term#open('rlwrap mosml -P full')
    :call conque_term#get_instance().writeln('use "' . currentFile . '";')
endfunc

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Python
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType python setlocal makeprg=python\ -i\ %
autocmd FileType python setlocal tabstop=4 softtabstop=4 shiftwidth=4 expandtab
autocmd FileType python setlocal textwidth=79
autocmd FileType python setlocal colorcolumn=80
autocmd FileType python highlight ColorColumn ctermbg=black

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Haskell
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType haskell set makeprg=ghci\ %
autocmd FileType haskell set textwidth=79

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" C
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType c set tabstop=4 softtabstop=4 shiftwidth=4 expandtab
autocmd FileType c set textwidth=79

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MIPS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType asm set textwidth=89
autocmd FileType asm set foldmethod=manual
autocmd FileType asm set syntax=mips

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" F#
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd BufNewFile,BufRead *.fs,*.fsx set filetype=fsharp
