"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" VIM configuration file
"   ~/.vimrc
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-plug
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

" A colorful, dark color scheme for Vim.
Plug 'nanotech/jellybeans.vim'

" A low-contrast Vim color scheme based on Seoul Colors.
Plug 'junegunn/seoul256.vim'

" FuzzyFinder : buffer/file/command/tag/etc explorer with fuzzy matching
" L9 : Vim-script library required by FuzzyFinder
Plug 'L9'
Plug 'FuzzyFinder'

call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Plugin 'taglist.vim'
"Plugin 'mips.vim'
"Plugin 'hexHighlight.vim'
"Plugin 'VisIncr'
"Plugin 'scrooloose/nerdtree'
"Plugin 'DrTom/fsharp-vim'
"Plugin 'scrooloose/syntastic'
" A dark vim color scheme for 256-color terminals.
"Plugin 'Lokaltog/vim-distinguished'
" A colorful, dark color scheme for Vim.
"Plugin 'nanotech/jellybeans.vim'
" Elegant buffer explorer - takes very little screen space.
"Plugin 'fholgado/minibufexpl.vim'
" Markdown Vim Mode
"Plugin 'plasticboy/vim-markdown'
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible  " be iMproved

" Set lines of command line history
set history=100

" Set spell checking region to US English
set spelllang=en_us
" Toggle spell checking on and off with ,s
nmap <silent> <leader>s :set spell!<CR>

" Set to read automatically when a file is changed from the outside
set autoread

" Set UTF-8 as standard encoding
set encoding=utf8

" Use unix as the standard file format
set ffs=unix,dos,mac

" Set <leader> key
let mapleader = ","

" C-n / C-p completion. The order of places to scan for completion:
" current buffer, buffers from other windows, other loaded buffers, tags
set complete=.,w,b,t

" Detect modelines in the first five or the last five lines of each file
"set modelines=5
" Prevent security exploits through modelines
set modelines=0

" Set default textwidth
set textwidth=79

" Make Vim more liberal about hidden buffers (buffers with unsaved changes that
" are not shown in a window), i.e. allow hiding buffers without saving them.
set hidden

" Disable F1 :help mapping
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" Show highlighting groups for current word
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Show line numbers
set number

" Highlighting of the current line
set cul
"hi CursorLine term=none cterm=none ctermbg=16
"hi CursorLineNr term=bold ctermfg=Yellow gui=bold guifg=Yellow

" Ignore case when searching, unless the pattern contains an uppercase letter
set ignorecase
set smartcase
  
" Hightlight search results
set hlsearch

" Incremental search
set incsearch

" Command-line completion in enhanced mode
"set wildmenu

" Show information about current command
set showcmd

" Put a dollar sign at the end of the thing being changed with the c command
set cpoptions+=$

" Invisible characters when :set list
set listchars=tab:▸\ ,eol:¬

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colors and fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable

" Set 256 colors (terminal independent way)
set t_Co=256

" Set colorscheme using CSApprox script
" (Make gvim-only colorschemes work transparently in terminal vim)
"let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
"colorscheme wombat

" seoul256 (dark):
"   Range:   233 (darkest) ~ 239 (lightest)
"   Default: 237
"let g:seoul256_background = 233
"colo seoul256

colo jellybeans

" Modifications:
" highlight todo keyword black on yellow
hi Todo ctermfg=16 ctermbg=11
" highlight search results black on white
hi Search ctermfg=16 ctermbg=15

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tabs and keep them 2 spaces wide by default
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab

filetype plugin indent on
"set ai      " Auto indent
"set si      " Smart indent
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
"map <F3> :NERDTreeToggle <CR>

" Taglist plugin: open the taglist window
map <F2> :TlistOpen<CR>

" Turn off highlight search
nmap <silent> ,n :nohls<CR>

nmap <leader>s :set spell!<CR>

" Yank all lines
nmap ,y ggVG

" FuzzyFinder mappings
map <leader>f :FufFileWithCurrentBufferDir **/<C-M>
map <leader>b :FufBuffer<C-M>

" Create file with the name under the cursor (like the goto file, gf, command)
map <leader>gf :e <cfile><cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mail (mutt)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd BufRead,BufNewFile *mutt-* setfiletype mail


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" LaTeX
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("autocmd")
  " Auto set ft=tex for .tex files, e.g. also ft=plaintex files
  autocmd BufEnter,BufNewFile,BufRead *.tex setfiletype tex

  " Mappings
  autocmd FileType plaintex,tex map <leader>ll :! texBuild.sh compile<CR>
  autocmd FileType plaintex,tex map <leader>lv :! texBuild.sh view<CR><CR>
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SML
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType sml set makeprg=rlwrap\ mosml\ -P\ full\ '%'
autocmd FileType sml set colorcolumn=80
autocmd FileType sml highlight ColorColumn ctermbg=black
autocmd BufEnter,BufNew *.sig set ft=sml

autocmd FileType sml setlocal splitbelow
autocmd FileType sml map <F4> :call OpenInMosML()<CR>

func! OpenInMosML()
    " Expland the current file name to full path, the result is a String.
    let currentFile = expand("%:p")
    :new
    :call conque_term#open('rlwrap mosml -P full')
    :call conque_term#get_instance().writeln('use "' . currentFile . '";')
endfunc

" Some syntax highlighting in lex / yacc
autocmd BufEnter,BufNew *.grm set ft=sml
autocmd BufEnter,BufNew *.grm syn region Comment start="/\*" end="\*/"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Python
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType python setlocal makeprg=python\ -i\ %
autocmd FileType python setlocal tabstop=4 softtabstop=4 shiftwidth=4 expandtab
autocmd FileType python setlocal colorcolumn=80
autocmd FileType python highlight ColorColumn ctermbg=black

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Haskell
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType haskell setlocal makeprg=ghci\ %
autocmd FileType haskell setlocal tabstop=4 softtabstop=4 shiftwidth=4 expandtab

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" C/C++
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("autocmd")
  autocmd FileType c,cpp setlocal ts=4 sts=4 sw=4 expandtab
  autocmd FileType c,cpp map <leader>cc :! make<CR>
  autocmd FileType c,cpp map <leader>cr :! make && make run<CR>
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MIPS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType asm setlocal textwidth=89
autocmd FileType asm setlocal foldmethod=manual
autocmd FileType asm setlocal syntax=mips
