""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Neovim / VIM configuration file
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Plugin management {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Specify a directory for plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'nanotech/jellybeans.vim'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

call plug#end()  " Update &runtimepath and initialize plugin system
 
" " Fuzzy finding
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
" Plug 'junegunn/fzf.vim'
 
" " Haskell
" Plug 'neovimhaskell/haskell-vim'  " Basics (better syntax highlighting etc.)
 
" Plug 'parsonsmatt/intero-neovim'
 
" "Plug 'eagletmt/neco-ghc'
 
" " Asynchronous Lint Engine
" Plug 'w0rp/ale'
 
" " Status line
" Plug 'itchyny/lightline.vim'
 
" " Completion
" "Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
 
" " Version control
" Plug 'airblade/vim-gitgutter'

"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
"Plug 'tpope/vim-fugitive'
""Plug 'benekastah/neomake'
"Plug 'morhetz/gruvbox'
"Plug 'raichoo/smt-vim'
"Plug 'ctrlpvim/ctrlp.vim'
"Plug 'shiracamus/vim-syntax-x86-objdump-d'
""Plug 'junegunn/vim-easy-align'
""Plug 'junegunn/goyo.vim'
""Plug 'majutsushi/tagbar'
"Plug 'walkie/twelf-vim'
"Plug 'vim-pandoc/vim-pandoc'
"Plug 'vim-pandoc/vim-pandoc-syntax'
"Plug 'jalvesaq/vimcmdline'
"Plug 'scrooloose/nerdtree'
"
"Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }


" General {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible  " be iMproved (removed from neovim)

" Set <leader> key
let mapleader = ","
let maplocalleader = "\\"

" Fold on markers
set foldmethod=marker

" Set spell checking region to US English
set spelllang=en_us
" Toggle spell checking on and off with ,s
"nmap <silent> <leader>s :set spell!<CR>

" Toggle spell checking on and off with ,s
nmap <silent> <leader>s :set spell!<CR>

" Set to read automatically when a file is changed from the outside
set autoread

" Set UTF-8 as standard encoding
set encoding=utf8

" Use unix as the standard file format
set ffs=unix,dos,mac

" C-n / C-p completion. The order of places to scan for completion:
" current buffer, buffers from other windows, other loaded buffers, tags
set complete=.,w,b,t

" Detect set commands in the first five or the last five lines of each file
"set modelines=5
set nomodeline  " Prevent security exploits through modelines

" Set default textwidth
set textwidth=80

" Make Vim more liberal about hidden buffers (buffers with unsaved changes that
" are not shown in a window), i.e. allow hiding buffers without saving them.
set hidden

" Disable F1 :help mapping
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" Backspace through newline and when entering insert mode
set backspace=indent,eol,start

" Disable beeps
set visualbell

" Backup
"set backup


" VIM user interface {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Show line numbers
set number

" Highlighting of the current line
set cursorline

" Ignore case when searching, unless the pattern contains an uppercase letter
set ignorecase
set smartcase

" Hightlight search results
set hlsearch

" Incremental search
set incsearch

" Command-line completion in enhanced mode
set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=longest,list,full
set wildmenu

" Show information about current command
set showcmd

" Put a dollar sign at the end of the thing being changed with the c command
set cpoptions+=$

" Invisible characters when :set list
set listchars=tab:▸\ ,eol:¬

" Show mode (insert / replace / visual) in bottom line
set noshowmode  " Unnecessary when using statusline

" Make the last window always have a status line (for vim-airline)
"set laststatus=2

" vim-airline unicode symbols (requires patched font)
"let g:airline_powerline_fonts = 1

" Don't show seperators
"let g:airline_left_sep=''
"let g:airline_right_sep=''

" airline tab line
"let g:airline#extensions#tabline#enabled = 1


" Colors and fonts {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax on

" Set 256 colors (terminal independent way)
"set t_Co=256

"let $NVIM_TUI_ENABLE_TRUE_COLOR=1

colorscheme jellybeans
"set background=dark
"let g:gruvbox_italic=1  " enable italic text
"colorscheme gruvbox
"let g:gruvbox_contrast_dark='hard'

" Modifications:
" highlight todo keyword black on yellow
hi Todo ctermfg=16 ctermbg=11
" Highlight search results black on white
hi Search ctermfg=16 ctermbg=15


" Text, tab and indent related {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tabs and keep them 2 spaces wide by default
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab

set autoindent  " Indent according to previous line
set smarttab

" Enable filytype detection and loading of plugin and indentation settings
filetype plugin indent on

"set wrap    " Wrap lines


" Moving around, tabs, windows, buffers, and files  {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Split window position, e.g. when using :split <file>
set splitbelow     " Open new windows below the current window.
set splitright     " Open new windows right of the current window.

" File browsing
let g:netrw_liststyle = 3

" Fuzzy finding files, buffers, etc.
nmap <leader>f :Files<CR>
nmap <leader>b :Buffers<CR>
nmap <leader>t :Tags<CR>

" fzf config
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

let g:fzf_layout = { 'down': '~40%' }  " fzf layout

" Closing buffers
function! BufferDelete()
  let s:total_nr_buffers = len(getbufinfo({'buflisted':1}))
  if s:total_nr_buffers == 1
    bdelete
  else
    bprevious
    bdelete #
  endif
endfunction

nmap <silent> <leader>q :call BufferDelete()<CR>

" Shortcuts {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"nmap <leader>t :TagbarToggle<CR>

"map <F5> :make<CR>

"" The_NERD_tree - A really handy tree-structured filebrowser.
"map <F3> :NERDTreeToggle<CR>

" Turn off highlight search
"nmap <silent> ,n :nohls<CR>

" Yank all lines
"nmap ,y ggVG

" Create file with the name under the cursor (like the goto file, gf, command)
"map <leader>gf :e <cfile><cr>

" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
"vmap <Enter> <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
"nmap ga <Plug>(EasyAlign)


" Asynchronous Lint Engine (ALE)  {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ale_linters = {}
let g:ale_fixers = {}


" Buffer management {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" CtrlP mappings
"let g:ctrlp_map = '<leader>f'
"nmap <leader>b :CtrlPBuffer<cr>
"
"" CtrlP ignores
"let g:ctrlp_custom_ignore = {
"  \ 'dir':  '\v[\/](\.(git|hg|svn)|\_site)$',
"  \ 'file': '\v\.(exe|so|dll|o|class|png|jpg|jpeg|aux|bbl|blg|toc|pdf)$',
"\}


" Language specific configurations {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Assembly {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"augroup ft_nasm
"  au!
"  au BufRead,BufNewFile *.asm set ft=nasm
"  au FileType nasm setlocal tabstop=4 softtabstop=4 shiftwidth=4 expandtab
"augroup END
"
"
"" C/C++ {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"augroup ft_cpp
"  au!
"  au FileType c,cpp setlocal ts=4 sts=4 sw=4 expandtab
"  au FileType c,cpp map <leader>cc :! make<CR>
"  au FileType c,cpp map <leader>cr :! make && make run<CR>
"augroup END
"
"set cino=N-s  " no indentation in namespaces
"
"" C++ specific Syntastic settings
""let g:syntastic_cpp_compiler = 'g++'
""let g:syntastic_cpp_compiler_options = '-std=c++11 -Wall'
"
""let g:neomake_cpp_gcc_maker = {
""  \ 'exe':          'g++',
""  \ 'args':         ['-fsyntax-only', '-std=c++14', '-Wall'],
""  \ 'errorformat':  '%f:%l:%c: %m',
""  \ }
""let g:neomake_cpp_enabled_makers = ['gcc']
"
"
"" Erlang {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"augroup ft_erlang
"  au!
"  au FileType erlang setlocal ts=4 sts=4 sw=4 expandtab
"augroup END
"
"
"" Haskell {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set async lint engine checkers/linters
let g:ale_linters.haskell = ['stack-ghc-mod']

" , 'hlint']

" Prefer starting Intero manually (faster startup times)
let g:intero_start_immediately = 0

" Use ALE (works even when not using Intero)
let g:intero_use_neomake = 0

" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc


"" Align 'then' two spaces after 'if'
""let g:haskell_indent_if = 2
"" Indent 'where' block two spaces under previous body
""let g:haskell_indent_before_where = 2
"" Allow a second case indent style (see haskell-vim README)
""let g:haskell_indent_case_alternative = 1
"" Only next under 'let' if there's an equals sign
""let g:haskell_indent_let_no_in = 0
"
"
""let g:neomake_haskell_enabled_makers = ['hlint']
""
""let g:tagbar_type_haskell = {
""    \ 'ctagsbin'  : 'hasktags',
""    \ 'ctagsargs' : '-x -c -o-',
""    \ 'kinds'     : [
""        \  'm:modules:0:1',
""        \  'd:data: 0:1',
""        \  'd_gadt: data gadt:0:1',
""        \  't:type names:0:1',
""        \  'nt:new types:0:1',
""        \  'c:classes:0:1',
""        \  'cons:constructors:1:1',
""        \  'c_gadt:constructor gadt:1:1',
""        \  'c_a:constructor accessors:1:1',
""        \  'ft:function types:1:1',
""        \  'fi:function implementations:0:1',
""        \  'o:others:0:1'
""    \ ],
""    \ 'sro'        : '.',
""    \ 'kind2scope' : {
""        \ 'm' : 'module',
""        \ 'c' : 'class',
""        \ 'd' : 'data',
""        \ 't' : 'type'
""    \ },
""    \ 'scope2kind' : {
""        \ 'module' : 'm',
""        \ 'class'  : 'c',
""        \ 'data'   : 'd',
""        \ 'type'   : 't'
""    \ }
""\ }
"
"" ale
""let g:ale_linters = {'haskell': ['stack-ghc-mod', 'hlint']}
""let g:ale_linters = {'haskell': []}
"
""" ----- parsonsmatt/intero-neovim -----
""
""" Prefer starting Intero manually (faster startup times)
""let g:intero_start_immediately = 0
""" Use ALE (works even when not using Intero)
""let g:intero_use_neomake = 0
""
""augroup interoMaps
""  au!
""
""  au FileType haskell nnoremap <silent> <leader>io :InteroOpen<CR>
""  au FileType haskell nnoremap <silent> <leader>iov :InteroOpen<CR><C-W>H
""  au FileType haskell nnoremap <silent> <leader>ih :InteroHide<CR>
""  au FileType haskell nnoremap <silent> <leader>is :InteroStart<CR>
""  au FileType haskell nnoremap <silent> <leader>ik :InteroKill<CR>
""
""  au FileType haskell nnoremap <silent> <leader>wr :w \| :InteroReload<CR>
""  au FileType haskell nnoremap <silent> <leader>il :InteroLoadCurrentModule<CR>
""  au FileType haskell nnoremap <silent> <leader>if :InteroLoadCurrentFile<CR>
""
""  au FileType haskell map <leader>t <Plug>InteroGenericType
""  au FileType haskell map <leader>T <Plug>InteroType
""  au FileType haskell nnoremap <silent> <leader>it :InteroTypeInsert<CR>
""
""  au FileType haskell nnoremap <silent> <leader>jd :InteroGoToDef<CR>
""  au FileType haskell nnoremap <silent> <leader>iu :InteroUses<CR>
""  au FileType haskell nnoremap <leader>ist :InteroSetTargets<SPACE>
""augroup END
"
"let g:LanguageClient_serverCommands = {
"    \ 'haskell': ['hie', '--lsp'],
"    \ }
"
"
"
"" Java {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"augroup ft_java
"  au!
"  au FileType java setlocal ts=4 sts=4 sw=4 expandtab
"augroup END
"
"
"" LaTeX {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"augroup ft_tex
"  au!
"  au FileType tex map <leader>ll :! latexmk -pdf<CR>
"  "au FileType tex map <leader>ll :! latexmk -xelatex<CR>
"  au FileType tex map <leader>lv :! texBuild.sh view<CR><CR>
"  au FileType tex setlocal foldmethod=marker
"  au FileType tex setlocal tw=79
"
"  " neomake config
"  "let g:neomake_tex_enabled_makers = ['lacheck']
"  "let g:neomake_tex_enabled_makers = []
"
"  syn match texGreek '\\eps\>' contained conceal cchar=ε
"augroup END
"
"
"" Python {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"augroup ft_python
"  au!
"  au FileType python setlocal makeprg=python\ -i\ %
"  au FileType python setlocal ts=4 sts=4 sw=4 expandtab
"augroup END
"
"
"" SMT {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"augroup ft_smt
"  au!
"  au BufRead,BufNewFile *.rada set ft=smt
"augroup END
"
"" Pandoc {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"let g:pandoc#folding#level = 3
"let g:pandoc#syntax#conceal#use = 0
"let g:pandoc#spell#enabled = 0
