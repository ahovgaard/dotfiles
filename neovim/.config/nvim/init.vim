""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Neovim / VIM configuration file
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Plugin management {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Specify a directory for plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'nanotech/jellybeans.vim'

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'elixir-editors/vim-elixir'
Plug 'slashmili/alchemist.vim'

call plug#end()  " Update &runtimepath and initialize plugin system


" General {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible  " be iMproved (removed from neovim)

" Set <leader> key
let mapleader = ','
let maplocalleader = '\\'

" Fold on markers
set foldmethod=marker

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

" Use deoplete.
let g:deoplete#enable_at_startup = 1


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

" Stop highlighting on enter
map <CR> :noh<CR>

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
set laststatus=2

" vim-airline unicode symbols (requires patched font)
"let g:airline_powerline_fonts = 1

" Don't show seperators
"let g:airline_left_sep=''
"let g:airline_right_sep=''

" airline tab line
"let g:airline#extensions#tabline#enabled = 1

nnoremap <F3> :NERDTreeToggle<CR>

" Colors and fonts {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax on

" Set 256 colors (terminal independent way)
set t_Co=256

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
nnoremap <leader>f :Files<CR>
nnoremap <leader>F :History<CR>
nmap <leader>b :Buffers<CR>
nmap <leader>t :Tags<CR>

" fzf config
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

let g:fzf_layout = { 'down': '~40%' }  " fzf layout

if has('nvim')
  aug fzf_setup
    au!
    au TermOpen term://*FZF tnoremap <silent> <buffer><nowait> <esc> <c-c>
  aug END
end

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


" Terminal {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has('nvim')
  tnoremap <expr> <esc> &filetype == 'fzf' ? "\<esc>" : "\<c-\>\<c-n>"
  " tnoremap <Esc> <C-\><C-n>
  tnoremap <C-v><Esc> <Esc>
endif

augroup TerminalStuff
  au!
  autocmd TermOpen * setlocal nonumber norelativenumber
augroup END

" Add color to the terminal cursor.
if has('nvim')
  " highlight! link TermCursor Cursor
  highlight! TermCursorNC guibg=red guifg=white ctermbg=1 ctermfg=15
endif


" Elixir {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let g:alchemist_compile_basepath = '/app/proj/'
" let g:alchemist#elixir_erlang_src = '/usr/local/share/src'
if exists("$COMPILE_BASEPATH")
  let g:alchemist_compile_basepath = $COMPILE_BASEPATH
endif


" Erlang {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup ft_erlang
  au!
  au FileType erlang setlocal ts=4 sts=4 sw=4 expandtab
augroup END


" Python {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup ft_python
  au!
  au FileType python setlocal makeprg=python\ -i\ %
  au FileType python setlocal ts=4 sts=4 sw=4 expandtab
augroup END


" LaTeX {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup ft_tex
  au!
  au FileType tex map <leader>ll :! latexmk -pdf<CR>
  "au FileType tex map <leader>ll :! latexmk -xelatex<CR>
  au FileType tex map <leader>lv :! texBuild.sh view<CR><CR>
  au FileType tex setlocal foldmethod=marker
  au FileType tex setlocal tw=79

  " neomake config
  "let g:neomake_tex_enabled_makers = ['lacheck']
  "let g:neomake_tex_enabled_makers = []

  syn match texGreek '\\eps\>' contained conceal cchar=ε
augroup END


" Assembly {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup ft_nasm
  au!
  au BufRead,BufNewFile *.asm set ft=nasm
  au FileType nasm setlocal tabstop=4 softtabstop=4 shiftwidth=4 expandtab
augroup END


" C/C++ {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup ft_cpp
  au!
  au FileType c,cpp setlocal ts=4 sts=4 sw=4 expandtab
  au FileType c,cpp map <leader>cc :! make<CR>
  au FileType c,cpp map <leader>cr :! make && make run<CR>
augroup END

set cino=N-s  " no indentation in namespaces

" C++ specific Syntastic settings
"let g:syntastic_cpp_compiler = 'g++'
"let g:syntastic_cpp_compiler_options = '-std=c++11 -Wall'

"let g:neomake_cpp_gcc_maker = {
"  \ 'exe':          'g++',
"  \ 'args':         ['-fsyntax-only', '-std=c++14', '-Wall'],
"  \ 'errorformat':  '%f:%l:%c: %m',
"  \ }
"let g:neomake_cpp_enabled_makers = ['gcc']
