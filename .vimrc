if has("multi_byte")
  if &termencoding == ""
    let &termencoding = &encoding
  endif
  set encoding=utf-8                     " better default than latin1
  setglobal fileencoding=utf-8           " change default file encoding when writing new files
endif
set smartcase   " mixed case is case sensitive, lowercase is insensitive
set hlsearch    " highlight all search results
set ignorecase  " do case insensitive search
set incsearch   " show incremental search results as you type
set number      " display line number
set noswapfile  " disable swapfile
set viminfo+=n${HOME}/.history/vim
