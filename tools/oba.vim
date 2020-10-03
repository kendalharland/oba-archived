" Vim syntax file
" Language: Oba
" Maintainer: Kendal Harland
" Latest Revision: 3 October 2020
"
" To install:
" 1. Copy this file to ~/.vim/syntax/oba.vim
" 2. Put the following into ~/.vim/ftdetect/oba.vim: au BufRead,BufNewFile *.oba set filetype=oba


if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword obaLanguageKeywords debug else false fn if let match true while 

" Matches
syn match obaOp      '[+-/*=]'
syn match obaDelim   '[{}|()]'
syn match obaName    '[a-zA-Z_][a-zA-Z0-9_]*'
syn match obaNumber  '\d\+'

" Regions
syn region obaComment start='//' end='\n'
syn region obaString  start='"' end='"'

let b:current_syntax = "oba"
hi def link obaLanguageKeywords Keyword
hi def link obaOp               Operator
hi def link obaDelim            Delimiter
hi def link obaName             Identifier
hi def link obaComment          Comment
hi def link obaString           String
hi def link obaNumber           Number

