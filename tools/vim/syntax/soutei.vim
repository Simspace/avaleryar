if exists("b:current_syntax")
  finish
endif

syntax keyword souteiKeyword says
highlight link souteiKeyword Keyword

syntax match souteiComment "\v;.*$"
highlight link souteiComment Comment

syntax match souteiOperator "\v:-"
syntax match souteiOperator "\v,"
syntax match souteiOperator "\v\."
highlight link souteiOperator Operator

syntax match souteiNumber "\v[1-9][0-9]*"
highlight link souteiNumber Number

syntax match souteiConstant "\v\#[tf]"
highlight link souteiConstant Constant

syntax region souteiString start=/\v"/ skip=/\v\\./ end=/\v"/
highlight link souteiString String

syntax match souteiVariable "\v\?[a-zA-Z!@$%&*/<=>~_^][a-zA-Z0-9!@$%&*/<=>~_^?+-]*"
highlight link souteiVariable Identifier

syntax keyword souteiSpecial system application
highlight link souteiSpecial Special

let b:current_syntax = "soutei"
