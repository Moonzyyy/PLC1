" Vim syntax file
" Language: Tsl


if exists("b:current_syntax")
  finish
endif

syn keyword TslTypes Tile Cell Bool Int Unit ->
syn keyword TslBools true false
syn keyword TslConditional if then else for to in

syn keyword TslTileFunctions Output Size Scale

syn keyword TslTileCreators read


syn keyword TslAssignement let in static


syn match napParens /[()]/
syn match napColon /:/


hi def link TslTypes Type
hi def link TslBools Boolean
hi def link TslConditional Conditional
hi def link TslTileFunctions Function
hi def link TslTileCreators Function