g" Vim syntax file
 " Language: Tsl


 if exists("b:current_syntax")
   finish
 endif

 syn keyword TslTypes Tile Bool Int String
 syn keyword TslBools True False
 syn keyword TslConditional If Then Else For To Let In Static And Or Not While

 syn keyword TslTileFunctions Scale Output Read Size Interlace Rotate90 Rotate180 Rotate270 Scale FlipX FlipY FlipXY Blank Subtile PlaceRight PlaceBelow RepeatRight RepeatDown RemoveRow RemoveColumn Swap Change + * / - Def Comma



 syn match TslParens /[()]/
 syn match TslColon /:/


 hi def link TslTypes Type
 hi def link TslBools Boolean
 hi def link TslConditional Conditional
 hi def link TslTileFunctions Function