" Attempt to add haddock highlighting for haskell comments
" It should be placed in ~/.vim/after/syntax/haskell.vim
" Brad Bowman <haddock.vim@bereft.net>

syn match   hsHdocChunk "$\i\+" contained
syn match   hsHdocMod /"\(\i\|[.]\)\+"/ contained
syn match   hsHdocLink "'\(\i\|[.#]\)\+'" contained
syn region  hsHdocAnchor start="\\\@<!#" skip="\\#" end="\\\@<!#" contained oneline
" I think emphasis can span multiple lines
syn region  hsHdocEm start="\\\@<!/" skip="\\/" end="\\\@!/" contained oneline
syn region  hsHdocURL start="\\\@<!<" end="\\\@<!>" contained oneline
syn region  hsHdocCode start="\\\@<!@" skip="\\@" end="\\\@<!@" contained oneline
syn region  hsHdocBCodeBlock start="^@\(\s\|$\)" end="^@\s*$" contained
syn region  hsHdocLCodeBlock start="\(^\s*--\s*\)\@<=@\s*$" end="\(^\s*--\s*\)\@<=@\s*$" contained
syn match   hsHdocBHeading "^\s*\*\+" contained
syn match   hsHdocLHeading "\(^\s*--\s*\)\@<=\*\+" contained
syn match   hsHdocBTracks "^\s*>" contained
" match only the > using a look-behind
syn match   hsHdocLTracks "\(^\s*--\s*\)\@<=>" contained

" todo: numbered lists, mark haddock start separately
"syn match   hsHdocStart "\([$^|]\|\*\+\)" contained

syn cluster hsHdocSpecial 
  \ contains=hsHdocMod,hsHdocLink,hsHdocEm,hsHdocCode,hsHdocURL,
  \ hsHdocAnchor,hsHdocChunk

syn region  hsHdocDef start="^\s*\(--\)\?\s*\[" end="\]" contained contains=hsHdocSpecial

syn region  hsHdocLines start="--\s*\([$\^|]\|\*\+\)" 
                      \ skip="^\s*\(--.*\)$" 
                      \ end="^\s*\(\$\|--\)\@!" 
                      \ contains=@hsHdocSpecial,hsHdocLTracks,hsHdocLHeading,hsHdocLCodeBlock,hsHdocDef
syn region  hsHdocBlock start="{-\s*\([$\^|]\|\*\+\)" end="-}" 
                      \ contains=@hsHdocSpecial,hsHdocBTracks,hsHdocBHeading,hsHdocBCodeBlock,hsHdocDef

syn sync minlines=20

if version >= 508 || !exists("did_haddock_syntax_inits")
  if version < 508
    let did_haddock_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink hsHdocLines            hsHdoc
  HiLink hsHdocBlock            hsHdoc
  HiLink hsHdoc                 PreProc
  HiLink hsHdocAnchor           Special
  HiLink hsHdocChunk            Special
  HiLink hsHdocMod              Special
  HiLink hsHdocLink             Special
  HiLink hsHdocEm               Special
  HiLink hsHdocURL              Special
  HiLink hsHdocCode             Special
  HiLink hsHdocLHeading         Special
  HiLink hsHdocBHeading         Special
  HiLink hsHdocLTracks          Special
  HiLink hsHdocBTracks          Special
  HiLink hsHdocBCodeBlock       Special
  HiLink hsHdocLCodeBlock       Special
  HiLink hsHdocSpecial          Special

  delcommand HiLink                       
endif

" Options for vi: sw=2 sts=2 nowrap ft=vim
