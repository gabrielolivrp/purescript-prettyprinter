module Prettyprinter.Symbols.Ascii where

import Prettyprinter.Doc (Doc(..), enclose)

angles :: forall ann. Doc ann -> Doc ann
angles = enclose langle rangle

parens :: forall ann. Doc ann -> Doc ann
parens = enclose lparen rparen

brackets :: forall ann. Doc ann -> Doc ann
brackets = enclose lbracket rbracket

braces :: forall ann. Doc ann -> Doc ann
braces = enclose lbrace rbrace

dquotes :: forall ann. Doc ann -> Doc ann
dquotes = enclose dquote dquote

lparen :: forall ann. Doc ann
lparen = Char '('

rparen :: forall ann. Doc ann
rparen = Char ')'

lbrace :: forall ann. Doc ann
lbrace = Char '{'

rbrace :: forall ann. Doc ann
rbrace = Char '}'

lbracket :: forall ann. Doc ann
lbracket = Char '['

rbracket :: forall ann. Doc ann
rbracket = Char ']'

dquote :: forall ann. Doc ann
dquote = Char '"'

comma :: forall ann. Doc ann
comma = Char ','

colon :: forall ann. Doc ann
colon = Char ':'

semi :: forall ann. Doc ann
semi = Char ';'

space :: forall ann. Doc ann
space = Char ' '

equals :: forall ann. Doc ann
equals = Char '='

langle :: forall ann. Doc ann
langle = Char '<'

rangle :: forall ann. Doc ann
rangle = Char '>'

slash :: forall ann. Doc ann
slash = Char '/'

backslash :: forall ann. Doc ann
backslash = Char '\\'

pipe :: forall ann. Doc ann
pipe = Char '|'
