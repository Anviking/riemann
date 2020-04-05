module Simple where

data NTriad = I | I# | II | II# III | IV | V | VI | VII | Minor NTriad

toN (Minor I)   = min 0
toN (Minor II)  = min 2
toN (Minor III) = min 3
toN (Minor IV)  = min 5
toN (Minor V)   = min 7
toN (Minor VI)  = min 8
toN (Minor VII) = min 10
toN I           = maj 0
toN II          = maj 2
toN III         = maj 3
toN IV          = maj 5
toN V           = maj 7
toN VI          = maj 8
toN VII         = maj 10

i = Minor I
ii = Minor II
iii = Minor III
iv = Minor IV
v = Minor V
vi = Minor VI
vii = Minor VII
