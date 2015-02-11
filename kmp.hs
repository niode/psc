search::[Char]->[Char]->Maybe Int
search w s = search' 0 s w w t t where t = table w

search'::Int->[Char]->[Char]->[Char]->[Int]->[Int]->Maybe Int
search' n _ [] _ _ _ = Just n
search' n [] _ _ _ _ = Nothing
search' n (s:ss) (w:ww) ow (t:tt) ot =
  if w == s
    then search' n ss ww ow tt ot
    else search' (n + (length nt) - (length tt)) ss nw ow nt ot
  where nw = drop t ow
        nt = drop t ot

table::[Char]->[Int]
table [a] = [-1]
table [a, b] = [-1, 0]
table (a:b:cs) = reverse $ table' (a:b:cs) [0,-1] [a] cs

table'::[Char]->[Int]->[Char]->[Char]->[Int]
table' _ xs _ [] = xs
table' s (x:xs) (cnd:cs) (pos:ps) =
  if cnd == pos
    then table' s ((x + 1):x:xs) cs ps
    else table' s (0:x:xs) s ps
