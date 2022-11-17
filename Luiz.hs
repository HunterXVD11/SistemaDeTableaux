data Formula = Formula { 
    label     :: Bool,
    operator  :: String,
    operand_1 :: String,
    operand_2 :: String
} deriving (Show)


parenPairs :: String -> [(Int, Int)]
parenPairs = go 0 []
  where
    go _ []        []         = []
    go _ (_ : _ )  []         = error "Parênteses não balanceados!"
    go j acc       ('(' : cs) =          go (j + 1) (j : acc) cs
    go j []        (')' : cs) = error "Parênteses não balanceados!"
    go j (i : is)  (')' : cs) = (i, j) : go (j + 1) is        cs
    go j acc       (c   : cs) =          go (j + 1) acc       cs


firstParenSeg :: String -> String
firstParenSeg s = f s (minimum (parenPairs s))
  where
    f s (i, j) = take (j - i + 1) (drop i s)