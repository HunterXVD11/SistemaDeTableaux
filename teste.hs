import Control.Monad

data Formula = 
  Formula { 
    label     :: Bool,
    operator  :: String,
    operand_1:: String,
    operand_2 :: String,
    final :: Bool
} deriving (Show)


data Struct = Struct {
    name :: String,
    isAthomic :: Bool
} deriving (Show)


foo xs = [x | x <- xs, x > 1]


nuu :: (Struct -> Bool) -> [Struct] -> Bool
nuu function nodeContent = and(bools)
    where
        bools = map function nodeContent

bazz nodeContent | nuu isAthomic nodeContent
                    = "Acaba recursao"

                 | otherwise
                    = "Continua recursao"