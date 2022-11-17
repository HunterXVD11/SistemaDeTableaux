------------- Data -------------

data Formula = Formula { 
    label     :: Bool,
    operator  :: String,
    operand_1 :: String,
    operand_2 :: String
} deriving (Show)


----------- Functions -----------

delByIndex list i = take i list ++ drop (1 + i) list

delInitLast list = delByIndex (delByIndex list 0) ((length list) - 2)

charToString :: Char -> String
charToString c = [c]

f str = [(take 1 str2), (delInitLast (tail str2))] where
    str2 = delInitLast str


------------- Main -------------

main = do
    putStrLn "Digite a fórmula:"
    input <- getLine
    let formula = input
    let teste = f formula
    print $ teste

