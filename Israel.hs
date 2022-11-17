------------- Data -------------

data Formula = Formula { 
    label     :: Bool,
    operator  :: String,
    operand_1 :: String,
    operand_2 :: String
} deriving (Show)


----------- Functions -----------

delByIndex list i = take i list ++ drop (1 + i) list

f str = [(str !! 1), (delInitLast (tail str))] where
    delInitLast list = delByIndex (delByIndex list 0) ((length list) - 2)


------------- Main -------------

main = do
    putStrLn "Digite a fórmula:"
    input <- getLine
    let formula = input
    let teste = f formula
    print $ teste
