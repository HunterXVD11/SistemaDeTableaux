------------- Data -------------

data Formula = Formula { 
    label     :: Bool,
    operator  :: String,
    operand_1 :: String,
    operand_2 :: String
} deriving (Show)


----------- Functions -----------

delByIndex list i = take i list ++ drop (1 + i) list

delInitLast list = 
    
f str = [(take 1 str), (tail str)]


------------- Main -------------

main = do
    putStrLn "Digite a fórmula:"
    input <- getLine
    let formula = input
    putStrLn (">> " ++ formula)
