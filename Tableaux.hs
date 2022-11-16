------------- Data -------------

data Formula = Formula { 
    label     :: Bool,
    operator  :: String,
    operand_1 :: String,
    operand_2 :: String
} deriving (Show)


------------- Main -------------

main = do
    putStrLn "Qual o seu nome?"
    input <- getLine
    let nome = input
    putStrLn ("Meu nome é " ++ nome)
