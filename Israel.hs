main = do
    putStrLn "Qual o seu nome?"
    input <- getLine
    let nome = input
    putStrLn ("Meu nome é " ++ nome)
