import Data.List.Split

--trataString :: String -> [String]
--trataString p1 p2 s = splitOneOf p1 s

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      " " -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

main = do
    putStrLn "Digite a fórmula a ser avaliada: "
    input <- getLine
    let formula = input
    putStrLn ("Sua fórmula é " ++ formula)
    print $ wordsWhen (==',') formula
    splitOneof "," "my,comma,separated,list"
    --print $ trataString "(" formula
