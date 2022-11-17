------------- Data -------------

data Formula = Formula { 
    label     :: Bool,
    operator  :: String,
    operand_1 :: String,
    operand_2 :: String
} deriving (Show)



----------- Functions -----------

-- Deletar item de uma lista pelo index
delByIndex list i = take i list ++ drop (1 + i) list


-- Deletar primeiro e último elementos de uma lista
delInitLast list = delByIndex (delByIndex list 0) ((length list) - 2)


-- Faz um match da posição de abertura e fechamento de um parêntese
-- ex: "a(bc(d))" -> [(1, 7), (4, 6)]
parenPairs :: String -> [(Int, Int)]
parenPairs = go 0 []
  where
    go _ []        []         = []
    go _ (_ : _ )  []         = error "Parênteses não balanceados!"
    go j acc       ('(' : cs) =          go (j + 1) (j : acc) cs
    go j []        (')' : cs) = error "Parênteses não balanceados!"
    go j (i : is)  (')' : cs) = (i, j) : go (j + 1) is        cs
    go j acc       (c   : cs) =          go (j + 1) acc       cs


-- Pega o primeiro segmento de Parênteses
-- ex: "a(bc(d))" -> "(bc(d)))"
firstParenSeg :: String -> String
firstParenSeg s = f s (minimum (parenPairs s))
  where
    f s (i, j) = take (j - i + 1) (drop i s)


-- Separar os 2 operandos da string colocando cada um como elemento da lista
-- ex: [">", "(v(b,a)),(v(c,a))"] -> [">", "v(b,a)", "v(c,a)"]
splitOperands =


-- Separar o operador da fórmula do restante dos operandos
-- ex: "(>((v(b,a)),(v(c,a))))" -> [">", "(v(b,a)),(v(c,a))"]
f str = [(take 1 str2), (delInitLast (tail str2))] where
    str2 = delInitLast str



------------- Main -------------

main = do
    putStrLn "Digite a fórmula:"
    input <- getLine
    let formula = input
    let teste = f formula
    print $ teste

