import Data.List

data Formula = 
    FormulaFF { 
      label     :: Int,
      operator  :: String,
      operand_1_formula:: Formula,
      operand_2_formula :: Formula
    } | 
    FormulaAA {
      label     :: Int,
      operator  :: String,
      operand_1_athomic :: String,
      operand_2_athomic :: String
    } | 
    FormulaAF {
      label     :: Int,
      operator  :: String,
      operand_1_athomic :: String,
      operand_2_formula :: Formula
    } | 
    FormulaFA {
      label     :: Int,
      operator  :: String,
      operand_1_formula :: Formula,
      operand_2_athomic :: String
    } deriving (Show)


----------- Functions -----------

-- Deleta item de uma lista pelo index
-- ex: [1, 2, 3, 4]  2 ---> [1, 2, 4] 
delByIndex :: [a] -> Int -> [a]
delByIndex list i = take i list ++ drop (1 + i) list


-- Deleta primeiro e último elementos de uma lista
-- ex: [1, 2, 3, 4] ---> [2, 4]
delInitLast :: [a] -> [a]
delInitLast list = delByIndex (delByIndex list 0) ((length list) - 2)


-- Adiciona um elemento ao final de uma lista
-- ex: 4  [1, 2, 3] ---> [1, 2, 3, 4]
appendElement :: a -> [a] -> [a]
appendElement el [] = [el]
appendElement el (x:xs) = x : appendElement el xs


-- Faz um match da posição de abertura e fechamento de um parêntese
-- ex: "a(bc(d))" ----> [(1, 7), (4, 6)]
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
-- ex: "a(bc(d))" ---> "(bc(d)))"
firstParenSeg :: String -> String
firstParenSeg s = f s (minimum (parenPairs s))
  where
    f s (i, j) = take (j - i + 1) (drop i s)


-- Separa os 2 operandos da string colocando cada um como elemento da lista
-- ex: "(v(b,a)),(v(c,a))" ---> ["(v(b,a))", "(v(c,a))"]
-- PRECISA IMPLEMENTAR A CONDIÇÃO CASO O TAMANHO DA STRING SEJA == 3
splitOperands :: String -> [String]
splitOperands strFormula = 
  if (length strFormula) > 3
    then [(fst dirtySeparatedOperands), (drop 1 (snd dirtySeparatedOperands))]
  else
    [ (fst(splitAt 1 strFormula)), (snd(splitAt 1 strFormula))] 
  where dirtySeparatedOperands = splitAt ( (snd (minimum (parenPairs strFormula))) + 1) strFormula


-- Refaz a lista que representa fórmula, agora com os operandos separados em cada elemento
-- ex: [">", "(v(b,a)),(v(c,a))"] ---> [">","(v(b,a))","(v(c,a))"]
refactorFormulaList :: [String] -> [String]
refactorFormulaList oldFormulaList = (init oldFormulaList) ++ (splitOperands (oldFormulaList !! 1))


-- Separa o operador da fórmula do restante dos operandos
-- ex: "(>((v(b,a)),(v(c,a))))" ---> [">", "(v(b,a)),(v(c,a))"]
splitOperator :: [a] -> [[a]]
splitOperator str = [(take 1 str2), (delInitLast (tail str2))] where
    str2 = delInitLast str

-- Dado o input de uma fórmula, envolta em parênteses, retorna uma lista com o operador
-- e os 2 operandos como elementos distintos dessa lista
-- ex: "(>((v(b,a)),(v(c,a))))" ---> [">","(v(b,a))","(v(c,a))"]

processFormula ::  String -> [String]
processFormula str = 
    if (length str) >= 8
        then
        if (length x) >= 8 && (length y) >= 8
            then (refactorFormulaList (splitOperator str) ++ processFormula (refactorFormulaList (splitOperator str) !! 1)++ processFormula (refactorFormulaList (splitOperator str) !! 2))
        else if (length x) >= 8 && (length y) < 8
            then (refactorFormulaList (splitOperator str) ++ processFormula (refactorFormulaList (splitOperator str) !! 1))
        else if (length x) < 8 && (length y) >= 8
            then (refactorFormulaList (splitOperator str) ++ processFormula (refactorFormulaList (splitOperator str) !! 2))
        else
            (refactorFormulaList (splitOperator str))
    else
        (refactorFormulaList (splitOperator str))
    where 
        x = (refactorFormulaList (splitOperator str) !! 1)
        y = (refactorFormulaList (splitOperator str) !! 2)
    

        

insertStruct obj = 
    --if (length x) > 3 && (length x > 3)
        --then FormulaFF (-1) op x y

    --if (length x) == 3 && (length x == 3)
        FormulaAA (-1) op x y

    --else if (length x > 3) && (length x == 3)
        --then FormulaFA (-1) op x y
        
    --else
        --FormulaAF (-1) op x y

    where 
        op = (refactorFormulaList (splitOperator obj) !! 0)
        x = (refactorFormulaList (splitOperator obj) !! 1)
        y = (refactorFormulaList (splitOperator obj) !! 2)

--insertRule :: Struct -> Arvore -- Mudar para arvore quando criar o construtor
--insertRule no =
  --   if (take 1 no2) == "v" 
  --      then 
  --   else if (take 1 no2) == "^" 
  --      then 
   --  else 
   --  where no2 = delInitLast no



main = do
    putStrLn "Digite a fórmula:"
    input <- getLine
    let formula = input
    let teste = processFormula formula
    print $ teste