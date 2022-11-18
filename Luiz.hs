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


numsign n | n < 0     = x
          | n > 0     = x
          | otherwise = True
            where x = False


-- Separa os 2 operandos da string colocando cada um como elemento da lista
-- ex: "(v(b,a)),(v(c,a))" ---> ["(v(b,a))", "(v(c,a))"]
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
createFormulaList :: String -> [String]
createFormulaList str = refactorFormulaList (splitOperator str)


-- createFormulaData formulaString | ( ((length x) > 3) && ((length y) > 3) )   = "FormulaFF"
--                                 | ( ((length x) > 3) && ((length y) == 3) )  = "FormulaFA"
--                                 | ( ((length x) == 3) && ((length y) > 3) )  = "FormulaAF"
--                                 | otherwise                                  = "FormulaAA" 
--                                 where
--                                   formulaList = createFormulaList formulaString
--                                   op = (formulaList !! 0)
--                                   x  = (formulaList !! 1)
--                                   y  = (formulaList !! 2)


-- createFormulaData formulaString | ( ((length x) > 3) && ((length y) > 3) )   = FormulaFF (-1) op x y
--                                 | ( ((length x) > 3) && ((length y) == 3) )  = FormulaFA (-1) op x y
--                                 | ( ((length x) == 3) && ((length y) > 3) )  = FormulaAF (-1) op x y
--                                 | otherwise                                  = FormulaAA (-1) op x y
--                                 where
--                                   formulaList = createFormulaList formulaString
--                                   op = (formulaList !! 0)
--                                   x  = (formulaList !! 1)
--                                   y  = (formulaList !! 2)


createFormulaData formulaString | ( ((length x) >= 3) && ((length y) >= 3) )   = FormulaFF (-1) op (createFormulaData x) (createFormulaData y)
                                | ( ((length x) >= 3) && ((length y) < 3) )  = FormulaFA (-1) op (createFormulaData x) y
                                | ( ((length x) < 3) && ((length y) >= 3) )  = FormulaAF (-1) op x (createFormulaData y)
                                | otherwise                                  = FormulaAA (-1) op x y
                                where
                                  formulaList = createFormulaList formulaString
                                  op = (formulaList !! 0)
                                  x  = (formulaList !! 1)
                                  y  = (formulaList !! 2)


main = do
    putStrLn "Digite a fórmula:"
    input <- getLine
    let formula = input
    let teste = createFormulaList formula
    print $ teste