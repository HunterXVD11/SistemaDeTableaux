------------------------------------------------------
--------------------- IMPORTS ------------------------
------------------------------------------------------

import Data.List



------------------------------------------------------
----------------------- DATA -------------------------
------------------------------------------------------

data Formula = 
    Formula { 
      label     :: Int,
      operator  :: String,
      operand_1:: Formula,
      operand_2 :: Formula
    } | 
    Athomic {
      label :: Int,
      name :: String
    } deriving (Show)



------------------------------------------------------
-------------------- FUNCTIONS -----------------------
------------------------------------------------------

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
splitOperands :: String -> [String]
splitOperands strFormula = 
  if (length strFormula) > 3
    then [(fst dirtySeparatedOperands), (drop 1 (snd dirtySeparatedOperands))]
  else
    [ (fst(splitAt 1 strFormula)), (drop 1 (snd(splitAt 1 strFormula)))] 
  where dirtySeparatedOperands = splitAt ( (snd (minimum (parenPairs strFormula))) + 1) strFormula


-- Refaz a lista que representa fórmula, agora com os operandos separados em cada elemento
-- ex: [">", "(v(b,a)),(v(c,a))"] ---> [">","(v(b,a))","(v(c,a))"]
refactorFormulaList :: [String] -> [String]
refactorFormulaList oldFormulaList = (init oldFormulaList) ++ (splitOperands (oldFormulaList !! 1))


-- Separa o operador da fórmula do restante dos operandos
-- ex: "(>((v(b,a)),(v(c,a))))" ---> [">", "(v(b,a)),(v(c,a))"]

splitOperator str = 
  if (length str) == 1
    then [str, ""]
  else
    [(take 1 str2), (delInitLast (tail str2))]
  where str2 = delInitLast str


-- Dado o input de uma fórmula, envolta em parênteses, retorna uma lista com o operador
-- e os 2 operandos como elementos distintos dessa lista
-- ex: "(>((v(b,a)),(v(c,a))))" ---> [">","(v(b,a))","(v(c,a))"]
createFormulaList :: String -> [String]
createFormulaList str = refactorFormulaList (splitOperator str)


-- Dada uma fórmula em string, envolta em parênteses, retorna o nome do construtor utilizado
-- para estruturar a fórmula em um data Formula (serve para debugar).
-- ex: "(>((v(b,a)),(v(c,a))))" ---> FormulaFF
-- ex: "(v((a),(>(c,d))))" ---> FormulaAF
defineTypeFormulaData :: String -> String
defineTypeFormulaData formulaString | ( ((length x) > 3) && ((length y) > 3) )   = "FormulaFF"
                                    | ( ((length x) > 3) && ((length y) <= 3) )  = "FormulaFA"
                                    | ( ((length x) <= 3) && ((length y) > 3) )  = "FormulaAF"
                                    | ( ((length x) == 0) && ((length y) == 0) ) = "Athomic"
                                    | otherwise                                  = "FormulaAA" 
                                    where
                                      formulaList = createFormulaList formulaString
                                      op = (formulaList !! 0)
                                      x  = (formulaList !! 1)
                                      y  = (formulaList !! 2)


-- Dada uma fórmula em string, envolta em parênteses, retorna toda a estrutura data Formula
-- criada recursivamente, contendo fórmulas e subfórmulas até chegar nas fórmulas atômicas
-- ex: "(>((v(b,a)),(v(c,a))))" --->
--   FormulaFF {
--     label = -1,
--     operator = ">",
--     operand_1_formula = FormulaAA {
--         label = -1,
--         operator = "v",
--         operand_1_athomic = "b",
--         operand_2_athomic = ",a"
--     },
--     operand_2_formula = FormulaAA {
--         label = -1,
--         operator = "v",
--         operand_1_athomic = "c",
--         operand_2_athomic = ",a"
--     }
-- }
createFormulaData :: String -> Formula
createFormulaData formulaString | ( ((length x) == 0) && ((length y) == 0) ) = Athomic (-1) op
                                | otherwise                                  = Formula (-1) op (createFormulaData x) (createFormulaData y)
                                where
                                  formulaList = createFormulaList formulaString
                                  op = (formulaList !! 0)
                                  x  = (formulaList !! 1)
                                  y  = (formulaList !! 2)


-- (Israel que sabe explicar)
-- (serve para debugar)
-- ex: ???
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



------------------------------------------------------
----------------------- MAIN -------------------------
------------------------------------------------------

main = do
    putStrLn "Digite a fórmula:"
    input <- getLine
    let formula = input
    let teste = createFormulaList formula
    print $ teste
