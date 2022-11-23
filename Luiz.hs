------------------------------------------------------
--------------------- IMPORTS ------------------------
------------------------------------------------------

import Data.List
import Data.Maybe



------------------------------------------------------
----------------------- DATA -------------------------
------------------------------------------------------

data Formula = 
  Formula { 
    label     :: Bool,
    operator  :: String,
    operand_1:: String,
    operand_2 :: String,
    isAthomic :: Bool
  } deriving (Show, Eq)


data Tree a = 
  Nulo |
  Node {
    content :: [a],
    left_child :: Tree a,
    right_child :: Tree a
  } deriving (Show, Eq)



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


removeElement :: Eq a => a -> [a] -> [a]
removeElement element list = filter (\e -> e/=element) list


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
createFormulaData :: String -> Bool -> Formula
createFormulaData formulaString label | ( ((length x) == 0) && ((length y) == 0) ) = Formula label "" op "" True
                                      | otherwise                                  = Formula label op x y False
                                      where
                                        formulaList = createFormulaList formulaString
                                        op = (formulaList !! 0)
                                        x  = (formulaList !! 1)
                                        y  = (formulaList !! 2)


-- applyRule :: Tree Formula -> [Formula]
applyRule formulaData | ( (label formulaData) == True) && ((operator formulaData) == ">" ) 
                        = [ [(createFormulaData (operand_1 formulaData) False)], [(createFormulaData (operand_2 formulaData) True)] ]

                      | ( (label formulaData) == False) && ((operator formulaData) == ">" ) 
                        = [ [(createFormulaData (operand_1 formulaData) True), (createFormulaData (operand_2 formulaData) False)] ]

                      | ( (label formulaData) == True) && ((operator formulaData) == "^" )
                        = [ [(createFormulaData (operand_1 formulaData) True), (createFormulaData (operand_2 formulaData) True)] ]

                      | ( (label formulaData) == False) && ((operator formulaData) == "^" )
                        = [ [(createFormulaData (operand_1 formulaData) False)], [(createFormulaData (operand_2 formulaData) False)] ]

                      | ( (label formulaData) == True) && ((operator formulaData) == "v" )
                        = [ [(createFormulaData (operand_1 formulaData) True)], [(createFormulaData (operand_2 formulaData) True)] ]

                      | ( (label formulaData) == False) && ((operator formulaData) == "v" )
                        = [ [(createFormulaData (operand_1 formulaData) False), (createFormulaData (operand_2 formulaData) False)] ]
                      
                      | ( (label formulaData) == True) && ((operator formulaData) == "~" )
                        = [ [(createFormulaData (operand_1 formulaData) False)] ]
                      
                      | ( (label formulaData) == False) && ((operator formulaData) == "~" )
                        = [ [(createFormulaData (operand_1 formulaData) True)] ]
                      
                      | ( (operator formulaData) == "" ) 
                        = [ [formulaData] ]

                      | otherwise
                        = error "ERRO no applyRule!"


-- initTree formulaString = Node [createFormulaData formulaString False] Nulo Nulo
initTree formulaString =
  if (length nodeChildrenContents) == 1  -- Se o nó só tem 1 filho, a árvore NÃO ramifica
    then Node {
      content = [formulaData],
      left_child = (growTree (nodeChildrenContents !! 0)),
      right_child = Nulo
    }
  else  -- se o nó tem 2 filhos, a árvore ramifica
    Node {
      content = [formulaData],
      left_child = (growTree(nodeChildrenContents !! 0)),
      right_child = (growTree(nodeChildrenContents !! 1))
    }
   where
    formulaData = createFormulaData formulaString False
    nodeChildrenContents = applyRule formulaData


findFirstCompoundFormula nodeContent = 
  if (length compoundFormulas) == 0 
    then nodeContent !! 0
  else
    compoundFormulas !! 0
  where
    compoundFormulas = [formula | formula <- nodeContent, length (operator formula) > 0]
  

verifyContentCondition :: (Formula -> Bool) -> [Formula] -> Bool
verifyContentCondition function nodeContent = and(bools)
    where
        bools = map function nodeContent


-- growTree :: [Formula] -> Tree
growTree nodeContent | verifyContentCondition isAthomic nodeContent
                      = Node {
                        content = nodeContent,
                        left_child = Nulo,
                        right_child = Nulo
                      }

                     | otherwise     
                      = if (length nodeChildrenContents) == 1  -- Se o nó só tem 1 filho, a árvore NÃO ramifica
                        then Node {
                          content = nodeContent,
                          left_child = growTree( (nodeChildrenContents !! 0) ++ filteredNodeContent ),
                          right_child = Nulo
                        }
                      else
                        Node {
                          content = nodeContent, 
                          left_child = growTree( (nodeChildrenContents !! 0) ++ filteredNodeContent ),
                          right_child = growTree( (nodeChildrenContents !! 1) ++ filteredNodeContent )
                        }
                      where
                        firstCompoundFormula = findFirstCompoundFormula nodeContent
                        filteredNodeContent = removeElement firstCompoundFormula nodeContent
                        nodeChildrenContents = applyRule firstCompoundFormula


invertLabel formulaData = Formula {
    label = not (label formulaData),
    operator = (operator formulaData),
    operand_1 = (operand_1 formulaData),
    operand_2 = (operand_2 formulaData),
    isAthomic = (isAthomic formulaData)
}


findTreeLeaves tree = case tree of
    Nulo             -> []
    Node v Nulo Nulo -> v:[]
    Node _ t1 t2     -> findTreeLeaves t1 ++ findTreeLeaves t2


validateLeafContent :: [Formula] -> Bool
validateLeafContent [] = False
validateLeafContent (formula:formulas) | (length  contradictions) > 0 = True
                                       | otherwise                    = validateLeafContent formulas
                                       where contradictions = [ x | x<-formulas, (operand_1 formula == operand_1 x) && (label formula /= label x) ]

-- [V:a, F:b, F:c, F:a]  --> [F:a]

-- [V:a, F:b, V:a, F:c]


isTautology tree = and(map validateLeafContent leaves)
  where
    leaves = findTreeLeaves tree


validateTableaux tree | (isTautology tree) = "Tautologia."
                      | otherwise = "Falsificável.\nContraprova: " ++ show(counterProofFormula)
                      where
                        counterProofIndex = elemIndex False (map validateLeafContent (findTreeLeaves tree))
                        counterProofFormula = (findTreeLeaves tree) !! (fromJust counterProofIndex)

-- validateTableaux tableauxTree = 


------------------------------------------------------
----------------------- MAIN -------------------------
------------------------------------------------------

main = do
    putStrLn "Digite a fórmula:"
    input <- getLine
    let formula = input
    let teste = validateTableaux(initTree formula)
    print $ teste
