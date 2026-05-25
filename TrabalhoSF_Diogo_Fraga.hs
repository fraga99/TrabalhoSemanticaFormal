-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | TenTimes C   ---- Executa o comando C 10 vezes
    | Repeat C B --- Repeat C until B: executa C enquanto B é falso
    | Loop E E C      ---- Loop e1 e2 c: executa (e2 - e1) vezes o comando C 
    | DuplaATrib E E E E -- recebe 2 variáveis e 2 expressões (DuplaATrib (Var v1) (Var v2) e1 e2) e faz v1:=e1 e v2:=e2
    | AtribCond B E E E --- AtribCond b (Var v1) e1 e2: se b for verdade, então faz v1:e1, se B for falso faz v1:=e2
    | Swap E E -- swap(x,y): troca o conteúdo das variáveis x e y 
   deriving(Eq,Show) 

-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n

---------------------------------
-- Semântica Expressões Aritméticas
---------------------------------

ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s) = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)  = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s) = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep (Div e1 e2,s)  = div (ebigStep (e1,s)) (ebigStep (e2,s))


---------------------------------
-- Semântica Expressões Booleanas
---------------------------------

bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False

bbigStep (Not b,s)
   | bbigStep (b,s) == True = False
   | otherwise              = True

bbigStep (And b1 b2,s) =
    bbigStep (b1,s) && bbigStep (b2,s)

bbigStep (Or b1 b2,s) =
    bbigStep (b1,s) || bbigStep (b2,s)

bbigStep (Leq e1 e2,s) =
    ebigStep (e1,s) <= ebigStep (e2,s)

bbigStep (Igual e1 e2,s) =
    ebigStep (e1,s) == ebigStep (e2,s)


---------------------------------
-- Semântica Comandos
---------------------------------

cbigStep :: (C,Memoria) -> (C,Memoria)

cbigStep (Skip,s) = (Skip,s)

-- IF
cbigStep (If b c1 c2,s)
   | bbigStep (b,s) == True = cbigStep (c1,s)
   | otherwise              = cbigStep (c2,s)

-- SEQUÊNCIA
cbigStep (Seq c1 c2,s) =
    let (_,s1) = cbigStep (c1,s)
    in cbigStep (c2,s1)

-- ATRIBUIÇÃO
cbigStep (Atrib (Var x) e,s) =
    (Skip, mudaVar s x (ebigStep (e,s)))

-- WHILE
cbigStep (While b c,s)
   | bbigStep (b,s) == True =
       let (_,s1) = cbigStep (c,s)
       in cbigStep (While b c,s1)

   | otherwise = (Skip,s)

-- TEN TIMES
cbigStep (TenTimes c,s) =
    executaNTimes 10 c s

-- REPEAT UNTIL
cbigStep (Repeat c b,s) =
    let (_,s1) = cbigStep (c,s)
    in if bbigStep (b,s1)
          then (Skip,s1)
          else cbigStep (Repeat c b,s1)

-- LOOP
cbigStep (Loop e1 e2 c,s) =
    executaNTimes ((ebigStep (e2,s)) - (ebigStep (e1,s))) c s

-- DUPLA ATRIBUIÇÃO
cbigStep (DuplaATrib (Var v1) (Var v2) e1 e2,s) =
    let val1 = ebigStep (e1,s)
        val2 = ebigStep (e2,s)
        s1 = mudaVar s v1 val1
        s2 = mudaVar s1 v2 val2
    in (Skip,s2)

-- ATRIBUIÇÃO CONDICIONAL
cbigStep (AtribCond b (Var v1) e1 e2,s)
   | bbigStep (b,s) == True =
       (Skip, mudaVar s v1 (ebigStep (e1,s)))

   | otherwise =
       (Skip, mudaVar s v1 (ebigStep (e2,s)))

-- SWAP
cbigStep (Swap (Var x) (Var y),s) =
    let valX = procuraVar s x
        valY = procuraVar s y
        s1 = mudaVar s x valY
        s2 = mudaVar s1 y valX
    in (Skip,s2)


---------------------------------
-- Função Auxiliar
---------------------------------

executaNTimes :: Int -> C -> Memoria -> (C,Memoria)

executaNTimes 0 c s = (Skip,s)

executaNTimes n c s =
    let (_,s1) = cbigStep (c,s)
    in executaNTimes (n-1) c s1