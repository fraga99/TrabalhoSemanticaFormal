-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
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

-------------------------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
-------------------------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]

exSigma2 :: Memoria
exSigma2 = [("x",3), ("temp",0), ("y",0), ("z",0)]


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


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)
smallStepE (Sub (Num n1) (Num n2), s)  = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e, s)          = let (el,sl) = smallStepE (e,s)
                                         in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)               = let (el,sl) = smallStepE (e1,s)
                                         in (Sub el e2,sl)


smallStepB :: (B,Memoria) -> (B, Memoria)
smallStepB (Not TRUE,s) = (FALSE,s)
smallStepB (Not FALSE,s) = (TRUE,s)
smallStepB (Not b,s) = let (bl,sl) = smallStepB(b,s)
                       in (Not bl,sl)

smallStepB (And TRUE b,s) = (b,s)
smallStepB (And FALSE b,s) = (FALSE,s)                       
smallStepB (And b1 b2,s )  = let (bl,sl) = smallStepB(b1,s)
                             in (And bl b2, sl)

smallStepB (Or TRUE b,s) = (TRUE,s)
smallStepB (Or FALSE b,s) = (b,s)
smallStepB (Or b1 b2,s )  = let (bl,sl) = smallStepB (b1,s)
                            in (Or bl b2,sl)

smallStepB (Leq (Num n1) (Num n2),s)
   | n1 <= n2 = (TRUE,s)
   | otherwise = (FALSE,s)
smallStepB (Leq (Num n) e,s) = let (el,sl) = smallStepE (e,s)
                               in (Leq (Num n) el, sl)
smallStepB (Leq e1 e2, s) = let (el,sl) = smallStepE (e1,s)
                            in (Leq el e2, sl)

smallStepB (Igual (Num n1) (Num n2),s)
   | n1 == n2 = (TRUE,s)
   | otherwise = (FALSE,s)
smallStepB (Igual (Num n) e,s) = let (el,sl) = smallStepE (e,s)
                                 in (Igual (Num n) el,sl)
smallStepB (Igual e1 e2, s) = let (el,sl) = smallStepE (e1,s)
                              in (Igual el e2,sl)


smallStepC :: (C,Memoria) -> (C,Memoria)
smallStepC (If TRUE c1 c2,s)  = (c1,s)
smallStepC (If FALSE c1 c2,s) = (c2,s)
smallStepC (If b c1 c2,s)     = let (bl,sl) = smallStepB (b,s)
                                in (If bl c1 c2,sl)

smallStepC (Seq Skip c2,s) = (c2,s)
smallStepC (Seq c1 c2,s)   = let (cl,sl) = smallStepC (c1,s)
                             in (Seq cl c2,sl)

smallStepC (Atrib (Var x) (Num n),s) = (Skip,mudaVar s x n)

smallStepC (Atrib (Var x) e,s) = let (el,sl) = smallStepE (e,s)
                                 in (Atrib (Var x) el,sl)
                                 
smallStepC (While b c,s) = (If b (Seq c (While b c)) Skip,s)

-----------------------------
-----  COMANDOS NOVOS  -----
-----------------------------

-- TenTimes
smallStepC (TenTimes c,s) = (Loop (Num 0) (Num 10) c,s)

-- Repeat
smallStepC (Repeat c b,s) = (Seq c (While (Not b) c),s)

-- Loop
smallStepC (Loop e1 e2 c,s) = (Seq (Atrib (Var "temp") (Sub e2 e1)) (While (Not (Leq (Var "temp") (Num 0))) (Seq c (Atrib (Var "temp") (Sub (Var "temp") (Num 1))))),s)

-- DuplaATrib
smallStepC (DuplaATrib v1 v2 e1 e2,s) = (Seq (Atrib v1 e1) (Atrib v2 e2),s)

-- AtribCond
smallStepC (AtribCond b v e1 e2,s) = (If b (Atrib v e1) (Atrib v e2),s)

-- Swap
smallStepC (Swap x y,s) = (Seq (Atrib (Var "temp") x) (Seq (Atrib x y) (Atrib y (Var "temp"))),s)

----------------------
--  INTERPRETADORES --
----------------------

--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False

interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas

isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

-- Descomentar quanto a função smallStepB estiver implementada:

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))


-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

-- Descomentar quando a função smallStepC estiver implementada:

interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM 
--- OS COMANDOS NOVOS PRINCIPALMENTE O TRATAMENTO DE EXCEÇÕES
--

----------------------------
------  MEUS TESTES  -------
----------------------------

-- TenTimes
testeTen :: C
testeTen = TenTimes (Atrib (Var "x") (Soma (Var "x") (Num 1)))

-- Repeat
testeRepeat :: C
testeRepeat = Repeat (Atrib (Var "x") (Soma (Var "x") (Num 1))) (Leq (Num 10) (Var "x"))

-- Loop
testeLoop :: C
testeLoop = Loop (Num 0) (Num 5) (Atrib (Var "x") (Soma (Var "x") (Num 2)))

-- DuplaAtrib
testeDupla :: C
testeDupla = DuplaATrib (Var "x") (Var "y") (Num 10) (Num 20)

-- AtribCond
testeCond :: C
testeCond = AtribCond (Leq (Var "x") (Num 10)) (Var "y") (Num 1) (Num 0)

-- Swap
testeSwap :: C
testeSwap = Swap (Var "x") (Var "y")

---------------------------------------------------
------  TESTES PARA TRATAMENTO DE EXCEÇÕES  -------
---------------------------------------------------
testeErro1 :: C
testeErro1 = Atrib (Var "a") (Num 10) -- irá gerar Variavel a nao definida no estado
-- interpretadorC (testeErro1, exSigma)

testeErro2 :: E
testeErro2 = Soma (Var "a") (Num 3)
-- interpretadorE (testeErro2, exSigma) -- irá gerar Variavel a nao definida no estado


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])


--- Para rodar os próximos programas é necessário primeiro implementar as regras que faltam
--- e descomentar os respectivos interpretadores


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))


---------------------------------------
------  VARIOS TIPOS DE TESTES  -------
---------------------------------------

----------------- Expressões Aritméticas -----------------

----------
-- Soma --

-- interpretadorE (progExp1, exSigma)

---------------
-- Subtração --

-- interpretadorE (Sub (Num 20) (Num 8), exSigma)

-------------------
-- Multiplicação --

-- interpretadorE (Mult (Num 7) (Num 5), exSigma)


----------------- Booleanos ------------------

-------------
-- Teste 1 --

-- interpretadorB (teste1, exSigma)

-------------
-- Teste 2 --

-- interpretadorB (teste2, exSigma)

----------------- DuplaAtrib -----------------

-------------
-- Teste 1 --

-- interpretadorC (testeDupla, exSigma)


----------------- AtribCond ------------------

-------------
-- Teste 1 --

-- interpretadorC (testeCond, exSigma)


----------------- Swap ------------------

-------------
-- Teste 1 --

-- interpretadorC (testeSwap, exSigma)

----------------- Repeat ------------------

-------------
-- Teste 1 --

-- interpretadorC (testeRepeat, exSigma2)

----------------- Loop ------------------

-------------
-- Teste 1 --

-- interpretadorC (testeLoop, exSigma2)

----------------- TenTimes ------------------

-------------
-- Teste 1 --

-- interpretadorC (testeTen, exSigma2)

----------------- Fatorial ------------------

-------------
-- Teste 1 --

-- interpretadorC (fatorial, exSigma2)

----------------- Exceção ------------------

-------------
-- Teste 1 --

-- interpretadorC (testeErro1, exSigma)