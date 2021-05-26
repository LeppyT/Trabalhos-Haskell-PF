l1 :: [Int]
l1=[1..2000]
l2 :: [Int]
l2=[2000,1999..1]
l3 :: [Int]
l3=l1++[0]
l4 :: [Int]
l4=0 : l2
l5 :: [Int]
l5=l1++[0]++l2
l6 :: [Int]
l6=l2++[0]++l1
l7 :: [Int]
l7=l2++[0]++l2
x1 :: [Int]
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2 :: [Int]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3 :: [Int]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4 :: [Int]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5 :: [Int]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6 :: [Int]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 :: [Int]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]



--EX 1
-- Analise
-- bolha Original
    -- para x1 -> trocas: 0, tempo: rápido
    -- para x2 -> trocas: 190, tempo: rápido
    -- para l2 -> trocas: 1999000, tempo: rápido
--Bolha V1
    -- para x1 -> trocas: 0, tempo: rápido
    -- para x2 -> trocas: 190, tempo: instantaneo
    -- para l2 -> trocas: 1999000, tempo: demora mais para calcular, porem apos feitas as operações o resultado sai rápido
--Bolha V2
    -- para x1 -> trocas: 0, tempo: rápido
    -- para x2 -> trocas: 190, tempo: instantaneo
    -- para l2 -> trocas: 1999000, tempo: demora mais para calcular e mostrar os resultados
--Conclusão 
    -- Em todas as versões a quantidade de troca permanece a mesma, o que muda é o tempo de processamento e como são feitas as trocas


bolha :: (Num b, Ord a) => [a] -> ([a], b)
bolha [] = ([],0)
bolha lista = bolhaOrd lista (length lista) 0
bolhaOrd :: (Num t1, Num t2, Ord a2, Eq t1) => [a2] -> t1 -> t2 -> ([a2], t2)
bolhaOrd lista 0 n = (lista,n)
bolhaOrd lista m n = bolhaOrd (troca lista) (m-1) (contador lista+n)

troca :: Ord a => [a] -> [a]
troca [x] = [x]
troca (x:y:zs)
    | x>y = y:troca (x:zs)
    | otherwise = x:troca (y:zs)

contador :: (Num a1, Ord a2) => [a2] -> a1
contador [x] = 0
contador (x:y:zs)
    | x>y = 1+contador (x:zs)
    | otherwise = 0+contador (y:zs)

bolhaV1 :: (Num b, Ord a) => [a] -> ([a], b)
bolhaV1 [] = ([],0)
bolhaV1 lista = bolhaOrdV1 lista (length lista) 0
bolhaOrdV1 :: (Num t1, Num t2, Ord a2, Eq t1) => [a2] -> t1 -> t2 -> ([a2], t2)
bolhaOrdV1 lista 0 n = (lista,n)
bolhaOrdV1 lista m n =
    if contador lista == 0
        then (lista,n)
    else
        bolhaOrdV1 (troca lista) (m-1) (contador lista+n)


bolhaV2::[Int]->([Int],Int)
bolhaV2 [] = ([],0)
bolhaV2 lista = bolhaOrdV2 lista (length lista) 0

bolhaOrdV2 :: (Ord a3, Num a2, Num t, Eq a2) => [a3] -> a2 -> t -> ([a3], t)
bolhaOrdV2 lista 0 n = (lista,n)
bolhaOrdV2 lista m n =
    if contadorV2 lista m == 0
        then (lista,n)
    else
        bolhaOrdV2 (trocaV2 lista m) (m-1) (contadorV2 lista m+n)



trocaV2 :: (Num a1, Ord a2, Eq a1) => [a2] -> a1 -> [a2]
trocaV2 [x] n = [x]
trocaV2 li 0 = li
trocaV2 (x:y:zs) n
    | x>y = y:trocaV2 (x:zs) (n-1)
    | otherwise = x:trocaV2 (y:zs) (n-1)



contadorV2 :: (Num a1, Num a2, Ord a3, Eq a2) => [a3] -> a2 -> a1
contadorV2 [x] n = 0
contadorV2 li 0 = 0
contadorV2 (x:y:zs) n
    | x>y = 1+contadorV2 (x:zs) (n-1)
    | otherwise = 0+contadorV2 (y:zs) (n-1)



--EX 2
-- Analise
-- Selection Original
    -- para x1 -> trocas: 20, tempo: rápido
    -- para x2 -> trocas: 20, tempo: rápido
    -- para l2 -> trocas: ?, tempo: não teve resultado devido a demora 
--Selection V1
    -- para x1 -> trocas: 20, tempo: instantaneo
    -- para x2 -> trocas: 20, tempo: instantaneo
    -- para l2 -> trocas: 2000, tempo: apesar de aparecer o numero de trocas total, a ordem não é mostrada
--Selection V2
    -- para x1 -> trocas: 20, tempo: rápido
    -- para x2 -> trocas: 20, tempo: rápido
    -- para l2 -> trocas: ?, tempo: não teve resultado devido a demora
--Conclusão 
    -- Em todas as versões a quantidade de troca permanece a sendo ela exatamente igual ao número de elementos na lista, o que muda em teoria é o tempo de processamento mas como para listas pequenas o resultado é muito rápido e em listas grandes não há resposta, então, não foi possível analizar o tempo 


selection:: [Int] -> ( Int, [Int])
selection li = doisUltimos (selSort (li,0, []))

doisUltimos (x,y,z) = (y,z)
minimo :: (Ord a) => [a] -> a
minino [] = undefined
minimo [x] = x
minimo (x:xs)
    | x <= minimo xs = x
    | otherwise = minimo xs

remove a [] = []
remove a (x:xs)
    | a==x = xs
    | otherwise = x:remove a xs
selSort :: (Num b, Ord a) => ([a], b, [a]) -> ([a], b, [a])
selSort ([x],n, li) = ([x],n+1, li++[x])
selSort (xs, n, listareal) = selSort (removido, n+1, listareal++[x])
    where{
            removido = remove (minimo xs) xs;
            x = minimo xs;
    }

embeleza :: ((a1, b1), a2, b2) -> (a2, b2)
embeleza ((_, _), n, atual) = (n, atual)

selectionV1 :: (Num a, Ord b) => [b] -> (a, [b])
selectionV1 li = embeleza (selSort1 (removeMenor (li, []), 0, []))

selSort1 :: (Num a1, Ord a2) => (([a2], a2), a1, [a2]) -> (([a2], a2), a1, [a2])
selSort1 (([],m),n, atual) = (([],m),n+1, atual++[m])
selSort1 ((li,m), n, atual) = selSort1 (removeMenor (li,[]), n+1, atual++[m])

removeMenor :: (Ord a) => ([a],[a]) ->([a], a)
removeMenor ([x],li) = (li, x);
removeMenor (x:y:xs, li)
    | x<y = removeMenor (x:xs, y:li)
    | otherwise = removeMenor (y:xs, x:li)

selectionV2 li = doisUltimos (selSort2 (li,0, []))

selSort2 :: (Num b, Ord a) => ([a], b, [a]) -> ([a], b, [a])
selSort2 ([x],n, li) = ([x],n+1, li++[x])
selSort2 (xs, n, listareal) = selSort (removido, n+1, listareal++[foldr1 (min) xs])
    where{
            removido = filter (\x-> x /=(foldr1 (min) xs)) xs;
    }


-- EX6 

data Exp =
    Val Float -- um numero
    | Add Exp Exp -- soma de duas expressoes
    | Sub Exp Exp -- subtração de duas expressoes
    | Mult Exp Exp
    | Div Exp Exp

avalia :: Exp -> Float
avalia (Val x) = x
avalia (Add exp1 exp2) = avalia exp1 + avalia exp2
avalia (Sub exp1 exp2) = avalia exp1 - avalia exp2
avalia (Mult exp1 exp2) = avalia exp1 * avalia exp2
avalia (Div exp1 exp2) = avalia exp1 / avalia exp2

expressao1 :: Exp
expressao1 =
    Div
    (Mult (Add (Val 3) (Val 12)) (Sub (Val 15) (Val 5)))
     (Mult (Val 1) (Val 3))
expressao2 :: Exp
expressao2 = Sub
    (Val 0)
    (
        Mult
        (
            Sub (Add (Val 6)(Val 8)) (Add (Val 5) (Val 1))
        )
        (
            Add (Val 2) (Div (Val 6) (Val 2))
        )
    )

-- EX 7

data Jogada =
    Pedra | Papel | Tesoura

vence :: Jogada -> Jogada -> Bool
vence Papel Pedra = True
vence Papel Papel = True
vence Papel Tesoura = False

vence Pedra Pedra = True
vence Pedra Papel = False
vence Pedra Tesoura = True

vence Tesoura Pedra = False
vence Tesoura Papel = True
vence Tesoura Tesoura = True

vencedora :: (Jogada, Jogada) -> Jogada
vencedora (j1, j2)
    | vence j1 j2 = j1
    | otherwise = j2

vencedoras :: [(Jogada, Jogada)] -> [Jogada]
vencedoras = map vencedora

--Ex 8

data Nebuloso = Verdadeiro | Falso | Talvez Float

fuzzifica :: Float -> Nebuloso
fuzzifica f
    | f>=1 = Verdadeiro
    | f<=0 = Falso
    | otherwise = Talvez f

verificaAlto :: Float -> Nebuloso
verificaAlto h = fuzzifica ((h-1.7)/0.2)

verificaBarato :: Float -> Nebuloso
verificaBarato p = fuzzifica ((50000-p)/ 20000)

--EX 9

data Estudante = Colegio Int String Int Float Float | Universitario String String Int Float Int

listaEstudantes :: [Estudante]
listaEstudantes = [Universitario "UFU" "direito" 11911678 1.65 19,
    Universitario "UNA" "computacao" 11911678 1.62 28,
    Universitario "UNITRI" "medicina" 11911679 1.55 22,
    Universitario "UFU" "musica" 11911680 1.96 38,
    Universitario "UFU" "direito" 11911681 1.75 18,
    Universitario "UNITRI" "medicina" 11911682 1.85 24,
    Universitario "UNA"" medicina" 11911683 1.72 22,
    Universitario "UNITRI" "medicina" 11911684 1.89 18,
    Universitario "UFU" "musica" 11911685 1.78 21,
    Universitario "UNA" "computacao" 11911686 1.92 22,
    Colegio 1 "nacional" 11542 1.65 58,
    Colegio 3 "olimpo" 11543 1.62 62,
    Colegio 2 "gabarito" 11544 1.55 38,
    Colegio 2 "olimpo" 11545 1.96 91,
    Colegio 3 "nacional" 11546 1.75 54,
    Colegio 1 "gabarito" 11547 1.85 75,
    Colegio 1 "gabarito" 11548 1.72 22,
    Colegio 2 "nacional" 11549 1.89 12,
    Colegio 3 "olimpo" 11550 1.78 60,
    Colegio 1 "olimpo" 11551 1.92 85]


descobreAlto :: Estudante -> (Int, Nebuloso)
descobreAlto (Colegio a b c d e) = (c, verificaAlto d)
descobreAlto (Universitario a b c d e) = (c, verificaAlto d)
descobreAltos :: [Estudante] -> [(Int,Nebuloso)]
descobreAltos [] = []
descobreAltos li = map descobreAlto li

--EX10

data ArvoreBinInt =
    Nulo
    | No Int ArvoreBinInt ArvoreBinInt
        deriving Show


folhas :: ArvoreBinInt -> [Int]
folhas Nulo = []
folhas (No x Nulo Nulo) = [x]
folhas (No x esq dir) = folhas esq ++ folhas dir

somaNosInternos :: ArvoreBinInt -> Int
somaNosInternos Nulo = 0
somaNosInternos (No x esq dir)= foldr (+) 0 (nosInternos (No x esq dir))

nosInternos :: ArvoreBinInt -> [Int]
nosInternos Nulo = []
nosInternos (No x Nulo Nulo)= []
nosInternos (No x esq dir)= nosInternos esq ++ nosInternos dir ++ [x]

pertence :: ArvoreBinInt -> Int -> Bool
pertence Nulo _ = False
pertence (No x esq dir) numero = 
    if numero /= x 
        then pertence esq numero || pertence dir numero
    else
        True

arvTeste :: ArvoreBinInt
arvTeste = No 14 (No 4 (No 3 Nulo Nulo) (No 9 (No 7 (No 5 Nulo Nulo) Nulo) Nulo))(No 15 Nulo (No 18 (No 16 Nulo (No 17 Nulo Nulo))(No 20 Nulo Nulo)))

