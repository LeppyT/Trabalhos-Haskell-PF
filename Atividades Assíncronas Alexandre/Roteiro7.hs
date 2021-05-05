--A diferença da estrutura do código é pois instalei uma extensão nova.

paridade:: [Int] -> [Bool]
paridade = map umpar
    where
        umpar n= even n

tresPrimeiros:: [a]->[a]
tresPrimeiros [] = [];
tresPrimeiros [x] = [x];
tresPrimeiros [x, y] = x:[y];
tresPrimeiros (x:y:z:xs) = x:[y,z];

prefixos:: [String]->[String]
prefixos = map tresPrimeiros



saudacao:: [String] -> [String]
saudacao = map (adicionaNaFrente "Oi ")
    where
        {
            adicionaNaFrente add elem = add ++ elem
        }

filtrar:: (a->Bool) -> [a] ->[a]
filtrar func [] = []
filtrar func (x:xs)
    | func x = x:filtrar func xs
    | otherwise = filtrar func xs

filtrarComp:: (a->Bool) -> [a] ->[a]
filtrarComp func li = [x|x<-li, func x]

pares :: Integral a => [a] -> [a]
pares = filter par
    where
            par n = even n

solucoes :: (Ord a, Num a) => [a] -> [a]
solucoes = filter (\x -> 5*x + 6 < x*x)

maior :: (Foldable t, Ord a) => t a -> a
maior = foldr1 compara
    where
        {
            compara x y =
                if x>y
                    then x
                else y
        }

menor_min10 :: (Foldable t, Ord b, Num b) => t b -> b
menor_min10 = foldr menor 10
    where
            menor x y =
                if x<y
                    then x
                else y

juntaSilabasPlural ::[String] -> String
juntaSilabasPlural = foldr (++) "s"

menores10 :: (Ord a, Num a) => [a] -> ([a], Int)
menores10 li = menores10Aux (filter (< 10) li)
    where
        menores10Aux li = (li, length li)

buscaElem :: (Eq t1, Num t2) => [t1] -> t1 -> (Bool, t2)
buscaElem li elem = buscaElemAux li elem 1
    where
        buscaElemAux [] elem n = (False, n)
        buscaElemAux (x:xs) elem n =
            if x==elem
                then (True, n)
            else
                buscaElemAux xs elem (n+1)