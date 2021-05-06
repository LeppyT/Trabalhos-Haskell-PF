
tem :: [Int] -> Bool
tem [] = False
tem (x:xs) = if(pertence x xs) then True
            else tem xs

naoAtaca :: Int -> [Int] -> Bool
naoAtaca x [] = True
naoAtaca x (y:ys) = if(x /=y && (x+1) /= y && (x-1) /= y) then naoAtaca y ys
                    else False



rainhasTeste :: Int -> [[Int]]
rainhasTeste n = rainhaAux n
    where
        rainhaAux 1 = [[x] | x <-[1..n]]
        rainhaAux j = [ x:xs | x<-[1..n], xs <- rainhaAux(j-1), naoAtaca x xs, not(tem xs), not(pertence x xs)]

    

































-- EX 1
analisa_raizes:: Float -> Float -> Float -> Int
analisa_raizes 0 b c = 4
analisa_raizes a b c
    | b*b > 4*a*c = 1
    | b*b == 4*a*c = 2
    | otherwise = 3

-- EX 2
equacao::Float->Float->Float->(Float,Float)
equacao a b c
    | analisa_raizes a b c == 4 = (((-c)/b),a)
    | otherwise = 
        (
        ((-(b) + sqrt (b*b - (4*a*c)))/2*a)
        ,
        ((-(b) - (sqrt (b*b - (4*a*c))))/2*a)
        )

-- EX 3
type Date = (Int,Int,Int)
idade:: Date->Date->Int
idade (da,ma,aa) (d,m,a)
    | ma>m = aa-a
    | ma==m && da>d = aa-a 
    | otherwise = ((aa-a)-1)
busao:: Float->Date->Date->Float
busao preco (da,ma,aa) (d,m,a)
    | idade (da,ma,aa) (d,m,a) <2 = preco*(15/100)
    | idade (da,ma,aa) (d,m,a) <=10 = preco*(40/100)
    | idade (da,ma,aa) (d,m,a) >=70 = preco*(50/100)
    | otherwise = preco

--EX 4

gera1 = [x*x*x | x <- [1..20], even x, x>3 , x< 11]

gera2 = [(x, y) | x <- [1..20], x<=5, y  <- [x..x*3]]

l1=[15,16]
gera3 = [ x | x<- [1..20], x<=   head (tail l1)] ++ [ x | x<- [1..20], x <=   head l1]

gera4 = [(x,x+1) | x <- [1..20], even x, x<=10]

gera5 = [fst (x)+snd (x) | x <- gera4]

-- EX 5

contaPosM3:: [Int]->Int
contaPosM3 y = length [x | x<- y, x>(-1), mod x 3 == 0]

contaPosM3L:: [Int]->[Int]
contaPosM3L y = [x | x<- y, x>(-1), mod x 3 == 0]

-- EX 6
fatores::Int->[Int]
fatores n = [i | i<-[1..n],n`mod`i == 0]
primos:: Int->Int->[Int]
primos a b = [x | x<-[a..b], fatores x == [1,x]]

-- EX 7
mdc2:: Int->Int->Int
mdc2 m n
    | n == 0 = m
    | otherwise = mdc2 n (mod m n)
mmc:: Int->Int->Int
mmc a b =  (a*b) `div` mdc2 a b

mmc3:: Int->Int->Int->Int
mmc3 a b c = mmc (mmc a b) c

-- EX 8
somalista:: [Float] -> Float
somalista [] = 0
somalista y = somalista (tail y) + head y

serie:: Float->Int->Float
serie x n = somalista ([fromIntegral y/x | y<-[1..n], odd y] ++ [x/fromIntegral y | y<-[1..n], even y])



-- EX 9

fizzbuzzum n
    | mod n 2 == 0 && mod n 3 == 0 = "FizzBuzz"
    | mod n 2 == 0 = "Fizz"
    | mod n 3 == 0 = "Buzz"
    | otherwise = "No"

fizzbuzz n = [fizzbuzzum x|x<-[1..n]]

-- EX 10
multiplo m n =
    if (mod m n == 0) then True
                      else False

seleciona_multiplos n li = [x| x<-li, multiplo x n]

-- EX 11

unica_ocorrencia n li = 
    if (length [x|x<-li, x==n] == 1) 
        then True
    else
        False


-- EX 12 
intercala [] [] = []
intercala (x:xs) [] = (x:xs)
intercala [] (y:ys) = (y:ys)
intercala (x:xs) (y:ys) = x:y:intercala xs ys

-- EX 13

zipar [] [] = []
zipar (y:ys) [] = []
zipar [] (y:ys) = []
zipar (x:xs) (y:ys) =
    [x,y]:zipar xs ys 


-- EX 14

type Contatos = (String, String, String, String)
contatos :: [Contatos]
contatos = [("a1","a2","a3","a4"),("b1","b2","b3","b4"),("c1","c2","c3","c4"),("d1","d2","d3","d4")]

recup_nome :: String->[Contatos]->String
recup_nome email ((nome, end, tel, emailCont):xs) = 
    if(email == emailCont) 
        then nome 
    else recup_nome email xs

-- EX 15

type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27,'F'),("João", 1.85, 26, 'C'),("Maria", 1.55, 62, 'S'),("Jose", 1.78, 42, 'C'),("Paulo", 1.93, 25, 'S'),("Clara", 1.70, 33, 'C'),("Bob", 1.45, 21, 'C'),("Rosana", 1.58,39, 'S'),("Daniel", 1.74, 72, 'S'),("Jocileide", 1.69, 18, 'S') ]


altura_media :: [Pessoa] -> Float -> Float
altura_media [] media = media/fromIntegral(length pessoas)
altura_media ((nome, altura, idade, estcivil):xs) media =  altura_media xs (media+altura)  

menor_idade :: [Pessoa] -> Int
menor_idade [(nome, altura, idade, estcivil)] = idade
menor_idade ((nome, altura, idade, estcivil):(nome2, altura2, idade2, estcivil2):resto)
    | idade < idade2 = menor_idade((nome, altura, idade, estcivil):resto)
    | otherwise = menor_idade((nome2, altura2, idade2, estcivil2):resto)


maior_idade :: [Pessoa] -> (String,Char)
maior_idade [(nome, altura, idade, estcivil)] = (nome, estcivil)
maior_idade ((nome, altura, idade, estcivil):(nome2, altura2, idade2, estcivil2):resto)
    | idade > idade2 = maior_idade((nome, altura, idade, estcivil):resto)
    | otherwise = maior_idade((nome2, altura2, idade2, estcivil2):resto)


mais_50 :: [Pessoa] -> [Pessoa]
mais_50 [] = []
mais_50 ((nome, altura, idade, estcivil):xs) = 
        if (idade < 50) then mais_50 xs
        else [(nome, altura, idade, estcivil)]++ mais_50 xs
    
casadas :: [Pessoa] -> Int -> Int -> Int
casadas [] i quant = quant
casadas ((nome, altura, idade, estcivil):xs) i quant =
    if(idade > i && estcivil == 'C')
        then casadas xs i (quant+1)
        else casadas xs i quant

-- Ex 16

insere_ord::Ord a => a -> [a] -> [a]
insere_ord m [] = [m]
insere_ord m (x:xs)
    | m>x = x:insere_ord m xs
    | otherwise = (m:x:xs)

-- Ex 17

reverte:: [a]->[a]
reverte [x] = [x]
reverte (x:xs) = reverte xs ++ [x]

-- Ex 18


pertence::Eq t=> t->[t]->Bool
pertence a [] = False
pertence a (x:xs)
    | a==x = True
    | otherwise = pertence a xs

elimina_repetidos:: Eq a => [a]->[a]->[a]
elimina_repetidos [] li = []
elimina_repetidos (x:xs) li
    | pertence x li == True = elimina_repetidos xs li
    | otherwise = x:elimina_repetidos xs (x:li)

elimina_repetidos_Input:: Eq a => [a]->[a]
elimina_repetidos_Input li = elimina_repetidos li []

-- Ex 19

disponiveis = [100,50,20,10,5,2,1]


-- | testa se um numero pode ser subtraido pelo m_esimo elemento de disponiveis; marca a operacao feita;
-- | marca a linha para remover se nao
testa_e_remove:: [Int] -> Int -> [Int] -> Int -> (Int,[Int])
testa_e_remove disponiveis n li m = 
    if (n - m_esimo >= 0)
        then ((n - m_esimo),(m_esimo:li))
    else if (n == 0)
        then (n, li)
    else
        (-1,[])
    where
        m_esimo = disponiveis !! m

-- | executa testa_e_remove para todos os valores de disponiveis pela primeira vez
compreenda:: Int-> [(Int,[Int])]
compreenda n = [testa_e_remove disponiveis n [] x | x<-[0..6]]

-- | executa testa_e_remove para todos os valores de disponiveis quando a lista ja existe
compreenda2:: [(Int,[Int])] -> [(Int,[Int])]
compreenda2 [] = []
compreenda2 ((x,li):xs) = elimina_repetidos_Input ([testa_e_remove disponiveis x li k| k<-[0..6]] ++ compreenda2 xs)


-- | verifica se todos os elemontos são 0
todos_elementos [] = True
todos_elementos ((x,_):xs) = 
    if (x==0)
        then todos_elementos xs
    else
        False


-- | executa compreenda2 e remove elementos marcados até todos_elementos ser True
recursa:: [(Int,[Int])]-> [(Int,[Int])]                              
recursa li = if (todos_elementos li == True)
                    then li
               else recursa [x| x<-compreenda2 li, x /= (-1,[])]

-- | prepara a lista inicial de recursa com compreenda
printa:: Int -> [(Int,[Int])]
printa n = recursa [x| x<-compreenda n, x /= (-1,[])]


-- | formata a lista resultante
troco n = [snd x | x<- printa n]
