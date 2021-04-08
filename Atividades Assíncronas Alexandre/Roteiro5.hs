conta_ch:: [Char] -> Int
conta_ch [] = 0
conta_ch (x:xs) = 1 + conta_ch xs

conta:: [a] -> Int
conta [] = 0
conta (x:xs) = 1 + conta xs

maior:: [Int] -> Int
maior [x] = x
maior (x:y:xs)
    | x>y = maior (x:xs)
    | otherwise = maior (y:xs)

primeiros:: [a]->Int->[a]
primeiros _ 0 = []
primeiros [] _ = []
primeiros (x:xs) n = x:primeiros xs (n-1)

pertence::Eq t=> t->[t]->Bool
pertence a [] = False
pertence a (x:xs)
    | a==x = True
    | otherwise = pertence a xs

uniaor::Eq t=> [t] -> [t] -> [t]
uniaor [] li = li
uniaor li [] = li
uniaor (x:xs) li
    | pertence x li == True = uniaor xs li
    | otherwise = x:uniaor xs li

npares:: [Int]->Int
npares [] = 0
npares (x:xs)
    | even x = 1+ npares xs
    | otherwise = npares xs

produtorio:: [Int]->Int
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

comprime:: [[a]] -> [a]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

tamanho:: [a]->Int
tamanho [] = 0
tamanho (x:xs) = 1+tamanho xs

uniao2r::Eq a=> [a]->[a]->Int->[a]
uniao2r li li2 0 = li ++ uniao2r li li2 1
uniao2r li [] 1 = []
uniao2r li (y:ys) 1 = 
    if (pertence y li == True)
        then uniao2r li ys 1
    else
        y:uniao2r li ys 1
uniao2Rec::Eq a=> [a]->[a]->[a]
uniao2Rec li li2 = uniao2r li li2 0