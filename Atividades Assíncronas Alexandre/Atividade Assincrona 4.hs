lista1 = [5..1]
lista2 = ['a','c'..'e']
lista3 = [1,4..16]
lista4 = zip [1,(-2)..(-11)] [1,5..17]

intervalo a b
    | a == b = [a]
    | a > b = []
    | otherwise = [a..b]

intPar a b
    | a >= b = []
    | even a = [a, (a+2)..b]
    | otherwise [(a+1),(a+3)..b]

quadrados a b = [x*x|x<-[a..b]]

selecionaimpares li = [x| x<-li, even x]

tabuada n = [n*x | x<-[1..10]]

bissexto n = 
    if(mod n 4 != 0)
        then false
    else
        if (mod n 100 == 0 && mod n 400 != 0)
            then false
        else true

bissextos li = [x|x<-li, bissexto x]

sublistas li = [y| y<-x, x<-head li]