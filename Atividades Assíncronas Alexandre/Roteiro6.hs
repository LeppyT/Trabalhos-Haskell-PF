bissextos [] = []
bissextos (x:xs)
    | bissexto x == False = bissextos xs
    | otherwise = x:bissextos xs
    where
        {bissexto n = if(mod n 4 /= 0)
                        then False
                    else
                        if (mod n 100 == 0 && mod n 400 /= 0)
                            then False
                        else True;}
        
valida:: Int->Int->Int->Bool
valida dia mes ano
    | mes<1 || mes >12 = False
    | mes == 2 && (bissexto ano == True) && (dia==29) = True
    | mes == 2 && (dia<1 || dia>28) = False
    | (pertence mes lista1 == True) && (dia<1 || dia>30) = False
    | (pertence mes lista2 == True) && (dia<1 || dia>31) = False
    | otherwise = True
    where
        {
        pertence::Int->[Int]->Bool;
        pertence elem [] = False;
        pertence elem (x:xs) = 
        if (elem == x)
            then True
            else pertence elem xs;
        lista1 = [1,3,5,7,8,10,12];
        lista2 = [4,6,9,11];
        bissexto n = 
        if(mod n 4 /= 0)
            then False
        else
            if (mod n 100 == 0 && mod n 400 /= 0)
                then False
            else True;}

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]


atrasados :: Emprestimos -> Data -> Emprestimos
atrasados emp dat = [x|x<-emp, atrasado x dat] 
    where
        {
        atrasado (_,_,_,(d1,m1,a1),_) (d,m,a)   
            | a1>a = False
            | a1<a = True
            | m1>m = False
            | m1<m = True
            | d1<d = True
            | otherwise = False;}

fatorial n = prodIntervalo 2 n
    where
        {
        prodIntervalo m n = produtorio [m..n]
            where
                {produtorio [] = 1;
                produtorio (x:xs) = x * produtorio xs;};}
    



fibo2 n = fibo n
    where
        {fibo 0 = 0;
        fibo 1 = 1;
        fibo n = snd(passo (fibo (n-2), fibo(n-1)))
            where
                {passo (x,y) = (y,(x+y))};}

bissextos2 [] = []

bissextos2 (x:xs)
             | let {bissexto n = 
                if(mod n 4 /= 0)
                   then False
                else
                  if (mod n 100 == 0 && mod n 400 /=0)
                      then False
                 else True} in bissexto x == False = bissextos xs
             | otherwise = x:bissextos xs
      
valida2 dia mes ano
    | mes<1 || mes >12 = False
    |let {bissexto n = 
        if(mod n 4 /= 0)
            then False
        else
            if (mod n 100 == 0 && mod n 400 /= 0)
                then False
            else True} in mes == 2 && (bissexto ano == True) && (dia==29) = True
    | mes == 2 && (dia<1 || dia>28) = False
    | let {
        pertence::Int->[Int]->Bool;
        pertence elem [] = False;
        pertence elem (x:xs) = 
        if (elem == x)
            then True
            else pertence elem xs;
            lista1 = [1,3,5,7,8,10,12]} in (pertence mes lista1 == True) && (dia<1 || dia>30) = False
    | let {
        pertence::Int->[Int]->Bool;
        pertence elem [] = False;
        pertence elem (x:xs) = 
        if (elem == x)
            then True
            else pertence elem xs;
            lista2 = [4,6,9,11]} in (pertence mes lista2 == True) && (dia<1 || dia>31) = False
    | otherwise = True

atrasados2 emp dat = 
    let  {
        atrasado (_,_,_,(d1,m1,a1),_) (d,m,a)   
         | a1>a = False
         | a1<a = True
         | m1>m = False
         | m1<m = True
         | d1<d = True
         | otherwise = False} 
         in [x|x<-emp, atrasado x dat]


fatorial2 n = let
    {
    prodIntervalo m n =
    let {produtorio [] = 1;
        produtorio (x:xs) = x * produtorio xs }
        in produtorio [m..n]  } in prodIntervalo 2 n


fibo2_2 n = 
    let
    {    fibo 0 = 0;
        fibo 1 = 1;
        fibo n = let {passo (x,y) = (y,(x+y))}
        in snd(passo (fibo (n-2), fibo(n-1))) }
    in fibo n


lambda2 = ((\f->(\x->f(f x)))(\y->y*y)3)
lambda3 = ((\f->(\x->f(f x)))(\y->y+y)5)
lambda5 = ((\f->(\x->f(f(f x))))(\y->y*y)2)
lambda6 = ((\x-> \y->(x+(\x->x-3) y))5 6)