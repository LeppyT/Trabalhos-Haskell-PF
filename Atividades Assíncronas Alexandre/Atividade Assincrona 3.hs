ou (True,_) = True
ou (_,True) = True
ou (False,False) = False

ou2 (x,y) = if fst(x,y)==True
            then True
            else if snd(x,y)==True
            then True
            else False

distancia (x,y) (x1,y1)= sqrt(
    ((abs(x-x1))*(abs(x-x1)))
    +
    ((abs(y-y1))*(abs(y-y1)))
    )

fatorial 1 = 1
fatorial x = (fatorial (x-1)) * x

fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-2) + fibo (n-1)

triangular 1 = 1
triangular n = triangular (n-1) + n


potencia2 1 = 2
potencia2 n = potencia2 (n-1) * 2

produtointervalo::Int -> Int -> Int
produtointervalo m n = if (m == n)
    then 1
    else n * produtointervalo m (n-1)

redefinefatorial n = produtointervalo 1 n

restodiv n m = if (n>m)
              then m
              else restodiv n (m-n)

divinteira n m k = if (n>m)
                    then k
                    else divinteira n (m-n) (k+1)

mdc m 0 = m
mdc m n = mdc n (mod m n) 
mdc2 m n
    | n == 0 = m
    | otherwise = mdc2 n (mod m n)

binomial (n,0) = 1
binomial (n,k) = if (n==k)
    then 1
    else binomial (n-1,k) + binomial (n-1,k-1)
binomial2 (n,k)
    | k==0 = 1
    | k==n = 1
    | otherwise = binomial2 (n-1,k) + binomial2 (n-1,k-1)


passo (x,y) = (y,(x+y))
fibo2 0 = 0
fibo2 1 = 1
fibo2 n = snd(passo (fibo2 (n-2), fibo2(n-1)))