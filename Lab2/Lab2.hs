--Ioanna Avanidi 4977
-----------------------------------------------------------------------------------------

-- ASKHSH 1
--First Helping Function--Number to List
intToList :: Integer->[Integer]
intToList n = 
    if n<10 then [n]
    else intToList(div n 10)++[mod n 10]

--Second Helping Function--List to Number
listToInt :: [Integer]->Integer
listToInt [] = 0
listToInt (h:t) = h*(10^(length t)) + listToInt t

--Third Helping Function--Zero Complement
zeroComp :: [Integer]->[Integer]->[Integer]
zeroComp (h1:t1) (h2:t2) =
    if length((h1:t1))>length((h2:t2)) then zeroComp (h1:t1) (0:h2:t2)
    else if length((h1:t1))<length((h2:t2)) then zeroComp (0:h1:t1) (h2:t2)
    else (h1:t1)

--Fourth helping function--Calculate of ci 
calcCi :: [Integer]->[Integer]->[Integer]
calcCi [][]=[]
calcCi (h1:t1) (h2:t2) =
    if length(h1:t1)/=length(h2:t2) then calcCi (zeroComp (h1:t1) (h2:t2)) (zeroComp (h2:t2) (h1:t1))  
    else mod (13*(ai+5)+19*(bi+3)) 10 : calcCi (init(h1:t1)) (init(h2:t2))
    where ai = last(h1:t1) 
          bi = last(h2:t2)

--Main Function
join :: Integer->Integer->Integer
join a b = 
    if (a<0 || b<0) then join (-a) (-b)
    else listToInt(reverse(calcCi (intToList(a)) (intToList(b))))

-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

--First Helping Function--Divisor of a number
numDiv :: Int->[Int]
numDiv n = divs 2 n
    where
        divs k n |n==1 =[1]
                 |k*k>n = [1, n]
                 |k*k == n =[1, k, n]
                 |mod n k == 0 = (k: (div n k): divs(k+1) n)
                 |otherwise = divs (k+1) n                                                                                                                                                  

--Second Helping Function--List Sorting
insertInt :: Int->[Int]->[Int]
insertInt n (h:t) |n<=h = n:h:t
                  |otherwise = h:insertInt n t
insertInt n [] = [n]

insSortInt :: [Int]->[Int]
insSortInt [] = []
insSortInt(f:r)= (insertInt f (insSortInt r))

--Third Helping Function--Find Common Elements between 2 lists
memberInt :: Int->[Int]->Bool
memberInt n (h:t) = n==h || memberInt n t
memberInt n [] = False

comElems :: [Int]->[Int]->[Int]
comElems [][] = []
comElems (h1:t1)[] = []
comElems [](h2:t2) = []
comElems (h1:t1) (h2:t2) =  
    if (memberInt h1 (h2:t2)) then h1:comElems t1 (h2:2)
    else if (memberInt h2 (h1:t1)) then h2:comElems t2 (h1:t1)
    else comElems t1 t2

--Fourth Helping Function--Find the k-th element in a list of ints
elemIntList :: Int->[Int]->Int
elemIntList 1 (h:t) = h
elemIntList n (h:t) = elemIntList(n-1) t
elemIntList n [] = error "Index out of range"

--Main Function
kgcd :: Int->Int->Int->Int
kgcd m n k =
    if length(comElems (numDiv m) (numDiv n))<k then 0
    else elemIntList k (reverse(insSortInt (comElems (numDiv m) (numDiv n))))


    




