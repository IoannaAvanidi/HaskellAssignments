--Ioanna Avanidi 4977
-----------------------------------------------------------------------------------------

-- ASKHSH 1
  
--First Helping Function--Sub each number of a list with a number and put the result in a list
subResult :: [Int]->Int->[Int]
subResult [] n = []
subResult (h:t) n = abs(h-n) : subResult t n

--Second Helping Function--Return smallest element
smallElem :: [Int]->Int
smallElem (h:[]) = h
smallElem (h:t) = min h (smallElem t)

--Third Helping Function--Find Element's position
elemPos :: [Int]->Int->Int
elemPos (h:t) n =  
    if h==n then 1
    else 1+(elemPos t n)

--main Function
nearest :: [Int]->Int->Int
nearest s n = elemPos (subResult s n) (smallElem (subResult s n)) 


-----------------------------------------------------------------------------------------
     
-- ASKHSH 2 

--First Helping Function--String to List
strToList :: String->[String]
strToList s = 
    if length s == 1 then [s]
    else [head s] : strToList (tail s)

--Second Helping Function--List to String
listToStr :: [String]->String
listToStr [] = ""
listToStr (h:t) = h ++ (listToStr t)

--Third Helping Function--Find and replace single element
fRepSingElem :: [String]->String->String->[String]
fRepSingElem [] a b = []
fRepSingElem (h:t) a b =
    if h == a  then b : fRepSingElem t a b  
    else h:fRepSingElem t a b 

--Main Function 
replace :: String->String->String->String
replace a b w = 
    if (length a == length b) && (length a == 1) && (length w == 1) then b
    else if (length a == length b) && (length a == 1) then listToStr(fRepSingElem (strToList w) a b)
    else if length w == 0 then ""
    else "2023"
