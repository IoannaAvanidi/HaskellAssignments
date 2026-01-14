-- ASKHSH 1
aBChar=['A','a','B','b','C','c','D','d','E','e','F','f','G','g','H','h','I','i','J','j','K','k','L','l','M','m','N','n','O','o','P','p','Q','q','R','r','S','s','T','t','U','u','V','v','W','w','X','x','Y','y','Z','z']



memberChar :: Char -> [Char] -> Bool
memberChar n (h:t) = n == h || memberChar n t
memberChar n [] = False


inABC :: String->Int->[[Char]]
inABC n p =
    if (p<(length n)) && (memberChar (n!!p) aBChar) then [[n!!p]] ++ inABC n (p+1)
    else if (p<(length n)) && (not (memberChar (n!!p) aBChar)) then [['<']] ++ inABC n (p+1)
    else []

inABCtoString :: String->Int->String
inABCtoString n p =
    if (p<(length n)) && (memberChar (n!!p) aBChar) then [n!!p] ++ inABCtoString n (p+1)
    else if (p<(length n)) && (not (memberChar (n!!p) aBChar)) then "<" ++ inABCtoString n (p+1)
    else ""
    
connectStrings :: [String]->Int-> String
connectStrings n p = 
    if (p<(length n)) && (memberChar ((n!!p)!!0) aBChar) then [((n!!p)!!0)] ++ connectStrings n (p+1)
    else ""

findPosition :: [String]->Int->Int
findPosition n p =
    if (p==(length n)) || (p>(length n)) then -1
    else if (p<(length n)) && (memberChar ((n!!p)!!0) aBChar) then findPosition n (p+1)
    else p

wordList :: String->[String]
wordList "" = []
wordList s =
    if ((findPosition (inABC s 0) 0) == -1)  && (memberChar (head s) aBChar) then [s]
    else if (length s == 1) && (memberChar (head s) aBChar) then []
    else if (length s == 1) && not(memberChar (head s) aBChar) then []
    else if not(memberChar (head s) aBChar) && (findPosition (inABC s 0) 0)<(length s) then wordList (inABCtoString s 1) 
    else if (findPosition (inABC s 0) 0) == -1  then []
    else if (findPosition (inABC s 0) 0)<(length s) then [connectStrings (inABC s 0) 0] ++ wordList (inABCtoString s ((findPosition (inABC s 0) 0)+1) ++ " ")
    else []

-- ASKHSH 2

trace :: [(Int,Int)]->[(Int,Int)]

trace s = []                                        


