
-----------------------------------------------------------------------------------------

-- ASKHSH 1
generating :: (Int->Double)->Int->(Double->Double)
generating f k = \z->(f (k-1) * z^(k-1))+(f k * z^k)
  
  


-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

--First Helping Function-Calculate list elements
calcElem :: [u]->[Int]->(u->Int->v)->[v]
calcElem [] b f = []
calcElem a [] f = []
calcElem a b f = f (head a) (head b) : calcElem (tail a) (tail b) f


--Main Function
mapi :: [u]->(u->Int->v)->[v]
mapi s f = calcElem s [1,2..(length s)] f

