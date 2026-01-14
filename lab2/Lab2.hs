{-Ioanna-Maria Avanidi 
  A.M.4977-}


-----------------------------------------------------------------------------------------

-- ASKHSH 1



-- search2 :: Integer->Integer->Integer->Integer->Integer
-- search2 a k m s
--   | (s^k) < (m^(s-a)) = s-a
--   | otherwise = search2 a m k (a+1)


s = 0
addN :: Integer -> Integer

addN n = n + 1

search :: Integer->Integer->Integer->Integer
search a k m
  | (((addN n)+a)^k) < (m^(addN n)) = addN n 
  | otherwise = search a k m 
  where n = s
                                     




{-ASKHSH 2-}
-- digits :: int->int
-- digits m =
--   if ((n>0) && (n<=9)) then 1
--   else  


lastDigitsMul :: Int -> Int

lastDigitsMul finalResult = (div finalResult (10 ^ ((length (show finalResult))-1))) * (mod finalResult (10 ^ ((length (show finalResult))-1)))

sumOfDigits :: Int -> Int -- Maybe change here
sumOfDigits 0 = 0
sumOfDigits n
  | (((mod n (10 ^ ((length (show n))-1))) /= 0) && (n>9))= (div n (10 ^ ((length (show n))-1))) * sumOfDigits((mod n (10 ^ ((length (show n))-1))))
  | ((mod n (10 ^ ((length (show n))-1))) == 0) = n
  | otherwise = (div n (10 ^ ((length (show n))-1)))




compress :: Integer->Integer

compress n = -2022                                        

  