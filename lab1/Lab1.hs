{-Ioanna-Maria Avanidi 
  A.M.4977-}

{-ASKHSH 1-}
discount :: Float->Float
discount a = 
  if a>100 then (a-(a/10)) else a 

amount :: Int->Float->Float
amount n p =
  if n<0 || p<0 then 0.0
  else if n<=4 then discount ((fromIntegral n)*p)
  else if (n>=5) && (n<=8) then discount ((fromIntegral n-1)*p)
  else if (n==9) then discount ((fromIntegral n-2)*p)
  else discount ((fromIntegral(n-((div (n-9) 3) +2 )))*p)


{-ASKHSH 2-}
minsToSeconds :: (Int,Int,Int)->Float
minsToSeconds (h1,m1,s1) = fromIntegral (h1*3600+m1*60+s1) 

lessThanZero :: (Int,Int,Int)->(Int,Int,Int)->Float
lessThanZero (h1,m1,s1) (h2,m2,s2) =
  if ((minsToSeconds(23,59,59)-minsToSeconds (h1,m1,s1)) + minsToSeconds (h2,m2,s2)) <= 180 then 0.58
  else (0.58+0.003*(((minsToSeconds(23,59,59)-minsToSeconds (h1,m1,s1)) + minsToSeconds (h2,m2,s2))-180))
 
cost :: (Int,Int,Int)->(Int,Int,Int)->Float
cost (h1,m1,s1) (h2,m2,s2)= 
  if minsToSeconds (h2,m2,s2) - minsToSeconds (h1,m1,s1)==0 then 0.0
  else if (minsToSeconds (h2,m2,s2) - minsToSeconds (h1,m1,s1))<0 then lessThanZero (h1,m1,s1) (h2,m2,s2)
  else if (minsToSeconds (h2,m2,s2) - minsToSeconds (h1,m1,s1))<=180 then 0.58
  else (0.58+(0.003*((minsToSeconds (h2,m2,s2)-minsToSeconds (h1,m1,s1))-180)))