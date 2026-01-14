--Ioanna Avanidi 4977

-----------------------------------------------------------------------------------------

-- ASKHSH 1

-- First Helping Function-For the length of the sides(variables a, b, c)
sLength :: (Double, Double)->(Double, Double)->Double
sLength (a1, b1) (a2, b2)= sqrt(((a1-a2)^2)+((b1-b2)^2))                                        

-- Second Helping Function-For semiperimeter(variable t)
t :: (Double,Double)->(Double,Double)->(Double,Double)->Double
t (z1,w1) (z2,w2) (z3,w3) = ((sLength(z1, w1) (z2, w2))+(sLength(z1, w1) (z3, w3))+(sLength(z2, w2) (z3, w3)))/2 

-- Main Function
area :: (Double,Double)->(Double,Double)->(Double,Double)->Double
area (x1,y1) (x2,y2) (x3,y3) = 
    if (((y2-y1)/(x2-x1))==((y3-y2)/(x3-x2))) then 0.0
    else sqrt((t (x1, y1) (x2, y2) (x3, y3))*((t(x1, y1) (x2, y2) (x3, y3))-sLength(x1,y1) (x2,y2))*((t(x1, y1) (x2, y2) (x3, y3))-sLength(x1,y1) (x3,y3))*((t(x1, y1) (x2, y2) (x3, y3))-sLength(x2,y2) (x3,y3)))

-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

--First Helping Function-Convert hours to minutes 
hoursToMinutes :: (Int, Int)->Int
hoursToMinutes (h, m) = (h*60+m)

--Second Helping Function-Calculate hours
calcHours :: (Int, Int)->(Int, Int)->Int
calcHours (h1, m1) (h2, m2) = 
    if m2<=m1 then h2-h1
    else h2-h1+1

--Main Function
parking :: (Int,Int)->(Int,Int)->Int            
parking (h1,m1) (h2,m2) = 
    if hoursToMinutes(h2, m2) - hoursToMinutes(h1, m1)==0 then 0
    else if(hoursToMinutes(h2, m2)-hoursToMinutes(h1, m1))<=180 then 8
    else if(hoursToMinutes(h2, m2)-hoursToMinutes(h1, m1))>=180 && (hoursToMinutes(h2, m2)-hoursToMinutes(h1, m1))<=360 then (calcHours(h1, m1)(h2, m2)-3)*2 + 8
    else  (calcHours(h1, m1)(h2, m2)-6)+14                                        





