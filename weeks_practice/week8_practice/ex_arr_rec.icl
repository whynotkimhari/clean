module ex_arr_rec

import StdEnv


// 1. Define algebraic type : Day (Mon,Tue,Wed,Thu,Fri,Sat,Sun).
// And define function IsWeekend :: Day -> Bool to check if it is Sat or Sun.
// if it is weekend, then output "Happy day!", otherwise, "Oh noo".

:: Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

happy :: Day -> String
happy Sat = "Happy day!"
happy Sun = "Happy day!"
happy _ = "Oh noo"

//Start = happy Sun  // "Happy day!"
//Start = happy Tue  // "Oh noo"



// 2. Given a predefined Shape type, argument of the Circle constructor 
// is the radius, side length for Square, and equilateral Triangle, 
// width and height for Rectangle, write a function that calculates 
// the circumference area and circumference of each shape in the array, 
// store the results of each shape as a tuple like (area,circumference) 
// in an array.
//    			Circumference		Area
//    Circle			2*r*pi			r^2*pi		p=3.14
//    Square			4*a				a^2
//    Tiangle			3*a				sqrt(3)*a^2/4
//    Rectangle		2*a+2*b			a*b

:: Shape = Circle Real
        | Square Real
        | Triangle Real
        | Rectangle Real Real

cir :: Shape -> (Real, Real)
cir (Circle r) = (2.0*r*3.14,(r*r)*3.14)
cir (Square a) = (4.0*a,a*a)
cir (Triangle a) = (3.0*a,sqrt(3.0)*a^2.0/4.0)
cir (Rectangle a b) = (2.0*a+2.0*b,a*b)

calc :: {Shape} -> {(Real,Real)}
calc ar = {cir t \\ t<-: ar}

//Start = calc {(Circle 3.0), (Square 2.5)} 
// {(18.84,28.26),(10,6.25)}
//Start = calc {(Triangle 4.3), (Rectangle 5.4 7.2), (Circle 2.45)} 
// {(12.9,8.00640485798713),(25.2,38.88),(15.386,18.84785)}
//Start = calc {(Triangle 7.6), (Circle 1.75), (Square 0.95)} 
// {(22.8,25.0108136612946),(10.99,9.61625),(3.8,0.9025)}




// 3. Given two Strings as parameters, remove all characters 
// of first string from the second one. Exampe: "z" "Pizza" -> "Pia"

remove_from_first_string :: String String -> String
remove_from_first_string sta stb = toString ls
where
	ls = [t \\ t<-: stb | not (isMember t la)]
	where la = [u\\ u<-: sta]
	
//Start = remove_from_first_string "z" "Zozo" // "Zoo"
//Start = remove_from_first_string "Xbc" "XccEcxacXmXs aXcrccXe hXaXccXbrXd"// "Exams are hard"
//Start = remove_from_first_string " " "Clean is the best"// "Cleanisthebest"
//Start = remove_from_first_string "" "It's a nice weather outside"// "It's a nice weather outside"
//Start = remove_from_first_string "" ""// ""



:: Point = {  x       ::  Real
            , y       ::  Real
            , visible ::  Bool
            }

Origo :: Point
Origo = { x = 0.0
        , y = 0.0
        , visible = True
        }

// 4. Test about 3 points if they can form a right-angled triangle.

IsTriangle :: Point Point Point -> Bool
IsTriangle p1 p2 p3 = a == (b + c) || b == (a + c) || c == (a + b)
where
  a = (p2.x-p1.x)*(p2.x-p1.x) + (p2.y-p1.y)*(p2.y-p1.y)
  b = (p3.x-p2.x)*(p3.x-p2.x) + (p3.y-p2.y)*(p3.y-p2.y)
  c = (p3.x-p1.x)*(p3.x-p1.x) + (p3.y-p1.y)*(p3.y-p1.y)

//Start = IsTriangle Origo {x = 0.0, y = 3.0, visible = True} {x = 2.0, y = 0.0, visible = True}


