module ex_classes

import StdEnv

/* 1. Given the Vector2 record type implement in a class 
operations with vectors. */

:: Vector2 = { x :: Real, y :: Real}

:: Vector3 = { x3 :: Real, y3 :: Real, z3 :: Real}

class Operations a
where 
	 (*==) :: a a -> Bool
	 (*<) :: a a -> Bool
	 (*>) :: a a -> Bool
     (*<=) :: a a -> Bool
     (*>=) :: a a -> Bool
     (!=) :: a a -> Bool
     
instance Operations Vector2
	where  
		(*==) :: Vector2 Vector2 -> Bool 
		(*==) v1 v2 = vlen v1 ==  vlen v2
		(*<) :: Vector2 Vector2 -> Bool 
		(*<) v1 v2 = vlen v1 < vlen v2
		(*>) :: Vector2 Vector2 -> Bool 
		(*>) v1 v2 = vlen v1 > vlen v2
		(*>=) :: Vector2 Vector2 -> Bool 
		(*>=) v1 v2 = vlen v1 >= vlen v2
	    (*<=) :: Vector2 Vector2 -> Bool 
		(*<=) v1 v2 = vlen v1 <= vlen v2
	    (!=) :: Vector2 Vector2 -> Bool 
		(!=) v1 v2 = vlen v1 <> vlen v2
		
vlen :: Vector2 -> Real 		  
vlen v = sqrt( v.x*v.x + v.y*v.y) 

Start = {x = 1.0, y = 1.0} *== {x = 1.0, y = 1.0} // True
//Start = {x = 3.0, y = 4.0} *== {x = 5.0, y = 0.0} // True
//Start = {x = 1.0, y = 1.0} != {x = 2.0, y = 1.0} // True
//Start = {x = 1.0, y = 1.0} *< {x = 2.0, y = 1.0} // True
//Start = {x = 1.0, y = 1.0} *> {x = 2.0, y = 1.0} // False
//Start = {x = 1.0, y = 1.0} *<= {x = 1.0, y = 1.0} // True
//Start = {x = 1.0, y = 1.0} *>= {x = 1.0, y = 1.0} // True

instance Operations Vector3
	where  
		(*==) :: Vector3 Vector3 -> Bool 
		(*==) v1 v2 = vlen3 v1 ==  vlen3 v2
		(*<) :: Vector3 Vector3 -> Bool 
		(*<) v1 v2 = vlen3 v1 < vlen3 v2
		(*>) :: Vector3 Vector3 -> Bool 
		(*>) v1 v2 = vlen3 v1 > vlen3 v2
		(*>=) :: Vector3 Vector3 -> Bool 
		(*>=) v1 v2 = vlen3 v1 >= vlen3 v2
	    (*<=) :: Vector3 Vector3 -> Bool 
		(*<=) v1 v2 = vlen3 v1 <= vlen3 v2
	    (!=) :: Vector3 Vector3 -> Bool 
		(!=) v1 v2 = vlen3 v1 <> vlen3 v2

vlen3 :: Vector3 -> Real 		  
vlen3 v = sqrt( v.x3*v.x3 + v.y3*v.y3 + v.z3*v.z3) 

//Start = {x3 = 1.0, y3 = 1.0, z3 = 1.0} *== {x3 = 1.0, y3 = 1.0, z3 = 1.0} // True
//Start = {x = 3.0, y = 4.0} *== {x = 5.0, y = 0.0} // True
//Start = {x = 1.0, y = 1.0} != {x = 2.0, y = 1.0} // True
//Start = {x = 1.0, y = 1.0} *< {x = 2.0, y = 1.0} // True
//Start = {x = 1.0, y = 1.0} *> {x = 2.0, y = 1.0} // False
//Start = {x = 1.0, y = 1.0} *<= {x = 1.0, y = 1.0} // True
//Start = {x = 1.0, y = 1.0} *>= {x = 1.0, y = 1.0} // True



/* 2. Implement in a class operations *+, *-, **, *!
and create instances for list of integers and strings. */

class Relations a
where 
	 (*+) :: a a -> a
	 (*-) :: a a -> a
	 (**) :: a a -> a
     (*!) :: a a -> a
   
instance Relations [Int]
	where  
		(*+) :: [Int] [Int] -> [Int] 
		(*+) l1 l2 = l1 ++ l2
		(*-) :: [Int] [Int] -> [Int] 
		(*-) l1 l2 = drop (length l2) l1
		(**) :: [Int] [Int] -> [Int] 
		(**) l1 l2 = [a*b \\ a <-l1 & b<-l2]
		(*!) :: [Int] [Int] -> [Int]
		(*!) l1 l2 = [a-b \\ a <-l1 & b<-l2]
		
//Start = [1..5] *+ [6..10] // [1,2,3,4,5,6,7,8,9,10]
//Start = [1..5] *- [6..8] // [4,5]
//Start = [1..5] ** [1..5] // [1,4,9,16,25]
//Start = [11..15] *! [1..5] // [10,10,10,10,10]

instance Relations String
	where  
		(*+) :: String String -> String
		(*+) s1 s2 = s1 +++ s2
		(*-) :: String String -> String 
		(*-) s1 s2 = {a \\ a<-:s1 & i<-[1..(size s2)]}
		(**) :: String String -> String
		(**) s1 s2 = {toChar(toInt(a)+toInt(b)) \\ a <-:s1 & b<-:s2}
		(*!) :: String String -> String
		(*!) s1 s2 = {toChar(toInt(a)-toInt(b)) \\ a <-:s1 & b<-:s2}

//Start = "hello" *+ "world!" // "helloworld!"
//Start = "funy functions" *- "hye" // "fun"
//Start = "123" ** "222" // "cde"
//Start = "abc" *! "111" // "012"
