module idk1
import StdEnv

// Recursive way!
//splitList _ [] = []
//splitList listA [b : bs] = [take b listA : splitList (drop b listA) bs]
//Start = splitList [1..9] [3,2,4]

// Higher order fuction and List comprehension way!
f :: [Int] -> [Int]
f [x] = []
f [x,y : xs] = [x + y] ++ f [x + y : xs]

splitList :: [Int] [Int] -> [[Int]]
splitList listA listB = [drop (n + 1) (take b listA) \\ b <- ([hd listB] ++ f listB) & n <- [-1] ++ tl listB]

//Start = splitList [1..9] [3,2,4]
	  
convert :: 	[[Int]] -> [Int]
convert lists = [ sum list / len list \\ list <- lists | isOK list]
where isOK [] = True
	  isOK [x:xs] 
			| x >= 0 = isOK xs
			= False
	  len [] = 1
	  len list = length list
	  
//Start = convert [[-2],[],[]]
//Start = toInt 'a'

traverseMatrix :: [[Int]] -> [(Int,Int,Int)]
traverseMatrix lists = flatten [[(a,i,j) \\ a <- list & j <- [0..] | a rem 2 == 0] \\ list <- lists & i <- [0..]]

Start = traverseMatrix [[1,2,3],[1,2,3],[1,2,3]]