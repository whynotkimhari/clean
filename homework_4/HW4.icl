module HW4
import StdEnv


//Name:    BUI NGUYEN KIM HAI            Neptun Code: QMIBHU

/*
Let's organize data. Given a list of numbers, let's create a list of tuples
where the first element of the tuple is the number and the second is its 
occurrence. The list of tuples should be sorted. 
You must use at least two higher order functions.

Tip: to remove duplicates you can use the isMember function, and recurse through the list 
Tip2: to sort a list of tuples, you can simply use the in-built sort operator
*/
	  
f :: [Int] -> [(Int,Int)]
f list = sort (toSet new_list [])

where 
	  // Var
	  new_list = map (\num = (num,length(filter ((==)num) list))) list
	  
	  // Func
	  toSet [] set = set
	  toSet [x:xs] set
	  | isMember x set = toSet xs set
	  | otherwise = toSet xs (set ++ [x])

//Start = f [] // []
//Start = f [1] // [(1,1)]
//Start = f [1,2,2,3,3,3,4,4,4,4] // [(1,1),(2,2),(3,3),(4,4)]
//Start = f [-1,2,3,-1,2,3,3,3,2] // [(-1,2),(2,3),(3,4)]


/*
We are given a list of tuples, in which the first element is the value of a character, and the second element
is the character itself. Filter out all those tuples which have the highest value. No duplicates should be present.

Example:
	[(10,'c'),(10,'b'),(0,'d'),(10,'a'),(10,'c'),(9,'a')] = [(10,'c'),(10,'b'),(10,'a')]

Tip: you can use the in built sort function to obtain a sorted list of tuples based on the first element.
Tip2: isMember operator works in tuple as well, so you can use it to remove duplicates
*/

keepTuples :: [(Int,Char)] -> [(Int,Char)]
keepTuples list = toSet (filter (\(first,second) = first == maxiVal) list) []

where 
	  // Var
	  sortedList = sort list
	  lenList = length list
	  maxiVal = fst (sortedList !! (lenList - 1))
	  
	  // Func
	  toSet [] set = set
	  toSet [x:xs] set
	  | isMember x set = toSet xs set
	  | otherwise = toSet xs (set ++ [x])
	  
//Start = keepTuples [] // []
//Start = keepTuples [(10,'c'),(10,'b'),(0,'d'),(10,'a'),(10,'c'),(9,'a')] // [(10,'b'),(10,'a'),(10,'c')]
//Start =  keepTuples [(-2,'c'),(0,'b'),(0,'d'),(1,'a'),(1,'c'),(1,'a')] // [(1,'c'),(1,'a')]
//Start = keepTuples [(999,'x'),(-1,'x'),(999,'x'),(999,'x'),(0,'k'),(0,'a')] // [(999,'x')]
