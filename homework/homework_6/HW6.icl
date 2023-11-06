module HW6
import StdEnv


// Name:      BUI NGUYEN KIM HAI          Neptun Code: QMIBHU
// Plagiarism of any kind will not be accepted !

/*
List comprehension - Workers
Your boss has assigned you a very important duty. Given some data about workers
you are to group these workers based on their salaries. 
A worker is represented by a tuple, where the first element is his/her name and the
second element is his/her wage. 
You have to create a list of lists, where the first list contains all the names of 
those workers who earn less than 10K, the second list should contain all those workers 
who earn between 10-20K, and the last list should contain all those workers who earn more 
than 20K.
*/

workers = [("Alice", 8000), ("Bob", 15000), ("Charlie", 25000), ("David", 9000), ("Eve", 18000)]
workers1 = [("Frank", 7500), ("Grace", 11000), ("Helen", 21000), ("Ivy", 9800), ("Jack", 24000)]
workers2 = [("Kate", 8500), ("Liam", 13000), ("Mia", 17000), ("Noah", 8000), ("Olivia", 22000)]

f :: [(String, Int)] -> [[String]]
f list = [group1,group2,group3]
where 
	  group1 = [name \\ (name,salary) <- list | salary < 10000]
	  group2 = [name \\ (name,salary) <- list | salary >= 10000 && salary <= 20000]
	  group3 = [name \\ (name,salary) <- list | salary > 20000]


//Start = f workers // [["Alice","David"],["Bob","Eve"],["Charlie"]]
//Start = f workers1 // [["Frank","Ivy"],["Grace"],["Helen","Jack"]]
//Start = f workers2 // [["Kate","Noah"],["Liam","Mia"],["Olivia"]]]


/*
List Comprehension and Indexing
Given a list of lists and an integer, keep all those lists for which the element at 
the given integer index is the max element of that list.
Tip: Check maxList and its functionality
*/

data = [[10, 20, 30], [15, 45, 35], [5, 10, 15], [12, 28, 25]]
data1 = [[8, 12, 5], [15, 20, 10], [9, 18, 27], [7, 7, 7]] 

maxIndex :: [[Int]] Int -> [[Int]]
maxIndex lists index = [list \\ list <- lists | maxList list == (list !! index)]
//Start = maxIndex data 1// [[15,45,35],[12,28,25]]
//Start = maxIndex data 0 // []
//Start = maxIndex data1 1 // [[8,12,5],[15,20,10],[7,7,7]]
//Start = maxIndex data1 2 // [[9,18,27],[7,7,7]]
