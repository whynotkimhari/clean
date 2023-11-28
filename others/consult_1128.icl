module consult_1128
import StdEnv

:: Library = {lib_name :: String , books :: {Book}}
:: Book= {title::String,author::String, pyear :: Int,num_of_pages::Int, can_be_borrowed::Bool}

b1::Book
b1 = {title = "C Programming Language", author = "Abel" , pyear =2022 , num_of_pages = 1501 , can_be_borrowed = False }
b2::Book
b2 = {title = "Functional Programming", author = "Andrey" , pyear =1999 , num_of_pages = 1250 , can_be_borrowed = True }
b3::Book
b3 = {title = "Java Programming Language", author = "John" , pyear =1508 , num_of_pages = 2980 , can_be_borrowed =True}
b4::Book
b4 = {title = "OOP Programming", author = "Peter" , pyear =2020 , num_of_pages = 280 , can_be_borrowed = False }
b5::Book
b5 = {title = "Programming", author = "James" , pyear =2000 , num_of_pages =1645 , can_be_borrowed =True}

lib1::Library
lib1 = {lib_name = "lib1" , books ={b1,b2}}
lib2::Library
lib2 = {lib_name = "lib2" , books ={b1,b2,b3}}
lib3::Library
lib3 = {lib_name = "lib3" , books ={b1,b2,b3,b4}}
lib4::Library
lib4 = {lib_name = "lib4" , books ={b1,b4,b5}}
lib5::Library
lib5 = {lib_name = "lib5" , books ={b1,b2,b3}}
lib6::Library
lib6 = {lib_name = "lib6" , books ={b4,b4,b2,b1}}

//----------------------------

/*3. Instances - Records. (20 points) Note this task has 4 parts each of 5 points!
*
* 3.1 Create an instance of '==' for the type Book. Two books are equal
* if they have the same title, author, publishing year and pages.
* (Whether the books can be borrowed does not matter when comparing them.)
*/

//==

//Start = b1 == b1 // True
//Start = b1 == b2 // False

//----------------------------

/* 3.2 Create an instance of '<' for the type Book. A book is smaller then
* another if the publishing year is smaller then second book's publishing year.
*/

//<

//Start = b1 < b2 // False
//Start = b3 < b2 // True

//----------------------------

/* 3.3 Write an instance of '+' for libraries, which unifies libraries' books,
* eliminate redundancies and arrange them according to the publication year.
* The name of new library is the concatenation of the 2 libraries' name.
*/

//+

//Start = lib1 + lib1
//(Library "lib1lib1" {(Book "Functional Programming" "Andrey" 1999 1250 True),(Book "C Programming Language" "Abel" 2022 1501 False)})
//Start = lib1 + lib2
//(Library "lib1lib2" {(Book "Java Programming Language" "John" 1508 2980 True),(Book "Functional Programming" "Andrey" 1999 1250 True),(Book "C Programming Language" "Abel" 2022 1501 False)})

//----------------------------

/* 3.4 Write '==' operator for 'Library' data type.
* Two libraries are equal if they have 'exactly' the same books.
*/

//==

//Start = lib1 == lib1 // True
//Start = lib3 == lib6 // False
//Start = lib1 == lib5 // False
//Start = and [li == li \\ li <- [lib1,lib2,lib3,lib4,lib5]] // True
//Start = lib3 == lib5 // False
//Start = lib2 == lib5 // True

/** Given a tree and an integer n, find the nodes equal to n and replace “its parent” by ‘-1’ (the value of leaf default to be 0) */

//f3 :: Int (Tree Int) -> (Tree Int)


//Start = f3 3 atree //(Node 4 (Node -1 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node -1 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)))
//Start = f3 2 ctree //(Node -1 (Node 2 (Node 8 Leaf Leaf) (Node 9 (Node 4 (Node 16 Leaf Leaf) Leaf) Leaf)) (Node -1 (Node 3 Leaf Leaf) (Node 2 Leaf Leaf)))


/* Firsts and Middles. (10 points)
*
* Given a tree of type (TypeName String), return all the
* Node values of type FirstName or MiddleName in a list.
*/


:: TypeName a = FirstName a | MiddleName a | LastName a

:: Tree a = Node a (Tree a) (Tree a) | Leaf


treeBig :: (Tree (TypeName String))
treeBig = Node (FirstName "Tariq") (Node (LastName "Forza") Leaf Leaf) ( Node (MiddleName "Beka") (Node (LastName "Arm") Leaf Leaf) (Node (MiddleName "Mohido") Leaf Leaf ))

treeRight :: (Tree (TypeName String))
treeRight = Node (FirstName "A") Leaf (Node (LastName "B") Leaf ( Node (MiddleName "C") Leaf (Node (MiddleName "D") Leaf (Node (LastName "E") Leaf Leaf))))

treeNone :: (Tree (TypeName String))
treeNone = Leaf


//firstAndMiddle :: (Tree (TypeName String)) -> [String]

//Start = firstAndMiddle treeBig // ["Tariq","Beka","Mohido"]
//Start = firstAndMiddle treeRight // ["A","C","D"]
//Start = firstAndMiddle treeNone // []

/*
	9. Write a filter function for colored rose tree.
	Colored rose Tree is a tree where each node has 
	some value, color and children nodes stored in list.
	Your filter function should take tree, color and a 
	condition function as an argument. Return a list of
	values stored in nodes which have given color and
	satisfy given condition (Condition function returns
	true for node's value)
*/

::NodeColor = Red | Green | Blue
::ColoredRoseTree a = Node a NodeColor [ColoredRoseTree a] | Leaf
tree1 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Leaf])]
tree2 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Node 7 Red [Node 9 Red [], Node 10 Red []]])]


//filterColoredTree :: (ColoredRoseTree a) NodeColor (a -> Bool) -> [a]

//Start = filterColoredTree tree1 Blue isEven // [2,4]
//Start = filterColoredTree tree1 Blue isOdd // [3]
//Start = filterColoredTree tree2 Red (\x = True) // [1.7,9,10]
//Start::[Int] // Uncomment this line too, to run next test
//Start = filterColoredTree Leaf Green isOdd // []


