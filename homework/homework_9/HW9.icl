module HW9
import StdEnv

//Name: BUI NGUYEN KIM HAI NeptunCode: QMIBHU

:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 = Node 7 
						( Node 2 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf)) 
						( Node 20 (Node 12 Leaf Leaf) (Node 4 Leaf Leaf))
						
						
tree2 = Node 5 
						( Node 3 (Node 13 Leaf Leaf) (Node 11 Leaf Leaf)) 
						( Node 1 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf))


/*  1)
    write a function that takes a tree and a list of tuples of the form (a,b), 
    you need to find the node with value a and change its value to b times
    its level in the tree.

    eg:
    
    input: 
             7                    1st level
           /   \                
          2     20               2nd level
         / \    / \ 
       10  30  12  4        3rd level

        [(10,2),(30,3),(4,6),(20,5)]

       output 
                  7
                /   \
               2     10
              / \    / \
            6   9  12   18
    (10,2) => 10 is at level 3, so its value is changed to 2*3 = 6
    (30,3) => 30 is at level 3, so its value is changed to 3*3 = 9
    (4,6) => 4 is at level 3, so its value is changed to 6*3 = 18
    (20,5) => 20 is at level 2, so its value is changed to 5*2 = 10

*/

getLevel :: Int (Tree Int) -> Int
getLevel x Leaf = 0
getLevel x (Node a le ri)
| x == a = 1
| L == 0 && R == 0 = 0
| L <> 0 && R == 0 = L + 1
| L == 0 && R <> 0 = R + 1			
= 0 								// This case means "exist duplicate value"
where
	L = getLevel x le
	R = getLevel x ri


swap :: Int (Tree Int) [(Int,Int)] -> Int
swap x root list
| z == [] = x						// If it is not the value needed changing, do nothing
= (getLevel x root) * (hd z)
where 
	z = [b \\(a,b) <- list | a == x]


mySwap :: (Tree Int) [(Int,Int)] (Tree Int) -> (Tree Int)
mySwap Leaf _ _ = Leaf
mySwap (Node x le ri) list root = (Node (swap x root list) (mySwap le list root) (mySwap ri list root))


SwapLevel :: (Tree Int) [(Int,Int)] -> (Tree Int)
SwapLevel tree list = mySwap tree list root 		// I need one more information 'bout the root
where root = tree

//Start = SwapLevel tree1 [(10,2),(30,3),(4,6),(20,5)]
//(Node 7 
//          (Node 2 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf)) 
//          (Node 10 (Node 12 Leaf Leaf) (Node 18 Leaf Leaf)))


//Start = SwapLevel tree2 [(13,7),(11,1),(1,5)] 
//(Node 5
//          (Node 3 (Node 21 Leaf Leaf) (Node 3 Leaf Leaf))
//          (Node 10 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf)))

//Do not modify!

:: Category = Baby | Infant | Toddler
:: Child = { name::String, category :: Category, favToys::{String} }

c1 :: Child
c1 = { name = "Johnny", category = Baby, favToys = {"toy1","toy2"}}

c2 :: Child
c2 = { name = "Emily", category = Infant, favToys = {"toy3", "toy4"} }

c3 :: Child
c3 = { name = "Oliver", category = Toddler, favToys = {"toy3", "toy4"} }

c4 :: Child
c4 = { name = "Sophia", category = Baby, favToys = {"toy2", "toy1"} }

c5 :: Child
c5 = { name = "Liam", category = Toddler, favToys = {"toy1", "toy5"} }

c6 :: Child
c6 = { name = "Ronny", category = Infant, favToys = {"toy3", "toy4"} }

c7 :: Child
c7 = { name = "Johnny", category = Baby, favToys = {"toy1","toy3"}}

/* 2) Instances
Define `==` operator on the child Data type. 
Two children are equal, if they fall onto the same category and they have the 
same favorite toys.
To check if they fall onto the same category you have to define the `==` for the Category.
To check for the same favorite toys, you have to define `==` for arrays of strings as well. 
Note that order does not matter, {"toy1","toy4"} is equal to {"toy4","toy1"}
*/

instance == Category
where
	(==) Baby Baby = True
	(==) Infant Infant = True
	(==) Toddler Toddler = True
	(==) _ _ = False
	
instance == {String}
where
	(==) arrA arrB = sort [x\\x<-:arrA] == sort [x\\x<-:arrB]

instance == Child
where
	(==) childA childB = childA.category == childB.category && childA.favToys == childB.favToys


	
//Start = c5 == c5 //True
//Start = c1 == c2 // False, because they have different categories
//Start = c1 == c4 && c4 == c1 // True, because they have same category and same favorite toys
//Start = c1 == c7 // False, because they have different favorite toys.
//Start = (c2 == c6) && c2 <> c7 && c2 <> c3// True 

