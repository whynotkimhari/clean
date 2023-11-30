module PT8gr5
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf
         
/*  WRITE NAME AND NEPTUN HERE !!! :
	Given a tree with positive values, check if each node is even. 
	If it is, collect the node and direct children's values 
	in triple tuple in post order way. 
	Leaf counts as 1.
					
			  
*/



treeOne = Node 10 (Node 8 (Node 4 (Node 30 Leaf Leaf) Leaf) (Node 4 Leaf Leaf)) (Node 70 (Node 6 Leaf Leaf) (Node 5 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)))
treeTwo = Node 10 (Node 2 (Node 40 (Node 9 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 3 (Node 1 Leaf Leaf) (Node 2 Leaf (Node 1 Leaf Leaf))) 
treeThree = Node 10 (Node 7 (Node 4 (Node 80 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 30 (Node 6 Leaf Leaf) (Node 5 Leaf (Node 2 Leaf Leaf))) 

extract:: (Tree Int) -> Int
extract Leaf = 1
extract (Node x l r) = x

sumn :: (Tree Int) -> [(Int, Int, Int)]
sumn Leaf = []
sumn (Node x le ri)
| isEven x = sumn le ++ sumn ri ++ [(x,extract le, extract ri)]
= sumn le ++ sumn ri

//Start = sumn treeOne		// [(30,1,1),(4,30,1),(4,1,1),(8,4,4),(6,1,1),(4,1,1),(70,6,5),(10,8,70)]

//Start = sumn treeTwo		// [(40,9,1),(2,40,7),(2,1,1),(10,2,3)]

//Start = sumn (Node 10 Leaf Leaf)  // ([(10,1,1)]

//Start = sumn treeThree	// [(80,1,1),(4,80,1),(6,1,1),(2,1,1),(30,6,5),(10,7,30)]

//Start = sumn Leaf // []
