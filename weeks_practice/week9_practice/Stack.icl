implementation module Stack

import StdEnv

:: Stack a :== [a]

newStack :: Stack a
newStack = []

empty  :: (Stack a) -> Bool
empty stack = count stack == 0

push :: a (Stack a) -> Stack a
push val stack = [val] ++ stack

pushes :: [a] (Stack a) -> Stack a 
pushes vals stack = (reverse vals) ++ stack

pop	:: (Stack a) -> Stack a
pop stack = init stack

popn :: Int (Stack a) -> Stack a 
popn n stack = reverse (drop n (reverse stackd))

top	:: (Stack a) -> a
top stack = last stack

topn     :: Int (Stack a) -> [a]
topn n stack = reverse (take n (reverse stackd))

elements :: (Stack a) -> [a]
elements stack = stack

count :: (Stack a) -> Int
count stack = length stack

//	You can use this Start-function to test your implementations:
Start				= ( "s0 = newStack = ",        s0,'\n'
					  , "s1 = push 1 s0 = ",       s1,'\n'
					  , "s2 = push 2 s1 = ",       s2,'\n'
					  , "s3 = pop s2 = ",          s3,'\n'
					  , "s5 = top s3 = ",          s5,'\n'
					  , "test = empty s1 = ",     test,'\n'
					  , "count s1 = ",     n,'\n'
					  , "pushes [1,2] s1 = ",     s6,'\n'
					  , "popn 2 s6 = ",           s7,'\n'
					  , "count s7 = ",            k,'\n')
where
	s0				= newStack
	s1				= push   1      s0
	s2				= push   2      s1
	s3				= pop           s2
	s5				= top           s3
	test            = empty         s1
	n               = count s1
	s6 				= pushes [1,2] s1
	s7 				= popn 2 s6
	k               = count s7