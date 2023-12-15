implementation module Queue

import StdEnv

:: Queue a :== [a]

newQueue :: Queue a
newQueue = []

empty :: (Queue a)
empty q = length q == 0

push :: a (Queue a) -> Queue a
push x q = q ++ [x]

pushes :: [a] (Queue a) -> Queue a
pushes xs q = q ++ xs

pop :: (Queue a) -> Queue a
pop q = tl q

popn :: Int (Queue a) -> Queue a
popn n q = drop n q

top :: (Queue a) -> a
top q = hd q

topn :: Int (Queue a) -> [a]
topn n q = take n q

elements :: (Queue a) -> [a]
elements q = q

count :: (Queue a) -> Int
count q = length q