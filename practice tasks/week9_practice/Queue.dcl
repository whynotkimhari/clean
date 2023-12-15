definition module Queue

:: Queue a
// a queue is a FIFO data structure - first element in - first element out

newQueue ::     (Queue a)                    // empty queue
empty    ::     (Queue a) -> Bool
push     ::  a  (Queue a) -> Queue a         // push new element at the end of the queue
pushes   :: [a] (Queue a) -> Queue a       // Consecutively push new elements to queue
pop      ::     (Queue a) -> Queue a         // Remove the top element from the queue
popn     :: Int (Queue a) -> Queue a       // Remove the top n elements from the queue
top      ::     (Queue a) -> a               // Return the top element from the queue
topn     :: Int (Queue a) -> [a]           // Return the top n elements from the queue
elements ::     (Queue a) -> [a]           // return all ements from the queue
count    ::     (Queue a) -> Int             // count the number of elements on the queue
