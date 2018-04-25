%               Filename:  Queue.lhs
%               Version :  1.3
%               Date    :  3/4/92

\section{A Queue Abstract data type}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
module Queue(
		Queue, createQueue, addFront, addBack,
		addAllFront, addAllBack, inquireFront,
		inquireBack, removeFront, removeBack, 
		emptyQueue
) where
\end{code}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
type Queue a = [a]
\end{code}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
createQueue::Queue a
createQueue = []

addFront::a -> Queue a -> Queue a
addFront x q = x:q

addBack::a -> Queue a -> Queue a
addBack x q = q ++ [x]

addAllFront::[a] -> Queue a -> Queue a
addAllFront list q = list ++ q

addAllBack::[a] -> Queue a -> Queue a
addAllBack list q = q ++ list

inquireFront::Queue a -> a
inquireFront []    = error "Cannot inquire on empty Queue"
inquireFront (h:t) = h

inquireBack::Queue a -> a
inquireBack []     = error "Cannot inquire on empty Queue"
inquireBack [x]    = x                 
inquireBack (x:xs) = inquireBack xs

removeFront::Queue a -> Queue a
removeFront []    = error "Cannot remove from an empty Queue"
removeFront (h:t) = t

removeBack::Queue a -> Queue a
removeBack []    = error "Cannot inquire on empty Queue"
removeBack [x]   =  []                
removeBack (x:xs) = x:(removeBack xs)

emptyQueue::(Eq a) => Queue a -> Bool
emptyQueue x = x==[]

sizeQueue::Queue b -> Int
sizeQueue xs =  length xs
\end{code}

