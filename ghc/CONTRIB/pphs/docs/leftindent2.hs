eval :: GmState -> [GmState]
eval state = state: restStates
             where
             restStates | gmFinal state = []
                        | otherwise = eval nextState
             nextState  = doAdmin (step state)
