-- !!! Brute force soln to a puzzle. Sent to us by Stephen Eldridge
module Main(main) where

data ItemType = Bono
              | Edge
              | Larry
              | Adam
              deriving (Eq, Ord, Enum)

data BankType = LeftBank
              | RightBank
              deriving Eq

data StateType = State {bonoPos :: BankType,
                        edgePos :: BankType,
                        larryPos :: BankType,
                        adamPos :: BankType}
                 deriving Eq

type History = [(Int, StateType)]

type Solutions = [History]

initialState, finalState :: StateType
initialState = State LeftBank LeftBank LeftBank LeftBank
finalState = State RightBank RightBank RightBank RightBank

position :: ItemType -> StateType ->  BankType
position Bono = bonoPos
position Edge = edgePos
position Larry = larryPos
position Adam = adamPos

updateState :: StateType -> ItemType -> BankType -> StateType
updateState state Bono pos = state {bonoPos = pos}
updateState state Edge pos = state {edgePos = pos}
updateState state Larry pos = state {larryPos = pos}
updateState state Adam pos = state {adamPos = pos}

opposite :: BankType -> BankType
opposite LeftBank = RightBank
opposite RightBank = LeftBank

notSeen :: StateType -> History -> Bool
notSeen state = all (\(_, s) -> state /= s)

writeItem :: ItemType -> BankType -> ShowS
writeItem Bono LeftBank
  = showString "    Bono |                    |\n"
writeItem Edge LeftBank
  = showString "The Edge |                    |\n"
writeItem Larry LeftBank
  = showString "   Larry |                    |\n"
writeItem Adam LeftBank
  = showString "    Adam |                    |\n"
writeItem Bono RightBank
  = showString "         |                    | Bono\n"
writeItem Edge RightBank
  = showString "         |                    | The Edge\n"
writeItem Larry RightBank
  = showString "         |                    | Larry\n"
writeItem Adam RightBank
  = showString "         |                    | Adam\n"

writeState :: StateType -> ShowS
writeState state
  = showString "----------------------------------------\n"
  . writeItem Bono (bonoPos state)
  . writeItem Edge (edgePos state)
  . writeItem Larry (larryPos state)
  . writeItem Adam (adamPos state)
  . showString "----------------------------------------\n"

totalTime :: History -> Int
totalTime ((time, _) : _) = time

writeHistory :: History -> ShowS
writeHistory [ ] = id
writeHistory history
  = foldr
    (\(time, state) acc ->
       showString "Time: "
     . shows (total - time)
     . showChar '\n'
     . writeState state . acc) id history
     where
       total = totalTime history

minSolutions :: Solutions -> Solutions
minSolutions [ ] = [ ]
minSolutions (history : next)
  = reverse (minAcc (totalTime history) [history] next)
      where
        minAcc minSoFar mins [ ] = mins
        minAcc minSoFar mins (history : next)
          = case compare minSoFar total of
              LT -> minAcc minSoFar mins next
              EQ -> minAcc minSoFar (history : mins) next
              GT -> minAcc total [history] next
            where
              total = totalTime history

writeSolutions :: Solutions -> Int -> ShowS
writeSolutions [ ] _ = id
writeSolutions (item : next) count
  = showString "Solution " . shows count . showChar '\n'
  . writeHistory item
  . writeSolutions next (succ count)

u2times :: ItemType -> Int
u2times Bono = 10
u2times Edge = 5
u2times Larry = 2
u2times Adam = 1

transfer :: StateType -> StateType -> BankType -> Int -> History -> Solutions

-- We are trying to get from a legal state, source, to another legal
-- state and history tells us one way to do it in time countdown
-- starting from dest where the torch is at location.
-- If we find newDest from which we can get to dest in one step
-- we can find all the solutions recursively.

transfer source dest location countdown history
  | source == dest
    = [(countdown, dest) : history]
  | otherwise
    = moveOne ++ moveTwo
        where
          newHistory = (countdown, dest) : history
          newLocation = opposite location

          moveOne = concat
                    [transfer
                       source newDest
                       newLocation newTime newHistory

                    | item <- [Bono .. Adam],
                      position item dest == location,
                      let newDest = updateState dest item newLocation,
                      notSeen newDest history,
                      let newTime = countdown + u2times item]

          moveTwo = concat
                    [transfer
                       source newDest
                       newLocation newTime newHistory

                    | i <- [Bono .. Larry],
                      j <- [succ i .. Adam],
                      position i dest == location &&
                      position j dest == location,
                      let
                        newDest
                          = updateState
                              (updateState dest i newLocation)
                              j newLocation,
                      notSeen newDest history,
                      let newTime = countdown + u2times i]


main :: IO ( )
main
  = putStr (writeSolutions mins 1 "")
      where
        solutions
          = transfer initialState finalState
                     RightBank 0 [ ]
        mins = minSolutions solutions

