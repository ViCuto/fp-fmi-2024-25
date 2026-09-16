module TicTacToe where

import Matrix

data Marker = X | O
  deriving (Eq, Show)

type Spot = Maybe Marker

type Board = Matrix Spot

data Result = Full | HasEmpty | Wins Marker
  deriving (Show)

infixr 2 `join`

join :: Result -> Result -> Result
join (Wins x) _ = Wins x
join _ (Wins x) = Wins x
join HasEmpty _ = HasEmpty
join _ HasEmpty = HasEmpty
join Full Full = Full

checkThreeSpots :: Thrice Spot -> Result
checkThreeSpots x = case (x Zero, x One, x Two) of
    (Just m1, Just m2, Just m3) 
        | m1 == m2 && m2 == m3 -> Wins m1  
        | otherwise -> Full    
    _ -> HasEmpty  

winnerRows :: Board -> Result
winnerRows (MkMatrix x) = 
    foldr join HasEmpty [checkThreeSpots (thrice (x Zero Zero) (x Zero One) (x Zero Two)),
                         checkThreeSpots (thrice (x One Zero) (x One One) (x One Two)),
                         checkThreeSpots (thrice (x Two Zero) (x Two One) (x Two Two))]

winnerCols :: Board -> Result
winnerCols (MkMatrix x) = 
    foldr join HasEmpty [checkThreeSpots (thrice (x Zero Zero) (x One Zero) (x Two Zero)),
                         checkThreeSpots (thrice (x Zero One) (x One One) (x Two One)),
                         checkThreeSpots (thrice (x Zero Two) (x One Two) (x Two Two))]

winnerDiags :: Board -> Result
winnerDiags (MkMatrix x) = 
    foldr join HasEmpty [checkThreeSpots (thrice (x Zero Zero) (x One One) (x Two Two)),
                         checkThreeSpots (thrice (x Zero Two) (x One One) (x Two Zero))]

winner :: Board -> Result
winner board = join (join (winnerRows board) (winnerCols board)) (winnerDiags board)

emptySpots :: Board -> [(Three, Three)]
emptySpots (MkMatrix x) =  [ (i, j) | i <- [Zero, One, Two], j <- [Zero, One, Two], x i j == Nothing ]
