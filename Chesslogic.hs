module Chesslogic where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Control.Monad
import Data.Functor
import Control.Applicative
import Data.Maybe
import Data.List

data Piece = Empty | Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Bounded, Eq, Enum)
type Size = Float
type Hor = Float
type Ver = Float
type Coordinates = ((Hor, Ver), Piece)

data Player = White | Black
    deriving (Show, Eq, Bounded, Enum)
data Board = Board
  { whites :: [Coordinates]
  , blacks :: [Coordinates]
  , emptysquares :: [Coordinates]
  , player  :: Player
  , highlight :: Maybe Coordinates
  , cancastle :: [Bool] --whiteright, whiteleft, blackright, blackleft
  , prevboards :: [Board]
  , nextboards :: [Board]
  }
  deriving (Show, Eq)

wherecanIgo :: Coordinates -> Player -> Board -> [Coordinates]
wherecanIgo c p b
    | piece == Empty = []
    | piece == Pawn = 
        let 
            moves = ( 
                (if ( ((hor, op ver 1), Empty) `elem`  (emptysquares b) ) then [ (hor, op ver 1) ] else []) 
                ++ ( if ( (ver == startingver) && ( ((hor, op ver 2), Empty) `elem`  (emptysquares b) ) && ( ((hor, op ver 1), Empty) `elem`  (emptysquares b) )) then [ (hor, op ver 2) ] else [] )
                ++ ( if pawncantakemin then [ (hor - 1, op ver 1 ) ] else [] )
                ++ ( if pawncantakepls then [ (hor + 1, op ver 1 ) ] else [] )      )
            
            legmoves = filter (killillegals c) moves
        in [ (lm, piece) | lm <- legmoves ] 
    
    | piece == Knight = 
        let
            moves = ( [ (hor + i, ver + j) | i <- [-1 , 1] , j <- [-2 , 2]  ]
                ++ [ (hor + i, ver + j) | i <- [-2 , 2] , j <- [-1 , 1]  ]  )
            legmoves = filter (killillegals c ) moves
        in [ (lm, piece) | lm <- legmoves ] 
    
    | piece == Bishop = 
        let
            moves = ( 
                ( takeWhile (\ (x, y) -> ( (x - 1, y - 1) == (hor, ver) ) || ((x - 1, y - 1) `elem` (remainers b) ) ) [ (hor + i, ver + i) | i <- [1 .. 7] ] )
                ++ 
                ( takeWhile (\ (x, y) -> ( (x + 1, y + 1) == (hor, ver) ) || ((x + 1, y + 1) `elem` (remainers b) ) ) [ (hor - i, ver - i) | i <- [1 .. 7] ] )
                ++
                ( takeWhile (\ (x, y) -> ( (x + 1, y - 1) == (hor, ver) ) || ((x + 1, y - 1) `elem` (remainers b) ) ) [ (hor - i, ver + i) | i <- [1 .. 7] ] )
                ++ 
                ( takeWhile (\ (x, y) -> ( (x - 1, y + 1) == (hor, ver) ) || ((x - 1, y + 1) `elem` (remainers b) ) ) [ (hor + i, ver - i) | i <- [1 .. 7] ] )
                )
            legmoves = filter (killillegals c ) moves
        in [ (lm, piece) | lm <- legmoves ] 
    
    | piece == Rook = 
        let
            moves = (
                ( takeWhile (\ (x, y) -> ( (x - 1, y) == (hor, ver) ) || ((x - 1, y) `elem` (remainers b) ) ) [ (hor + i, ver) | i <- [1 .. 7] ] )
                ++ 
                ( takeWhile (\ (x, y) -> ( (x + 1, y) == (hor, ver) ) || ((x + 1, y) `elem` (remainers b) ) ) [ (hor - i, ver ) | i <- [1 .. 7] ] )
                ++
                ( takeWhile (\ (x, y) -> ( (x, y - 1) == (hor, ver) ) || ((x, y - 1) `elem` (remainers b) ) ) [ (hor, ver + i) | i <- [1 .. 7] ] )
                ++ 
                ( takeWhile (\ (x, y) -> ( (x, y + 1) == (hor, ver) ) || ((x, y + 1) `elem` (remainers b) ) ) [ (hor, ver - i) | i <- [1 .. 7] ] )
                )
            legmoves = filter (killillegals c ) moves
        in [ (lm, piece) | lm <- legmoves ] 
        
    | piece == Queen = 
        let
            moves = (
            --rook
                ( takeWhile (\ (x, y) -> ( (x - 1, y) == (hor, ver) ) || ((x - 1, y) `elem` (remainers b) ) ) [ (hor + i, ver) | i <- [1 .. 7] ] )
                ++ 
                ( takeWhile (\ (x, y) -> ( (x + 1, y) == (hor, ver) ) || ((x + 1, y) `elem` (remainers b) ) ) [ (hor - i, ver ) | i <- [1 .. 7] ] )
                ++
                ( takeWhile (\ (x, y) -> ( (x, y - 1) == (hor, ver) ) || ((x, y - 1) `elem` (remainers b) ) ) [ (hor, ver + i) | i <- [1 .. 7] ] )
                ++ 
                ( takeWhile (\ (x, y) -> ( (x, y + 1) == (hor, ver) ) || ((x, y + 1) `elem` (remainers b) ) ) [ (hor, ver - i) | i <- [1 .. 7] ] )
                ++
            -- bishop
                ( takeWhile (\ (x, y) -> ( (x - 1, y - 1) == (hor, ver) ) || ((x - 1, y - 1) `elem` (remainers b) ) ) [ (hor + i, ver + i) | i <- [1 .. 7] ] )
                ++ 
                ( takeWhile (\ (x, y) -> ( (x + 1, y + 1) == (hor, ver) ) || ((x + 1, y + 1) `elem` (remainers b) ) ) [ (hor - i, ver - i) | i <- [1 .. 7] ] )
                ++
                ( takeWhile (\ (x, y) -> ( (x + 1, y - 1) == (hor, ver) ) || ((x + 1, y - 1) `elem` (remainers b) ) ) [ (hor - i, ver + i) | i <- [1 .. 7] ] )
                ++ 
                ( takeWhile (\ (x, y) -> ( (x - 1, y + 1) == (hor, ver) ) || ((x - 1, y + 1) `elem` (remainers b) ) ) [ (hor + i, ver - i) | i <- [1 .. 7] ] )
                )
            legmoves = filter (killillegals c ) moves
        in [ (lm, piece) | lm <- legmoves ] 
    
    | piece == King = 
        let
            moves = ( [ (hor + i, ver) | i <- [-1 , 1] ] 
                ++ [ (hor, ver + i ) |  i <- [-1 , 1]  ] 
                ++ [ (hor + i, ver + i) |  i <- [-1 , 1] ] 
                ++ [ (hor - i, ver + i ) |  i <- [-1 , 1] ]
                 )
            castlingwr = if ( (p == White) && ( (hor, ver) == startingkingcoords) && ( ((hor + 1, ver), Empty) `elem`  (emptysquares b) )
                    && (cancastle b !! 0) ) then ( [(hor + 2, ver)]  ) else []
            castlingwl = if ( (p == White) && ( (hor, ver) == startingkingcoords) && ( ((hor - 1, ver), Empty) `elem`  (emptysquares b) )
                    && (cancastle b !! 1) )  then ( [(hor - 2, ver)]  ) else []
                    
            castlingbr = if ((p == Black) && ( (hor, ver) == startingkingcoords) && ( ((hor - 1, ver), Empty) `elem`  (emptysquares b) )
                    && (cancastle b !! 2) ) then  ( [(hor - 2, ver)]  ) else []
            castlingbl = if ( (p == Black) && ( (hor, ver) == startingkingcoords) && ( ((hor + 1, ver), Empty) `elem`  (emptysquares b) )
                    && (cancastle b !! 3) ) then ( [(hor + 2, ver)]  ) else []
            castling = castlingwl ++ castlingwr ++ castlingbr ++ castlingbl
            
            legmoves = filter (killillegals c ) (moves ++ castling)
        in [ (lm, piece) | lm <- legmoves ] 
    
    
    | otherwise = []
    
    where
    killillegals c (a, b) = (a >= 0 && a <= 7 && b >= 0 && b <= 7) && ( (a, b) /= (fst c) )
    piece = snd c
    hor = fst $ fst c
    ver = snd $ fst c
    op = if p == White then (+) else (-)
    startingver = if p == White then 1 else 6
    
    startingkingcoords = if p == White then (4, 0) else (4, 7)
    
    pawncantakemin = (  (hor - 1, op ver 1 )  ) `elem` (if p == White then blackspots b else whitespots b)
    pawncantakepls = (  (hor + 1, op ver 1 )  ) `elem` (if p == White then blackspots b else whitespots b)
    
    
allpossiblemoves :: Player -> Board -> [Coordinates]
allpossiblemoves p b = 
    let 
    col = if p == White then whites else blacks
    
    listlist = [ wherecanIgo whichpiece p b  | whichpiece <- col b] 
  
    allmoves = concat listlist
  
    in allmoves



blackspots b = [ fst $ blacks b !! i | i <- [0..( (length $ blacks b ) -1)] ]
whitespots b = [ fst $ whites b !! i | i <- [0..( (length $ whites b ) -1)] ]

blankb = [ (fromIntegral i, fromIntegral j) | i <- [0..7], j <- [0..7] ]

remainers :: Board -> [(Hor, Ver)]
remainers b = filter (\x -> (not (x `elem` blackspots b)) && (not (x `elem` whitespots b)) ) (blankb)
    
calculateempties b = [ (r, Empty) | r <- remainers b] 

startingboard = Board {
    whites = [ ((i, 1), Pawn) | i <- [0..7] ] ++ [ ((7 * i, 0), Rook) | i <- [0..1] ] 
        ++ [ ((1 + 5 * i, 0), Knight) | i <- [0..1] ]
        ++ [ ((2 + 3 * i, 0), Bishop) | i <- [0..1] ]  
        ++ [ ((4, 0), King) ] 
        ++ [ ((3, 0), Queen) ]
    
    , blacks = [ ((i, 6), Pawn) | i <- [0..7] ] ++ [ ((7 * i, 7), Rook) | i <- [0..1] ] 
        ++ [ ((1 + 5 * i, 7), Knight) | i <- [0..1] ]
        ++ [ ((2 + 3 * i, 7), Bishop) | i <- [0..1] ] 
        ++ [ ((4, 7), King) ]
        ++ [ ((3, 7), Queen) ]
    
    , emptysquares = calculateempties startingboard
    
    , player = White
    
    , highlight = Nothing 
    
    , cancastle = [True, True, True, True]
    
    , prevboards = [startingboard]
    
    , nextboards = []
    }


