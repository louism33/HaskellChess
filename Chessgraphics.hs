import Chesslogic
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Control.Monad
import Data.Functor
import Control.Applicative
import Data.Maybe
import Data.List

debug = False

sdim = 1200
size = 50
offsetX = (- fromIntegral sdim) / 8
offsetY = (- fromIntegral sdim) / 8
    
resize :: Size -> Path -> Path
resize k = fmap (\ (x, y) -> (x * k, y * k))


drawpawn :: Color -> Size -> Coordinates -> Picture
drawpawn col k ((x, y), p) = 
    let x' = k * x
        y' = k * y
        szz = 0.2
        sz = 0.2
    in color col $ translate x' y' $ Pictures $ fmap (polygon . resize k)
         [ [ (-sz, -sz), (sz, -sz), (0, sz/1.1) ]
         ] ++ (thickCircle (0.1 * k) (0.15 * k)) : []

drawknight :: Color -> Size -> Coordinates -> Picture
drawknight col k ((x, y), p) = 
    let x' = k * x
        y' = k * y
        szz = 0.2
        sz = 0.3
    in color col $ translate x' y' $ Pictures $ fmap (polygon . resize k)
         [
        [ (-sz, -sz), (0, -sz), (0, sz) ] 
        , [ (0, sz), (0, 0), (szz, 0) ] 
        , [ (0, 0), (szz, 0), (szz/2, -szz/2) ]
         ]

drawbishop :: Color -> Size -> Coordinates -> Picture
drawbishop col k ((x, y), p) = 
    let x' = k * x
        y' = k * y
        szz = 0.2
        sz = 0.3
    in color col $ translate x' y' $ Pictures $ fmap (polygon . resize k)
         [ 
        [ (-szz/4, -sz), (szz/4, -sz), (szz/4, sz), (-szz/4, sz) ] -- vertical
        , [ (-szz, szz/2), (szz, szz/2), (szz, szz), (-szz, szz) ] -- horizontal
         ]

drawrook :: Color -> Size -> Coordinates -> Picture
drawrook col k ((x, y), p) = 
    let x' = k * x
        y' = k * y
        szz = 0.2
        sz = 0.3
    in color col $ translate x' y' $ Pictures $ fmap (polygon . resize k)
         [ [ (-sz, -sz), (-sz/2, -sz), (-sz/2, sz), (-sz, sz) ] --battlement left
            , [ (sz/2, -sz), (sz, -sz), (sz, sz), (sz/2, sz) ] -- battlement right
            , [ (-sz/4, -sz), (sz/4, -sz), (sz/4, sz), (-sz/4, sz) ] --battelement centre
            , [ (-szz, -szz), (szz, -szz), (szz, szz), (-szz, szz) ]
            , [ (-sz, -sz), (0, -sz), (0, 0), (-sz, 0) ]
         --   , [ (-sz, -sz), (sz, -sz), (sz, 0), (-sz, 0) ] -- commenting this gives you "door"
         ]

drawqueen :: Color -> Size -> Coordinates -> Picture
drawqueen col k ((x, y), p) = 
    let x' = k * x
        y' = k * y
        szz = 0.25
        sz = 0.35
    in color col $ translate x' y' $ Pictures $ 
        fmap (polygon . resize k)
         [ [ (-sz, -szz), (-szz, -sz), (sz, szz), (szz, sz) ]
        , [ (sz, -szz), (szz, -sz), (-sz, szz), (-szz, sz) ]
        , [ (-sz/4, -szz), (sz/4, -szz), (sz/4, sz), (-sz/4, sz) ]
        , [ (-szz, -szz/2), (szz, -szz/2), (szz, szz/2), (-szz, szz/2) ]
        , [ (-szz/2, -szz), (szz/2, -szz), (szz/2, szz), (-szz/2, szz) ]
         ]

drawking :: Color -> Size -> Coordinates -> Picture
drawking col k ((x, y), p) = 
    let x' = k * x
        y' = k * y
        szz = 0.25
        sz = 0.35
    in color col $ translate x' y' $ Pictures $  fmap (polygon . resize k)
         [ [ (-sz, -sz), (sz, -sz), (0, szz) ]
         ] ++ (thickCircle (0.2 * k) (0.2 * k)) : []

drawbox col k (x, y) =
    let x' = k * x
        y' = k * y
        szz = 1
        sz = 2
    in color col $ translate x' y' $ Pictures $  fmap (polygon . resize k)
         [ [ (-sz, -sz), (sz, -sz), (sz, sz), (-sz, sz) ]
         ]

drawbutton col k (x, y) = 
    let x' = k * x
        y' = k * y
        szz = 0.5
        sz = 2
    in color col $ translate x' y' $ Pictures $  fmap (line . resize k)
         [ [ (-sz, -szz), (sz, -szz), (sz, szz), (-sz, szz), (-sz, -szz) ]
         ]

drawprev col k (x, y) =
    let x' = k * x
        y' = k * y
        szz = 0.5
        sz = 1
    in color col $ translate x' y' $ Pictures $  fmap (polygon . resize k)
         [ 
         [ (sz, -szz), (sz, szz), (-sz, 0) ]
         ]

drawnext col k (x, y) =
    let x' = k * x
        y' = k * y
        szz = 0.5
        sz = 1
    in color col $ translate x' y' $ Pictures $  fmap (polygon . resize k)
         [ 
         [ (-sz, -szz), (-sz, szz), (sz, 0)]
         ]

legalmoves :: Board -> Maybe Coordinates -> [Coordinates]
legalmoves _ Nothing = []
legalmoves b (Just c) = wherecanIgo c (player b) b

drawboard k b = Pictures $ gridcolours ++ grid : pieces ++ highlite : infobox : whichplayer : prevbutton : nextbutton : (if debug then allposmoves : [] else [] ) 
    where
    infobox = drawbox (light $ light orange) k (-6.5, 0.5)
    
    whichplayer = case player b of
    
        White -> drawking whitecolor (k) ( (-6.5, 0.5), Empty )
        
        Black -> drawking blackcolor (k) ( (-6.5, 0.5), Empty )
    
    nextbutton = Pictures $ (drawbutton (light $ light orange) k (-6.5, 4) ) : (drawnext (light orange) k (-6.5, 4)) : []
    
    prevbutton = Pictures $ (drawbutton (light orange) k (-6.5, 3) ) : (drawprev (light orange) k (-6.5, 3)) : []
    
    help1 = translate (-400) (0) $ scale 0.1 0.1 $ text $ show (length $ prevboards b)
    help2 = translate (-400) (-10) $ scale 0.1 0.1 $ text $ show (length $ blacks b)
    help3 = translate (-400) (-20) $ scale 0.1 0.1 $ text $ show (length $ emptysquares b)
    
    helpt = translate (-400) (-40) $ scale 0.1 0.1 $ text $ show ((length $ whites b) + (length $ blacks b) + (length $ emptysquares b))
    
    highlite :: Picture
    highlite
        | highlight b == Nothing = Blank
        | otherwise =  translate (offsetX) (offsetY)  $   
            Pictures [ 
                if (fst lm `elem` (if player b == White then whitespots b else blackspots b) ) then drawCross red $ fst lm 
                else ( 
                    if (fst lm `elem` (if player b == Black then whitespots b else blackspots b) ) then (drawCross (dark $ dark green) $ fst lm)
                    else (drawCross (green) $ fst lm) 
                    )
                | lm <- legalmoves b $ highlight b ]
    
    drawCross farbe (x, y) =
      let x' = k * x
          y' = k * y
      in color farbe $ translate x' y' $ Pictures
         $ fmap (polygon . resize k)
         [ [ (-0.35, -0.25), (-0.25, -0.35), (0.35,0.25), (0.25, 0.35) ]
         , [ (0.35, -0.25), (0.25, -0.35), (-0.35,0.25), (-0.25, 0.35) ]
         ]
    
    allposmoves = translate (offsetX) (offsetY)  $   
            Pictures [ (drawBabyCross (black) $ fst pm) 
                | pm <- allpossiblemoves (player b) b ]
    
    drawBabyCross farbe (x, y) =
      let x' = k * x
          y' = k * y
      in color farbe $ translate x' y' $ Pictures
         $ fmap (polygon . resize (k/5))
         [ [ (-0.35, -0.25), (-0.25, -0.35), (0.35,0.25), (0.25, 0.35) ]
         , [ (0.35, -0.25), (0.25, -0.35), (-0.35,0.25), (-0.25, 0.35) ]
         ]
    
    boardcolora = dark orange
    boardcolorb = light orange
    
    whitecolor = light $ light $ light yellow
    blackcolor = light black
    
    whitepawns = fmap (drawpawn whitecolor k) (filter (\x -> snd x == Pawn) $ whites b)
    whiteknights = fmap (drawknight whitecolor k) (filter (\x -> snd x == Knight) $ whites b)
    whitebishops = fmap (drawbishop whitecolor k) (filter (\x -> snd x == Bishop) $ whites b)
    whiterooks = fmap (drawrook whitecolor k) (filter (\x -> snd x == Rook) $ whites b)
    whitequeens = fmap (drawqueen whitecolor k) (filter (\x -> snd x == Queen) $ whites b)
    whitekings = fmap (drawking whitecolor k) (filter (\x -> snd x == King) $ whites b)
    
    allwhites = whitepawns ++ whiteknights ++ whitebishops ++ whiterooks ++ whitequeens ++ whitekings
    
    blackpawns = fmap (drawpawn blackcolor k) (filter (\x -> snd x == Pawn) $ blacks b)
    blackknights = fmap (drawknight blackcolor k) (filter (\x -> snd x == Knight) $ blacks b)
    blackbishops = fmap (drawbishop blackcolor k) (filter (\x -> snd x == Bishop) $ blacks b)
    blackrooks = fmap (drawrook blackcolor k) (filter (\x -> snd x == Rook) $ blacks b)
    blackqueens = fmap (drawqueen blackcolor k) (filter (\x -> snd x == Queen) $ blacks b)
    blackkings = fmap (drawking blackcolor k) (filter (\x -> snd x == King) $ blacks b)
    
    allblacks = blackpawns ++ blackknights ++ blackbishops ++ blackrooks ++ blackqueens ++ blackkings
    
    pieces = fmap (translate (offsetX) (offsetY) ) $ allblacks ++ allwhites
    
    gridcolours = fmap (translate (offsetX) (offsetY) ) $ [gridcolours' boardcolora (2*i, 2*j ) | i <- [0..3], j <- [0..3] ] 
            ++ [gridcolours' boardcolora (1 + 2*i, 1 + 2*j) | i <- [0..3], j <- [0..3] ]
            
            ++ [gridcolours' boardcolorb (2*i, 1 + 2*j) | i <- [0..3], j <- [0..3] ]
            ++ [gridcolours' boardcolorb (1 + 2*i, 2*j) | i <- [0..3], j <- [0..3] ]
    
    gridcolours' col (x, y) =
      let x' = k * x
          y' = k * y
      in color col $ translate x' y' $ Pictures
         $ fmap (polygon . resize k)
         [ [ (-0.5, -0.5), (0.5, -0.5), (0.5,0.5), (-0.5, 0.5) ]
         ]
     
    grid :: Picture
    grid = translate offsetX offsetY $ color black $ Pictures $ fmap (line . resize k)
       [ -- horizontal lines 
        [(-0.5, -0.5), (7.5 , -0.5)]
        , [(-0.5, 0.5), (7.5 , 0.5)]
        , [(-0.5, 1.5), (7.5 , 1.5)]
        , [(-0.5, 2.5), (7.5 , 2.5)]
        , [(-0.5, 3.5), (7.5 , 3.5)]
        , [(-0.5, 4.5), (7.5 , 4.5)]
        , [(-0.5, 5.5), (7.5 , 5.5)]
        , [(-0.5, 6.5), (7.5 , 6.5)]
        , [(-0.5, 7.5), (7.5 , 7.5)]
        -- vertical lines
        , [(-0.5 , -0.5), (-0.5 , 7.5)]
        , [(0.5 , -0.5), (0.5 , 7.5)]
        , [(1.5, -0.5), (1.5, 7.5)]
        , [(2.5 , -0.5), (2.5 , 7.5)]
        , [(3.5 , -0.5), (3.5, 7.5)]
        , [(4.5, -0.5), (4.5, 7.5)]
        , [(5.5 , -0.5), (5.5 , 7.5)]
        , [(6.5 , -0.5), (6.5 , 7.5)]
        , [(7.5 , -0.5), (7.5 , 7.5)]
       ]

checkCoordinate :: Size -> Float -> Maybe Float
checkCoordinate k f' =
  let f = f' / k
  in  (-1) <$ guard (-8.5 < f && f < -4.5)  -- clicking on button
  <|> 0 <$ guard (-3.5 < f && f < -2.5)
  <|> 1    <$ guard (-2.5 < f && f < -1.5)
  <|> 2    <$ guard (-1.5 < f && f < -0.5)
  <|> 3    <$ guard (-0.5 < f && f < 0.5)
  <|> 4    <$ guard (0.5 < f && f < 1.5)
  <|> 5    <$ guard (1.5 < f && f < 2.5)
  <|> 6    <$ guard (2.5 < f && f < 3.5)
  <|> 7    <$ guard (3.5 < f && f < 4.5)
  
keys :: Size -> Event -> Board -> Board
keys k (EventKey (MouseButton LeftButton) Down _ (x', y')) b 
    | True =
      fromMaybe b $ do
        x <- checkCoordinate (k) x'
        y <- checkCoordinate (k) y'
        if ((x == -1) && (y == 6)) then ( return prevmove ) 
        else ( 
            if ( ((x == -1) && (y == 7))) then ( return nextmove) 
            else 
            ( return (makeMove ((x, y), Empty) b) ) ) 
    | otherwise = b
    
    where 
    prevmove = if (length (prevboards b) == 1) then startingboard else (
        (prevboards b !! 1) { 
            nextboards = b : (nextboards b)
            })
    
    nextmove 
        | length (nextboards b) > 0 = nextboards b !! 0
        | otherwise = b
    

keys _ _ b = b

makeMove :: Coordinates -> Board -> Board
makeMove coords b 
    | highlight b == Nothing = case player b of 
        White -> b {
            highlight = if occupyingpiece == Empty then Nothing else Just (fst coords, occupyingpiece)
            }
        Black -> b {
            highlight = if occupyingpiece == Empty then Nothing else Just (fst coords, occupyingpiece)
            }
            
    | physicallypossiblemove && enemytarget = 
        case player b of 
            White -> (
                let 
                possibleboard = Board { whites = ( ( filter (\x -> x /= (fromJust $ highlight b) ) (whites b) ) 
                        ++ (fst coords, snd (fromJust $ highlight b) ) : [] )
                    , blacks = ( filter (\(x, z) -> x /= ((fst coords)) ) (blacks b) )
                    , emptysquares = (emptysquares b) ++ ((fst $ fromJust $ highlight b), Empty) : []
                    , player = Black
                    , highlight = Nothing
                    , cancastle = castlehelperwhite
                    , prevboards = possibleboard : (prevboards b)
                    , nextboards = []
                    }
                leadstocheck = incheckhypothetical Black (whites possibleboard) possibleboard
                in (if (leadstocheck) then (b) else (possibleboard))
                )
                
            Black -> (
                let 
                possibleboard = Board { whites = ( filter (\(x, z) -> x /= ((fst coords)) ) (whites b) )
                    , blacks = ( filter (\x -> x /= (fromJust $ highlight b) ) (blacks b) ) 
                    ++ (fst coords, snd (fromJust $ highlight b) ) : []
                    , emptysquares = (emptysquares b) ++ ((fst $ fromJust $ highlight b), Empty) : []
                    , player = White
                    , highlight = Nothing
                    , cancastle = castlehelperblack
                    , prevboards = possibleboard : (prevboards b)
                    , nextboards = []
                    }
                leadstocheck = incheckhypothetical White (blacks possibleboard) possibleboard
                in (if (leadstocheck) then (b) else (possibleboard))
                )
    
    | physicallypossiblemove && emptytarget = 
        case player b of 
            White -> (
                let 
                possibleboard = Board {
                    whites = whitemovehelper 
                    , blacks = blacks b
                    , emptysquares = emptymovehelperwhite
                    , player = Black
                    , highlight = Nothing
                    , cancastle = castlehelperwhite
                    , prevboards = possibleboard : (prevboards b)
                    , nextboards = []
                    }
                leadstocheck = incheckhypothetical Black (whites possibleboard) possibleboard
                in (if (leadstocheck) then (b) else (possibleboard))
                )
            
            Black -> (
                let 
                possibleboard = Board { whites = whites b 
                    , blacks = blackmovehelper
                    , emptysquares = emptymovehelperblack
                    , player = White
                    , highlight = Nothing
                    , cancastle = castlehelperblack
                    , prevboards = possibleboard : (prevboards b)
                    , nextboards = []
                    }
                leadstocheck = incheckhypothetical White (blacks possibleboard) possibleboard
                in (if (leadstocheck) then (b) else (possibleboard))
                )
        
    | otherwise = (
        case player b of 
            White -> b {
                highlight = Nothing
                }
            Black -> b {
                highlight = Nothing
                } 
        ) 
            
            
    where
    king = filter (\(x, z) -> z == King) (col b) -- get my king
    justpossiblecoords player board = [ fst $ (allpossiblemoves (player) board) !! i | i <- [0.. ((length $ allpossiblemoves (player) board) - 1) ] ]
    currentlyincheck = (fst $ king !! 0) `elem` (justpossiblecoords (enemyplayer b) b) 
    testcurrentlyincheck board = (fst $ king !! 0) `elem` (justpossiblecoords (enemyplayer b) board)
    kinghypo coo = filter (\(x, z) -> z == King) (coo) -- get king's (possible) new position
    incheckhypothetical player coo board = (fst $ kinghypo coo !! 0) `elem` (justpossiblecoords player board)
    
    incheckmate player coo = currentlyincheck && (noncheckmoves player coo == 0)
    noncheckmoves player coo = length [ incheckhypothetical player coo (b)]
    
    whitemovepiece = ( ( filter (\x -> x /= (fromJust $ highlight b) ) (whites b) ) ++ (fst coords, snd (fromJust $ highlight b) ) : [] )
    
    whitemovehelper 
        | ( (cancastle b !! 0) && ( (snd $ fromJust $ highlight b) == King) && (fst coords == (6, 0) ) )
            = ( ( filter (\x -> x /= ( (7, 0), Rook) ) (whitemovepiece) ) ++ ( (5, 0), Rook) : [] )
        | ( (cancastle b !! 1) && ( (snd $ fromJust $ highlight b) == King) && (fst coords == (2, 0) ) )
            = ( ( filter (\x -> x /= ( (0, 0), Rook) ) (whitemovepiece) ) ++ ( (3, 0), Rook) : [] )
        | otherwise = whitemovepiece 
    
    updateempty = ( filter (\x -> x /= (fst coords, Empty) ) (emptysquares b) ) ++ (  ((fst $ fromJust $ highlight b), Empty)  ) : []
    emptymovehelperwhite
        | ( (cancastle b !! 0) && ( (snd $ fromJust $ highlight b) == King) && (fst coords == (6, 0) ) )
            = ( ( filter (\x -> x /= ( (5, 0), Empty) ) (updateempty) ) ++ ( (7, 0), Empty) : [] )
            
        | ( (cancastle b !! 1) && ( (snd $ fromJust $ highlight b) == King) && (fst coords == (2, 0) ) )
            = ( ( filter (\x -> x /= ( (3, 0), Empty) ) (updateempty) ) ++ ( (0, 0), Empty) : [] )
            
        | otherwise = updateempty 
    
    
    blackmovepiece = ( ( filter (\x -> x /= (fromJust $ highlight b) ) (blacks b) ) ++ (fst coords, snd (fromJust $ highlight b) ) : [] )
    blackmovehelper 
        | ( (cancastle b !! 2) && ( (snd $ fromJust $ highlight b) == King) && (fst coords == (2, 7) ) )
            = ( ( filter (\x -> x /= ( (0, 7), Rook) ) (blackmovepiece) ) ++ ( (3, 7), Rook) : [] )
        | ( (cancastle b !! 3) && ( (snd $ fromJust $ highlight b) == King) && (fst coords == (6, 7) ) )
            = ( ( filter (\x -> x /= ( (7, 7), Rook) ) (blackmovepiece) ) ++ ( (5, 7), Rook) : [] )
        | otherwise = blackmovepiece 
     
    emptymovehelperblack
        | ( (cancastle b !! 2) && ( (snd $ fromJust $ highlight b) == King) && (fst coords == (6, 7) ) )
            = ( ( filter (\x -> x /= ( (5, 7), Empty) ) (updateempty) ) ++ ( (7, 7), Empty) : [] )
            
        | ( (cancastle b !! 3) && ( (snd $ fromJust $ highlight b) == King) && (fst coords == (2, 7) ) )
            = ( ( filter (\x -> x /= ( (3, 7), Empty) ) (updateempty) ) ++ ( (0, 7), Empty) : [] )
            
        | otherwise = updateempty 
    
    castlehelperwhite 
        | ( (snd $ fromJust $ highlight b) == King) = [False, False, (cancastle b !! 2), (cancastle b !! 3) ] 
        | ( (fromJust $ highlight b) == ((7, 0), Rook) ) = [False, (cancastle b !! 1), (cancastle b !! 2), (cancastle b !! 3) ] 
        | ( (fromJust $ highlight b) == ((0, 0), Rook) ) = [(cancastle b !! 0), False, (cancastle b !! 2), (cancastle b !! 3) ] 
        | otherwise = cancastle b
    
    castlehelperblack
        | ( (snd $ fromJust $ highlight b) == King) = [(cancastle b !! 0), (cancastle b !! 1), False, False ] 
        | ( (fromJust $ highlight b) == ((0, 7), Rook) ) = [(cancastle b !! 0), (cancastle b !! 1), False, (cancastle b !! 3) ] 
        | ( (fromJust $ highlight b) == ((7, 7), Rook) ) = [(cancastle b !! 0), (cancastle b !! 1), (cancastle b !! 2), False ] 
        | otherwise = cancastle b 
    
    col = if (player b == White) then whites else blacks
    enemycol = if (player b == Black) then whites else blacks
    enemyplayer b = if (player b == White) then Black else White
    
    possiblepiece = (filter (\(cs, piece) -> (cs == (fst coords) ) ) (col b))
    occupyingpiece = if length possiblepiece > 0 then snd $ possiblepiece !! 0 else Empty
    plop = legalmoves b (highlight b)
    legalcoords = [ fst $ plop !! i | i <- [0..((length plop) - 1)] ]
    
    physicallypossiblemove = ( (fst coords) `elem` (legalcoords) )
    
    emptytarget = ( (fst coords) `elem` (remainers b) )
    
    spots = if (player b == White) then blackspots b else whitespots b
    enemytarget = ( (fst coords) `elem` (spots) )
    
    acceptablemove = physicallypossiblemove && (emptytarget || enemytarget)
    

time f b 
    | ispawnatendw b = b { whites = (filter (/= thatwhitepawn b !! 0) (whites b)) ++ [ (( fst (thatwhitepawn b !! 0) ), Queen ) ] }
    | ispawnatendb b = b { blacks = (filter (/= thatblackpawn b !! 0) (blacks b)) ++ [ (( fst (thatblackpawn b !! 0) ), Queen ) ] }
    | otherwise = b
    
ispawnatendw b = (length $ thatwhitepawn b ) > 0
thatwhitepawn b = filter (\(x, z) -> (z == Pawn) && (snd x == 7) ) (whites b)

ispawnatendb b = (length $ thatblackpawn b ) > 0
thatblackpawn b = filter (\(x, z) -> (z == Pawn) && (snd x == 0) ) (blacks b)


main = play window white 10 startingboard (drawboard size) (keys size) time
    where
    window = InWindow "Chess" (sdim, sdim * 9 `div` 16) (100, 0)



