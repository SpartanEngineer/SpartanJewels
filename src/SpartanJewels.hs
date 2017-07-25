module Main where

import System.Random(randomRIO)
import Data.Maybe
import qualified Data.Vector as V
import Graphics.UI.Gtk
import Control.Monad(replicateM)
import Control.Monad.IO.Class
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

data JewelType = JDiamond | JStar | JCircle | JSquare | JX | NoJewel deriving (Eq, Show)
data MatchInfo = MatchInfo {_matchSize :: Int, _matchRow :: Int, _matchCol :: Int, _matchIsVert :: Bool} deriving (Show)
type RowColIndex = (Int, Int)
data GameState = GameState {
    g_SelIndex1 :: Maybe RowColIndex
  , g_SelIndex2 :: Maybe RowColIndex
  , g_Points :: Int
} deriving (Show)

nCols :: Int
nCols = 8
nRows :: Int
nRows = 8

jewelTypes :: V.Vector JewelType
jewelTypes = V.fromList [JDiamond, JStar, JCircle, JSquare, JX]

getRandomJewel :: IO JewelType
getRandomJewel = do
  a <- randomRIO(0, (V.length jewelTypes - 1))
  return (jewelTypes V.! a)

getRandomJewelList :: IO [JewelType]
getRandomJewelList = replicateM (nCols*nRows) $ getRandomJewel

replaceNoJewelWithRandomJewel :: JewelType -> IO JewelType
replaceNoJewelWithRandomJewel jewel = do
  case jewel of
       NoJewel -> getRandomJewel
       _ -> return jewel

rowColToIndex :: Int -> Int -> Int
rowColToIndex row col = (row * nRows) + col

indexToRowCol :: Int -> (Int, Int)
indexToRowCol index = 
  let row = index `div` nRows
      col = index `mod` nCols
  in (row, col)

makeJewelMatchInfo :: Int -> Int -> Int -> Bool -> MatchInfo
makeJewelMatchInfo size index index2 isVert =
  case isVert of False -> MatchInfo{_matchSize=size, _matchRow=index2, _matchCol=index, _matchIsVert=True} 
                 True -> MatchInfo{_matchSize=size, _matchRow=index, _matchCol=index2, _matchIsVert=False} 

getJewelMatch :: V.Vector JewelType -> Int -> Int -> Bool -> Maybe MatchInfo
--j must be a single row or column, call with i equal to the length of j - 1
getJewelMatch _ 0 _ _ = Nothing
getJewelMatch _ 1 _ _ = Nothing
getJewelMatch j i index2 isVert
  | match5 = Just (makeJewelMatchInfo 5 (i-4) index2 isVert)
  | match4 = Just (makeJewelMatchInfo 4 (i-3) index2 isVert)
  | match3 = Just (makeJewelMatchInfo 3 (i-2) index2 isVert)
  | i < 0 = Nothing
  | i >= (V.length j) = Nothing
  | otherwise = getJewelMatch j (i-1) index2 isVert
  where match5 = i >= 4 && (j V.! i) == (j V.! (i - 4)) && (j V.! i) == (j V.! (i - 3)) && (j V.! i) == (j V.! (i - 2)) && (j V.! i) == (j V.! (i - 1))
        match4 = i >= 3 && (j V.! i) == (j V.! (i - 3)) && (j V.! i) == (j V.! (i - 2)) && (j V.! i) == (j V.! (i - 1))
        match3 = i >= 2 && (j V.! i) == (j V.! (i - 2)) && (j V.! i) == (j V.! (i - 1))

compareMatches :: Maybe MatchInfo -> Maybe MatchInfo -> Maybe MatchInfo
compareMatches x Nothing = x
compareMatches Nothing y = y
compareMatches x y
  | left >= right = x 
  | otherwise = y
  where left = _matchSize (fromJust x)
        right = _matchSize (fromJust y)

getBestJewelMatch :: V.Vector JewelType -> Maybe MatchInfo
getBestJewelMatch jewels =
  let rows = V.fromList [V.fromList [jewels V.! (rowColToIndex i j) | j <- [0..nCols-1]] | i <- [0..nRows-1]]
      cols = V.fromList [V.fromList [jewels V.! (rowColToIndex i j) | i <- [0..nRows-1]] | j <- [0..nCols-1]]
      rowMatches = [getJewelMatch (rows V.! i) (nCols-1) i False | i <- [0..nRows-1]]
      colMatches = [getJewelMatch (cols V.! i) (nRows-1) i True | i <- [0..nCols-1]]
      bestMatch = foldl compareMatches Nothing (rowMatches ++ colMatches)
  in bestMatch

getNewJewelGrid :: V.Vector JewelType -> MatchInfo -> V.Vector JewelType
--removes the jewels from the match
getNewJewelGrid jewels info
  | mIsVert == True = vertReplaced
  | mIsVert == False = horizReplaced
  where mIsVert = (_matchIsVert info)
        mRow = _matchRow info
        mCol = _matchCol info
        mMatchSize = _matchSize info

        getVertJewel :: Int -> Int -> JewelType
        getVertJewel row col
          | col == mCol && row >= 0 && row < mMatchSize = NoJewel
          | col == mCol && row >= mMatchSize && row <= (mRow + mMatchSize - 1) = jewels V.! (rowColToIndex (row - mMatchSize) col)
          | otherwise = jewels V.! (rowColToIndex row col)

        getHorizJewel :: Int -> Int -> JewelType
        getHorizJewel row col
          | col >= mCol && col <= mCol + (mMatchSize - 1) && row == 0 = NoJewel
          | col >= mCol && col <= mCol + (mMatchSize - 1) && row > 0 && row <= mRow = jewels V.! (rowColToIndex (row - 1) col)
          | otherwise = jewels V.! (rowColToIndex row col)

        vertReplaced = V.fromList [getVertJewel i j | i <- [0..nRows-1], j <- [0..nCols-1]]
        horizReplaced = V.fromList [getHorizJewel i j | i <- [0..nRows-1], j <- [0..nCols-1]]

updateJewelGrid :: V.Vector JewelType -> Int -> IO (V.Vector JewelType, Int)
updateJewelGrid jewels points = do
  let match = getBestJewelMatch jewels
  case match of
       Nothing -> return (jewels, points)
       _ -> do
            let justMatch = fromJust match
            let newJewels = getNewJewelGrid jewels justMatch
            replacedJewels <- sequence $ (V.map replaceNoJewelWithRandomJewel newJewels)
            updateJewelGrid replacedJewels (points + (_matchSize justMatch))

makeButtonAndAttach :: Grid -> Int -> Int -> IO (Button, AddHandler RowColIndex)
makeButtonAndAttach grid i j = do
  button <- buttonNew
  button `on` buttonActivated $ do
    set button [ buttonLabel := "clicked" ]
    widgetModifyFg button StateNormal (Color 65535 0 0)

  buttonAddHandler <- makeButtonAddHandler button i j

  gridAttach grid button j i 1 1
  return (button, buttonAddHandler)

makeButtonAddHandler :: Button -> Int -> Int -> IO (AddHandler RowColIndex)
makeButtonAddHandler button row col = do
  (addHandler, fire) <- newAddHandler
  _ <- button `on` buttonActivated $ do fire (row, col)
  return addHandler

updateButtonText :: (Button, JewelType) -> IO Button
updateButtonText pair = do
  let button = fst pair
  let jewel = snd pair
  set button [buttonLabel := (show jewel)]
  return button

updatePointsLabel :: Label -> Int -> IO Label
updatePointsLabel label points = do
  set label [labelText := ((show points) ++ " Points")]
  return label

main :: IO ()
main = do
  initGUI
  window <- windowNew
  window `on` deleteEvent $ liftIO mainQuit >> return False --quit on close of window

  set window [ windowTitle := "Jewels"
             , windowResizable := True
             , windowDefaultHeight := 600
             , windowDefaultWidth := 600 ]

  vbox <- vBoxNew False 10
  containerAdd window vbox

  grid <- gridNew
  gridSetRowHomogeneous grid True
  gridSetColumnHomogeneous grid True
  widgetSetSizeRequest grid 600 550

  buttonsAndHandlers <- sequence $ [makeButtonAndAttach grid i j | i <- [0..(nRows-1)], j <- [0..(nCols-1)]]
  let buttons = fmap fst buttonsAndHandlers
  randomJewels <- getRandomJewelList
  updatedJewelGridAndPoints <- updateJewelGrid (V.fromList randomJewels) 0
  let jewelGrid = V.toList (fst updatedJewelGridAndPoints)

  _ <- sequence $ map updateButtonText (zip buttons jewelGrid) --set the buttons to the values of the jewelGrid

  hbox <- hBoxNew True 2
  newGameButton <- buttonNewWithMnemonic "New Game?"
  pointsLabel <- labelNewWithMnemonic "0 Points"
  boxPackStart hbox newGameButton PackNatural 0
  boxPackStart hbox pointsLabel PackNatural 0

  separator <- hSeparatorNew

  boxPackStart vbox grid PackNatural 0
  boxPackStart vbox separator PackNatural 0
  boxPackStart vbox hbox PackNatural 0

  let networkDescription :: MomentIO()
    networkDescription = do
      buttonEventsList <- sequence $ fmap fromAddHandler (fmap snd buttonsAndHandlers)
      let mergedEvents = foldl (unionWith (\x _ -> x)) (head buttonEventsList) (tail buttonEventsList)

      let initialState = GameState {g_SelIndex1=Nothing, g_SelIndex2=Nothing, g_Points=0}
      buttonAccum <- accumE initialState ((\_ y -> y) <$> mergedEvents)

      reactimate $ fmap print buttonAccum

  network <- compile networkDescription
  actuate network

  widgetShowAll window
  mainGUI
