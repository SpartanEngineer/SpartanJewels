module Main where

import System.Random(randomRIO)
import Data.Maybe
import qualified Data.Vector as V
import Graphics.UI.Gtk
import Control.Monad(replicateM)
import Control.Monad.IO.Class
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import System.IO.Unsafe(unsafePerformIO)

data JewelType = JDiamond | JStar | JCircle | JSquare | NoJewel deriving (Eq, Show)
data MatchInfo = MatchInfo {_matchSize :: Int, _matchRow :: Int, _matchCol :: Int, _matchIsVert :: Bool} deriving (Show)

type RowColIndex = (Int, Int)
data GUIEvent = RowColEvent RowColIndex | NewGameEvent deriving (Show)

data GameState = GameState {
    g_SelIndex1 :: Maybe RowColIndex
  , g_SelIndex2 :: Maybe RowColIndex
  , g_Points :: Int
  , g_Event :: GUIEvent
  , g_Jewels :: [JewelType]
  , g_Buttons :: [Button]
  , g_PointsLabel :: Label
}

fileSepChar :: String
fileSepChar = "/"
resourcesDir :: String
resourcesDir = "res"
imagesDir :: String
imagesDir = resourcesDir ++ fileSepChar ++ "images"

circleFileLocation :: String
circleFileLocation = imagesDir ++ fileSepChar ++ "circle.png"
pressedCircleFileLocation :: String
pressedCircleFileLocation = imagesDir ++ fileSepChar ++ "pressed_circle.png"
starFileLocation :: String
starFileLocation = imagesDir ++ fileSepChar ++ "star.png"
pressedStarFileLocation :: String
pressedStarFileLocation = imagesDir ++ fileSepChar ++ "pressed_star.png"
squareFileLocation :: String
squareFileLocation = imagesDir ++ fileSepChar ++ "square.png"
pressedSquareFileLocation :: String
pressedSquareFileLocation = imagesDir ++ fileSepChar ++ "pressed_square.png"
diamondFileLocation :: String
diamondFileLocation = imagesDir ++ fileSepChar ++ "diamond.png"
pressedDiamondFileLocation :: String
pressedDiamondFileLocation = imagesDir ++ fileSepChar ++ "pressed_diamond.png"

nCols :: Int
nCols = 8
nRows :: Int
nRows = 8

jewelTypes :: V.Vector JewelType
jewelTypes = V.fromList [JDiamond, JStar, JCircle, JSquare]

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

rowColRowDiff :: RowColIndex -> RowColIndex -> Int
rowColRowDiff (row1, _) (row2, _) = abs(row1 - row2)

rowColColDiff :: RowColIndex -> RowColIndex -> Int
rowColColDiff (_, col1) (_, col2) = abs(col1 - col2)

rowColToIndex :: Int -> Int -> Int
rowColToIndex row col = (row * nRows) + col

indexToRowCol :: Int -> (Int, Int)
indexToRowCol index = 
  let row = index `div` nRows
      col = index `mod` nCols
  in (row, col)

makeJewelMatchInfo :: Int -> Int -> Int -> Bool -> MatchInfo
makeJewelMatchInfo size index index2 isVert =
  case isVert of False -> MatchInfo{_matchSize=size, _matchRow=index2, _matchCol=index, _matchIsVert=False} 
                 True -> MatchInfo{_matchSize=size, _matchRow=index, _matchCol=index2, _matchIsVert=True} 

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
compareMatches (Just x) (Just y)
  | left >= right = Just x 
  | otherwise = Just y
  where left = _matchSize x
        right = _matchSize y

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
            replacedJewels <- V.mapM replaceNoJewelWithRandomJewel newJewels
            updateJewelGrid replacedJewels (points + (_matchSize justMatch))

getSwappedJewelGrid :: V.Vector JewelType -> RowColIndex -> RowColIndex -> V.Vector JewelType
getSwappedJewelGrid jewels index1 index2 = jewelsReplaced
  where row1 = fst index1
        col1 = snd index1
        row2 = fst index2
        col2 = snd index2

        swapJewel :: Int -> Int -> JewelType
        swapJewel r c 
          | r == row1 && c == col1 = (jewels V.! (rowColToIndex row2 col2))
          | r == row2 && c == col2 = (jewels V.! (rowColToIndex row1 col1))
          | otherwise = jewels V.! (rowColToIndex r c)

        jewelsReplaced = V.fromList [swapJewel i j | i <- [0..nRows-1], j <- [0..nCols-1]]

updateJewelGridState :: GameState -> RowColIndex -> RowColIndex -> IO GameState
updateJewelGridState state index1 index2 = do
  let jewels = g_Jewels state
  let swappedJewels = getSwappedJewelGrid (V.fromList jewels) index1 index2
  updatedGrid <- updateJewelGrid swappedJewels 0
  let updatedJewels = fst updatedGrid
  let points = snd updatedGrid
  let newPoints = (g_Points state) + points
  case points of
    0 -> return state
    _ -> return state {g_Jewels = (V.toList updatedJewels), g_Points = newPoints}

makeButtonAndAttach :: Grid -> Int -> Int -> IO (Button, AddHandler GUIEvent)
makeButtonAndAttach grid i j = do
  button <- buttonNew
  buttonAddHandler <- makeButtonAddHandler button i j
  gridAttach grid button j i 1 1
  return (button, buttonAddHandler)

makeButtonAddHandler :: Button -> Int -> Int -> IO (AddHandler GUIEvent)
makeButtonAddHandler button row col = do
  (addHandler, fire) <- newAddHandler
  button `on` buttonActivated $ do fire (RowColEvent (row, col))
  return addHandler

makeNewGameButtonAddHandler :: Button -> IO (AddHandler GUIEvent)
makeNewGameButtonAddHandler button = do
  (addHandler, fire) <- newAddHandler
  button `on` buttonActivated $ do fire NewGameEvent
  return addHandler

getJewelImage :: JewelType -> Bool -> IO Image
getJewelImage jt selected = do
  pixBuf <- pixbufNewFromFileAtScale fileName height width False
  img <- imageNewFromPixbuf pixBuf
  return img
  where height = 60
        width = 60
        fileName = case (jt, selected) of
                   (JStar, False) -> starFileLocation
                   (JStar, True) -> pressedStarFileLocation
                   (JCircle, False) -> circleFileLocation
                   (JCircle, True) -> pressedCircleFileLocation
                   (JDiamond, False) -> diamondFileLocation
                   (JDiamond, True) -> pressedDiamondFileLocation
                   (JSquare, False) -> squareFileLocation
                   (JSquare, True) -> pressedSquareFileLocation
                   (_, _) -> ""

updateButtonJewelImage :: (Button, JewelType, Bool) -> IO Button
updateButtonJewelImage (button, jt, selected) = do
  buttonChildren <- containerGetChildren button
  mapM (containerRemove button) buttonChildren

  img <- getJewelImage jt selected
  buttonBox <- hBoxNew False 0

  boxPackStart buttonBox img PackNatural 0
  containerAdd button buttonBox
  widgetShowAll button --doesn't update display without this!
  return button

updatePointsLabel :: Label -> Int -> IO Label
updatePointsLabel label points = do
  set label [labelText := ((show points) ++ " Points")]
  return label

processRowColEvent :: RowColIndex -> GameState -> IO GameState
processRowColEvent index state =
  let index1 = g_SelIndex1 state
      index2 = g_SelIndex2 state
      colDiff = rowColColDiff (fromJust index1) index
      rowDiff = rowColRowDiff (fromJust index1) index
      (minDiff, maxDiff) = (min colDiff rowDiff, max colDiff rowDiff)
  in case (index1, index2, minDiff, maxDiff) of 
    (Nothing, Nothing, _, _) -> return state {g_SelIndex1 = Just index, g_SelIndex2 = Nothing}
    (Just _, Just _, _, _) -> return state {g_SelIndex1 = Just index, g_SelIndex2 = Nothing}
    (Just a, Nothing, 0, 1) -> updateJewelGridState (state {g_SelIndex2 = Just index}) a index
    (_, _, _, _) -> return state {g_SelIndex1 = Nothing, g_SelIndex2 = Nothing}

processNewGameEvent :: GameState -> IO GameState
processNewGameEvent state = do
  randomJewels <- getRandomJewelList
  updatedJewelGridAndPoints <- updateJewelGrid (V.fromList randomJewels) 0
  let jewelGrid = V.toList (fst updatedJewelGridAndPoints)
  return (state {g_Points=0, g_SelIndex1=Nothing, g_SelIndex2=Nothing, g_Jewels=jewelGrid})

mergeState :: GUIEvent -> GameState -> GameState
--note: is there a way to avoid using unsafePerformIO here???
mergeState event state =
  let eventState = state {g_Event=event}
      newState = case event of 
                 RowColEvent index -> processRowColEvent index eventState
                 NewGameEvent -> processNewGameEvent eventState
  in unsafePerformIO newState

updateRowColEvent :: GameState -> IO()
updateRowColEvent state =
  let index1 = g_SelIndex1 state
      index2 = g_SelIndex2 state
      buttons = g_Buttons state
      pointsLabel = g_PointsLabel state
      points = g_Points state
      jewels = g_Jewels state
      zipped = zip3 buttons jewels (replicate (nCols*nRows) False)
  in case (index1, index2) of
    (Just a, Nothing) -> do mapM_ updateButtonJewelImage zipped
                            let buttonSelected = buttons !! (rowColToIndex (fst a) (snd a))
                            let jewelSelected = jewels !! (rowColToIndex (fst a) (snd a))
                            updateButtonJewelImage (buttonSelected, jewelSelected, True)
                            return ()
    (Just _, Just _) -> do mapM_ updateButtonJewelImage zipped
                           updatePointsLabel pointsLabel points
                           return ()
    (Nothing, Nothing) -> do mapM_ updateButtonJewelImage zipped
                             return ()
    (_, _) -> return ()

updateNewGameEvent :: GameState -> IO()
updateNewGameEvent state = 
  let buttons = g_Buttons state
      jewels = g_Jewels state
      points = g_Points state
      pointsLabel = g_PointsLabel state
      zipped = zip3 buttons jewels (replicate (nCols*nRows) False)
  in do
    mapM_ updateButtonJewelImage zipped
    updatePointsLabel pointsLabel points
    return ()

updateGUIDisplay :: GameState -> IO()
updateGUIDisplay state =
  let event = g_Event state
  in case event of
    RowColEvent _ -> updateRowColEvent state
    NewGameEvent -> updateNewGameEvent state

main :: IO ()
main = do
  initGUI
  window <- windowNew
  window `on` deleteEvent $ liftIO mainQuit >> return False --quit on close of window

  set window [ windowTitle := "SpartanJewels"
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

  mapM_ updateButtonJewelImage (zip3 buttons jewelGrid (replicate (nCols*nRows) False)) --set the buttons to the values of the jewelGrid

  hbox <- hBoxNew True 2
  newGameButton <- buttonNewWithMnemonic "New Game?"
  newGameButtonAddHandler <- makeNewGameButtonAddHandler newGameButton

  pointsLabel <- labelNewWithMnemonic "0 Points"
  boxPackStart hbox newGameButton PackNatural 0
  boxPackStart hbox pointsLabel PackNatural 0

  separator <- hSeparatorNew

  boxPackStart vbox grid PackNatural 0
  boxPackStart vbox separator PackNatural 0
  boxPackStart vbox hbox PackNatural 0

  let allGUIEventHandlers = newGameButtonAddHandler : (fmap snd buttonsAndHandlers) 

  let networkDescription :: MomentIO()
      networkDescription = do
        guiEventsList <- mapM fromAddHandler allGUIEventHandlers
        let mergedEvents = foldl (unionWith (\x _ -> x)) (head guiEventsList) (tail guiEventsList)

        let initialState = GameState {g_SelIndex1=Nothing, g_SelIndex2=Nothing, g_Points=0, g_Event=NewGameEvent, g_Jewels=jewelGrid, g_Buttons=buttons, g_PointsLabel=pointsLabel}
        buttonAccum <- accumE initialState (mergeState <$> mergedEvents)

        reactimate $ fmap updateGUIDisplay buttonAccum

  network <- compile networkDescription
  actuate network

  widgetShowAll window
  mainGUI
