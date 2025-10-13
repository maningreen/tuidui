module Main where

import Brick
import Brick.Widgets.Border (border, borderWithLabel, vBorder)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (uncons)
import qualified Data.Map as Map
import GHC.IO.Handle (hGetContents, hPutChar)
import GHC.IO.Handle.FD (openFile, withFile)
import GHC.IO.Handle.Text (hPutStrLn)
import GHC.IO.IOMode (IOMode (ReadWriteMode, WriteMode))
import Graphics.Vty (Event (EvKey), Key (..), black, defAttr, white)
import Util

data WidgetID = Doing Int | Todo Int
  deriving (Eq, Ord, Show)
data Mode = Normal | Insert | Help | Following Char | Any
  deriving (Ord, Eq, Show)
data Data = Data {_todo :: [String], _doing :: [String], _index :: WidgetID}
data State = State {_state :: Mode, _data :: Data}

normalPlus :: [Mode]
normalPlus =
  [ Help
  , Any
  ]
    ++ map Following [' ' .. 'z']

todoPath :: FilePath
todoPath = ".todo"

helpStrs :: [String]
helpStrs =
  [ "Mode Commands: Effect"
  , "Any Esc:                         exit mode (If Normal mode exits program)"
  , "Normal/Help q:                   exit mode ^"
  , "Normal gg:                       focus top"
  , "Normal G:                        focus bottom"
  , "Normal j, Down:                  focus down"
  , "Normal k, Up:                    focus up"
  , "Normal l, Left                   focus not done"
  , "Normal h, Right                  focus doing"
  , "Normal n, i, a:                  add new item"
  , "Normal x, d, r:                  remove item"
  , "Normal L:                        append to 'doing' list"
  , "Normal H:                        append to 'todo' list"
  , "Normal ?:                        show this help"
  , "Insert <Enter>                   exit mode"
  ]

inputs :: Map.Map (Key, Mode) (State -> EventM WidgetID State ())
inputs =
  Map.fromList
    [ ((KChar 'q', Normal), exitMode)
    , ((KBS, Normal), exitMode)
    , ((KChar 'q', Help), exitMode)
    , ((KEsc, Any), exitMode)
    , ((KChar 'g', Following 'g'), f . top)
    , ((KChar 'G', Normal), f . bottom)
    , ((KChar 'j', Normal), f . move 1)
    , ((KChar 'k', Normal), f . move (-1))
    , ((KUp, Normal), f . move (-1))
    , ((KDown, Normal), f . move 1)
    , ((KChar 'h', Normal), f . moveLeft)
    , ((KChar 'l', Normal), f . moveRight)
    , ((KChar 'n', Normal), f . add)
    , ((KChar 'i', Normal), f . add)
    , ((KChar 'x', Normal), f . remove)
    , ((KChar 'd', Normal), f . remove)
    , ((KChar 'r', Normal), f . remove)
    , ((KChar 'L', Normal), f . prependDoing)
    , ((KChar 'H', Normal), f . prependTodo)
    , ((KChar 'h', Help), f . help)
    , ((KChar '?', Normal), f . help)
    , ((KChar '?', Help), f . help)
    ]
 where
  f = modify . const

parseItems :: String -> ([String], [String])
parseItems = mapSnd tailSafe . break (== "") . lines

prependDoing :: State -> State
prependDoing s@(State m (Data t d i))
  | Todo x <- i = (State m (Data (dropIndex t x) (t !! x : d) (Doing 0)))
  | otherwise = s

prependTodo :: State -> State
prependTodo s@(State m (Data t d i))
  | Doing x <- i = State m (Data (d !! x : t) (dropIndex d x) (Todo 0))
  | otherwise = s

move :: Int -> State -> State
move i (State s (Data todo doing index))
  | Todo x <- index = State s $ Data todo doing (Todo $ clampIndex todo $ x + i)
  | Doing x <- index = State s $ Data todo doing (Doing $ clampIndex doing $ x + i)

bottom :: State -> State
bottom (State s (Data todo doing index))
  | Todo _ <- index = f . Todo $ length todo - 1
  | Doing _ <- index = f . Doing $ length doing - 1
 where
  f = State s . Data todo doing

top :: State -> State
top (State s (Data todo doing index))
  | Todo _ <- index = f $ Todo 0
  | Doing _ <- index = f $ Doing 0
 where
  f = State s . Data todo doing

moveLeft :: State -> State
moveLeft s@(State _ (Data todo doing i))
  | Doing x <- i = State Normal $ (Data todo doing $ Todo $ clampIndex todo x)
  | otherwise = s

moveRight :: State -> State
moveRight (State _ (Data todo doing (Todo i))) = State Normal $ (Data todo doing $ Doing $ clampIndex doing i)
moveRight s = s

remove :: State -> State
remove (State s (Data todo doing (Doing x))) = State s $ Data todo dropped $ Doing $ clampIndex dropped x
 where
  dropped = dropIndex doing x
remove (State s (Data todo doing (Todo x))) = State s $ Data dropped doing $ Todo $ clampIndex dropped x
 where
  dropped = dropIndex todo x

help :: State -> State
help s@(State Help _) = setStateMode Normal s
help s@(State _ _) = setStateMode Help s

add :: State -> State
add (State Normal (Data todo doing index))
  | Todo x <- index = State Insert $ Data (insertIndex (x + 1) "" todo) doing . Todo $ x + 1
  | Doing x <- index = State Insert $ Data todo (insertIndex (x + 1) "" doing) . Doing $ x + 1
add s = s

setStateMode :: Mode -> State -> State
setStateMode m (State _ d) = State m d

setStatePure :: Mode -> State -> State
setStatePure m (State _ d) = State m d

handleInsert :: Key -> State -> EventM WidgetID State ()

handleInsert (KChar k) (State Insert (Data [] doing (Todo i))) = modify . const . State Insert . Data [[k]] doing $ Todo i
handleInsert (KChar k) (State Insert (Data todo doing (Todo i))) = modify . const . State Insert . Data (applyIndex (++ [k]) todo i) doing $ Todo i

handleInsert (KChar k) (State Insert (Data todo [] (Doing i))) = modify . const . State Insert . Data todo [[k]] $ Doing i
handleInsert (KChar k) (State Insert (Data todo doing (Doing i))) = modify . const . State Insert . Data todo (applyIndex (++ [k]) doing $ i) $ Doing i

handleInsert (KBS) (State Insert (Data todo doing (Doing i))) = modify . const . State Insert . Data todo (applyIndex (initSafe) doing $ i) $ Doing i
handleInsert (KBS) (State Insert (Data todo doing (Todo i))) = modify . const . State Insert . Data (applyIndex (initSafe) todo i) doing $ Todo i

handleInsert (KEnter) (State Insert (Data todo doing x))
  | Doing i <- x, null (doing !! i) = modify . const $ State Normal (Data todo (dropIndex doing $ clampIndex doing i) x)
  | Todo i <- x, null (todo !! i) = modify . const $ State Normal (Data (dropIndex todo i) doing x)
  | otherwise = modify . const $ State Normal (Data todo doing x)

handleInsert _ _ = return ()

closeApp :: Data -> IO ()
closeApp (Data todo doing _) =
  withFile
    todoPath
    WriteMode
    ( \hdl -> do
        mapM_ (hPutStrLn hdl) todo
        hPutChar hdl '\n'
        mapM_ (hPutStrLn hdl) doing
    )

exitMode :: State -> EventM WidgetID State ()
exitMode s@(State Insert _) = modify . const $ setStateMode Normal s
exitMode (State Normal theData) = liftIO (closeApp theData) >> halt
exitMode x = modify . const $ setStateMode Normal x

app :: App State () WidgetID
app =
  App
    { appDraw = draw
    , appHandleEvent = handleEvent
    , appChooseCursor = const $ fmap fst . uncons
    , appStartEvent = return ()
    , appAttrMap =
        const $
          attrMap
            defAttr
            [ (attrName "selected", black `on` white)
            ]
    }
 where
  handleEvent :: BrickEvent WidgetID () -> EventM WidgetID State ()
  handleEvent (VtyEvent (EvKey k [])) = do
    st <- get
    case (_state st, k) of
      (Insert, c)
        | KBS <- c -> handleInsert c st
        | KEnter <- c -> handleInsert c st
        | KChar _ <- c -> handleInsert c st
        | KEsc <- c -> modify . const $ setStateMode Normal st
      (state, c)
        | (Just f) <- spec -> f st
        | elem state normalPlus, (Just f) <- gen -> f (State Normal $ _data st)
        | (KChar i) <- c -> modify . const $ setStateMode (Following i) st
        | otherwise -> return ()
       where
        spec = Map.lookup (k, state) inputs
        gen = Map.lookup (k, Normal) inputs
  handleEvent _ = return ()

  drawBorder = borderWithLabel . str

  draw s@(State n _) = str (show n) : drawState s

  generateWidgets :: Bool -> Data -> ([Widget WidgetID], [Widget WidgetID])
  generateWidgets b d = (getTodoWids b d, getDoingWids b d)

  formatWidgets :: Mode -> ([Widget WidgetID], [Widget WidgetID]) -> Widget WidgetID
  formatWidgets m (todo, doing) = borderWithLabel (str $ show m) $ hCenter (makeViewport Todo . vBox $ map hCenter todo) <+> vBorder <+> hCenter (makeViewport Doing . vBox $ map hCenter doing)

  makeViewport :: (Int -> WidgetID) -> Widget WidgetID -> Widget WidgetID
  makeViewport f = viewport (f (-1)) Vertical

  renderState :: State -> Widget WidgetID
  renderState (State m d) = formatWidgets m $ generateWidgets True d

  getTodoWids True (Data todo _ i) = [(if Todo index == i then visible . withAttr (attrName "selected") else id) . hCenter . str $ a | (a, index) <- zip todo [0 ..]]
  getTodoWids False (Data todo _ _) = map (str . (\x -> if x == "" then " " else x)) todo
  getDoingWids True (Data _ doing i) = [(if Doing index == i then visible . withAttr (attrName "selected") else id) . hCenter . str $ a | (a, index) <- zip doing [0 ..]]
  getDoingWids False (Data _ doing _) = map (str . (\x -> if x == "" then " " else x)) doing

  drawState :: State -> [Widget WidgetID]
  drawState s@(State Normal (Data todo doing _))
    | null doing && null todo = return . drawBorder "Todo" . center $ str "No Items! (For help type ?)"
    | otherwise = return $ renderState s
  drawState (State Insert d) = return . formatWidgets Insert . applyCursor d $ generateWidgets False d
   where
    applyCursorToWidget :: WidgetID -> Int -> Widget WidgetID -> Widget WidgetID
    applyCursorToWidget id i wid = Brick.showCursor id (Location (i, 0)) wid

    applyCursor :: Data -> ([Widget WidgetID], [Widget WidgetID]) -> ([Widget WidgetID], [Widget WidgetID])
    applyCursor (Data todo _ (Todo i)) (a, b) = (applyIndex (applyCursorToWidget (Todo i) (length $ todo !! i)) a i, b)
    applyCursor (Data _ doing (Doing i)) (a, b) = (a, applyIndex (applyCursorToWidget (Todo i) (length $ doing !! i)) b i)
  drawState (State Help x) = (centerLayer . border . vBox $ map str helpStrs) : drawState (State Normal x)
  drawState (State _ d) = drawState $ State Normal d

startApp :: IO State
startApp = do
  hdl <- openFile todoPath ReadWriteMode
  conts <- hGetContents hdl
  let (todo, doing) = parseItems conts
  print todo
  print doing
  defaultMain app $ State Normal $ Data todo doing $ Todo 0

main :: IO State
main = startApp
