module Main where

import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as Map
import GHC.IO.Handle (hGetContents, hPutStr)
import GHC.IO.Handle.FD (openFile, withFile)
import GHC.IO.IOMode (IOMode (ReadWriteMode, WriteMode))
import Graphics.Vty (Event (EvKey), Key (..), black, defAttr, white)
import Data.List (uncons)

type WidgetID = Int
data Modes = Normal | Insert | Help | Following Char | Any
  deriving (Ord, Eq, Show)
data Data = Data {_items :: String, _index :: WidgetID}
data State = State {_state :: Modes, _data :: Data}

normalPlus :: [Modes]
normalPlus = [
    Help,
    Any
  ] ++ map Following [' '..'z']

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
  , "Normal n, i, a:                  add new item"
  , "Normal x, d, r:                  remove item"
  , "Normal ?, h:                     show this help"
  , "Insert <Enter>                   exit mode"
  ]

inputs :: Map.Map (Key, Modes) (State -> EventM WidgetID State ())
inputs =
  Map.fromList
    [ ((KChar 'q', Normal), exitMode)
    , ((KBS, Any), exitMode)
    , ((KChar 'q', Help), exitMode)
    , ((KEsc, Any), exitMode)
    , ((KChar 'g', Following 'g'), top)
    , ((KChar 'G', Normal), bottom)
    , ((KChar 'j', Normal), move 1)
    , ((KUp, Normal), move (-1))
    , ((KDown, Normal), move (1))
    , ((KChar 'k', Normal), move (-1))
    , ((KChar 'n', Normal), add)
    , ((KChar 'i', Normal), add)
    , ((KChar 'x', Normal), remove)
    , ((KChar 'd', Normal), remove)
    , ((KChar 'r', Normal), remove)
    , ((KChar 'h', Normal), help)
    , ((KChar 'h', Help), help)
    , ((KChar '?', Normal), help)
    , ((KChar '?', Help), help)
    ]
clampI :: [a] -> Int -> Int
clampI a b
  | b < 0 = 0
  | b > l = l
  | otherwise = b
 where
  l = length a - 1

move :: Int -> State -> EventM WidgetID State ()
move i (State Normal (Data items index))
  | index + i >= (length . lines) items || index + i < 0 = return ()
  | otherwise = modify . const . State Normal $ Data items (index + i)
move _ (State _ _) = return ()

bottom :: State -> EventM WidgetID State ()
bottom (State s (Data items _)) = modify . const . State s . Data items $ length (lines items) - 1

top :: State -> EventM WidgetID State ()
top (State _ (Data items _)) = modify . const . State Normal . Data items $ 0

remove :: State -> EventM WidgetID State ()
remove (State s (Data items index)) = modify . const . State s . Data (unlines lose) $ clampI lose index
 where
  lns = lines items
  lose = take index lns ++ drop (index + 1) lns

help :: State -> EventM WidgetID State ()
help s@(State Help _) = setStateMode Normal s
help s@(State _ _) = setStateMode Help s

add :: State -> EventM WidgetID State ()
add (State Normal (Data items index)) = modify . const $ State Insert $ Data items index
add _ = return ()

setStateMode :: Modes -> State -> EventM WidgetID State ()
setStateMode m (State _ d) = modify . const $ State m d

manageInsert :: Key -> State -> EventM WidgetID State ()
manageInsert KBS (State _ (Data s i))
  | '\n' <- last s = modify . const $ State Normal (Data (init s) i)
  | otherwise = modify . const $ State Insert (Data (unlines $ init $ lines s) i)
manageInsert KEnter (State _ (Data s i)) 
  | '\n' <- last s = modify . const $ State Normal (Data (init s) i)
  | otherwise = bottom $ State Normal (Data (s ++ "\n") i)
manageInsert (KChar c) (State _ (Data s i)) = modify . const $ State Insert (Data (s ++ [c]) i)
manageInsert _ _ = return ()

closeApp :: String -> IO ()
closeApp s = withFile todoPath WriteMode (`hPutStr` s)

exitMode :: State -> EventM WidgetID State ()
exitMode s@(State Insert _) = manageInsert KEnter s
exitMode (State Normal (Data s _)) = liftIO (closeApp s) >> halt
exitMode x = setStateMode Normal x

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
        | KBS <- c -> manageInsert c st
        | KEnter <- c -> manageInsert c st
        | KChar _ <- c -> manageInsert c st
      (state, c)
        | (Just f) <- spec -> f st
        | elem state normalPlus, (Just f) <- gen -> f (State Normal $ _data st)
        | (KChar i) <- c -> setStateMode (Following i) st
        | otherwise -> return ()
       where
        spec = Map.lookup (k, state) inputs
        gen = Map.lookup (k, Normal) inputs
  handleEvent _ = return ()

  drawBorder = borderWithLabel (str "Todo items")
  drawContents :: State -> [Widget WidgetID]

  drawState :: Modes -> Widget WidgetID
  drawState = str . show

  draw s@(State n _) = drawState n : drawContents s

  drawContents (State Normal (Data c i))
    | null $ lines c = return $ drawBorder . center $ str "No Items! (For help type ?)"
    | otherwise = return . drawBorder . center $ v
   where
    lns = map (hCenter . str) $ lines c
    wid = vBox $ [if b == i then visible $ withAttr (attrName "selected") a else a | (a, b) <- zip lns [0 ..]]
    v = viewport (-1) Vertical wid
  drawContents (State Help x) = (centerLayer . border . vBox $ map str helpStrs) : drawContents (State Normal x)
  drawContents (State Insert (Data items _)) = drawContents . State Normal . Data items $ -1
  drawContents (State _ d) = drawContents $ State Normal d

startApp :: IO State
startApp = do
  hdl <- openFile todoPath ReadWriteMode
  conts <- hGetContents hdl
  defaultMain app $ State Normal $ Data conts 0

main :: IO State
main = startApp
