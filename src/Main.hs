module Main where

import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as Map
import Data.Maybe (isJust)
import GHC.IO.Handle (hGetContents, hPutStr)
import GHC.IO.Handle.FD (openFile, withFile)
import GHC.IO.IOMode (IOMode (ReadWriteMode, WriteMode))
import Graphics.Vty (Event (EvKey), Key (..), black, defAttr, white)

type WidgetID = Int
data Modes = Normal | Insert | Help | Any
  deriving (Ord, Eq, Bounded, Enum, Show)
data Data = Data {_items :: String, _index :: WidgetID}
data State = State {_state :: Modes, _data :: Data}

todoPath :: FilePath
todoPath = ".todo"

helpStrs :: [String]
helpStrs =
  [ "Mode Commands: Effect"
  , "Any Esc:                         exit mode (If Normal mode exits program)"
  , "Normal/Help q:                   exit mode ^"
  , "Normal g:                        focus top"
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
    , ((KChar 'q', Help), exitMode)
    , ((KEsc, Any), exitMode)
    , ((KChar 'g', Normal), bottom)
    , ((KChar 'G', Normal), top)
    , ((KChar 'j', Normal), move 1)
    , ((KChar 'k', Normal), move (-1))
    , ((KChar 'n', Normal), add)
    , ((KChar 'i', Normal), add)
    , ((KChar 'x', Normal), remove)
    , ((KChar 'd', Normal), remove)
    , ((KChar 'r', Normal), remove)
    , ((KChar 'h', Normal), help)
    , ((KChar '?', Normal), help)
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
top (State s (Data items _)) = modify . const . State s . Data items $ 0

remove :: State -> EventM WidgetID State ()
remove (State s (Data items index)) = modify . const . State s . Data (unlines lose) $ clampI lose index
 where
  lns = lines items
  lose = take index lns ++ drop (index + 1) lns

help :: State -> EventM WidgetID State ()
help = setStateMode Help

add :: State -> EventM WidgetID State ()
add (State Normal (Data items index)) = modify . const . State Insert $ Data items index
add _ = return ()

setStateMode :: Modes -> State -> EventM WidgetID State ()
setStateMode m (State _ d) = modify . const $ State m d

manageInsert :: Key -> State -> EventM WidgetID State ()
manageInsert KBS (State _ (Data s i))
  | [_] <- last (lines s) = modify . const $ State Normal (Data (init s) i)
  | otherwise = modify . const $ State Insert (Data (init s) i)
manageInsert KEnter (State _ (Data s i)) = bottom $ State Normal (Data (s ++ "\n") i)
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
    , appChooseCursor = const . const $ Nothing
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
    x <- get
    case (_state x, k) of
      (Insert, c)
        | KBS <- c -> manageInsert c x
        | KEnter <- c -> manageInsert c x
        | KChar _ <- c -> manageInsert c x
      (state, _)
        | isJust spec -> mapM_ (get >>=) spec
        | otherwise -> mapM_ (get >>=) gen
       where
        spec = Map.lookup (k, state) inputs
        gen = Map.lookup (k, Any) inputs
  handleEvent _ = return ()

  drawBorder = borderWithLabel (str "Todo items")
  draw :: State -> [Widget WidgetID]

  draw (State Normal (Data c i))
    | null $ lines c = return . drawBorder . center $ str "No Items! (For help type ?)"
    | otherwise = return . drawBorder . center $ v
   where
    lns = map (hCenter . str) $ lines c
    wid = vBox $ [if b == i then visible $ withAttr (attrName "selected") a else a | (a, b) <- zip lns [0 ..]]
    v = viewport (-1) Vertical wid
  draw (State Help x) = (centerLayer . border . vBox $ map str helpStrs) : draw (State Normal x)
  draw (State Insert (Data items _)) = draw . State Normal . Data (items ++ "█") $ -1
  draw (State s _) = return . str $ "No rendering for state: " ++ show s

startApp :: IO State
startApp = do
  hdl <- openFile todoPath ReadWriteMode
  conts <- hGetContents hdl
  defaultMain app $ State Normal $ Data conts 0

main :: IO State
main = startApp
