module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import Data.Functor((<$>))
import System.Exit

--------------------------------------------------------------------------------
--Types

type GameState = InputState

data InputState =
  InputState { a :: Bool
             , b :: Bool
             } deriving Show

--------------------------------------------------------------------------------
--Settings

fullScreen :: Bool
fullScreen = False

windowSize :: (Int, Int)
windowSize = (1280, 720)

windowName :: String
windowName = "My Game"

bgColor :: Color
bgColor = black

fps :: Int
fps = 30

--------------------------------------------------------------------------------
--Input State

initInputState :: InputState
initInputState = InputState False False

--------------------------------------------------------------------------------
--Display

getWindowPoint :: IO (Int, Int)
getWindowPoint = diff windowSize <$> getScreenSize
  where diff (a, b) (x, y) = ((x-a) `div` 2, (y-b) `div` 2)

getDisplay :: IO Display
getDisplay = if fullScreen then pure FullScreen else InWindow windowName windowSize <$> getWindowPoint

--------------------------------------------------------------------------------
--Game State

initGameState :: GameState
initGameState = initInputState

--------------------------------------------------------------------------------
--Drawing Functions

drawNothing :: Applicative f => a -> f Picture
drawNothing = const $ pure Blank

draw :: GameState -> IO Picture
draw = drawNothing

--------------------------------------------------------------------------------
--Generic Event/Time Handling

ignoreHandling :: Applicative f => a -> b -> f b
ignoreHandling = const pure

printHandling :: Show a => a -> b -> IO b
printHandling a b = const b <$> (putStrLn $ show a)

printState :: Show b => a -> b -> IO b
printState a b = const b <$> (putStrLn $ show b)

--------------------------------------------------------------------------------
--Event Handling

eventHandler :: Event -> GameState -> IO GameState
eventHandler e@(EventKey _ _ _ _) = eventKeyHandler e
eventHandler _ = pure

eventKeyHandler :: Event -> GameState -> IO GameState
eventKeyHandler (EventKey k s _ _) state = case k of
  (Char 'a') -> pure $ state {a = ksToBool s}
  (Char 'b') -> pure $ state {b = ksToBool s}
  (SpecialKey KeyEsc) -> exitSuccess

ksToBool :: KeyState -> Bool
ksToBool Down = True
ksToBool Up = False

--------------------------------------------------------------------------------
--Time Handling

timeHandler :: Float -> GameState -> IO GameState
timeHandler = printState

--------------------------------------------------------------------------------
--Main

main :: IO ()
main = getDisplay >>= (\display -> playIO display bgColor fps initGameState draw eventHandler timeHandler)

--------------------------------------------------------------------------------

{-
event and time printing functions:

when printing time, it will effectively print (1/fps) every (1/fps) seconds

when printing events, it will print each time any button is pressed or released,
as well as modifiers held at that time along with the mouse co-ordinates when it triggered

it will also print each time the mouse position changes and its new co-ordinates

response to events is NOT dependent on fps, it happens instantly
-}
