{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Data.Monoid

import Control.Monad.State

import Control.Lens

inhaleTo = 35
inhaleDiameter = 15

exhaleTo = 13
exhaleDiameter = 10

background = makeColor8 255 231 186 0
inhale = makeColor8 84 255 157 255
inhaleTransparent = makeColor8 84 255 157 127
exhale = makeColor8 135 206 250 255
exhaleTransparent = makeColor8 135 206 250 127

data BreathDirection = Inhale | Exhale deriving (Ord, Eq)

data BreathState = BreathState { _level :: Float
                               , _direction :: BreathDirection
                               }

data GameState = GameState { _keyState :: KeyboardState
                           , _breath :: BreathState
                           }

data KeyboardState = KeyboardState { _spacePressed :: Bool }

makeLenses ''BreathState
makeLenses ''GameState
makeLenses ''KeyboardState

type Time = Float

main :: IO ()
main = play (InWindow "Relax!" (800, 600) (0,0)) background 60 (GameState (KeyboardState False) (BreathState 30 Exhale)) render processInput tick

tick :: Float -> GameState -> GameState
tick f = execState $ do
    gameState <- get
    if view (keyState . spacePressed) gameState
      then breath.level += 8*f
      else breath.level -= 8*f
    breath.level %= max 0

processInput :: Event -> GameState -> GameState
processInput (EventKey (SpecialKey KeySpace) Down _ _) = execState $ do
    game <- get
    when (inExhaleRing (game ^. breath) && game ^. breath.direction == Exhale) 
         (breath.direction .= Inhale)
    keyState.spacePressed .= True
processInput (EventKey (SpecialKey KeySpace) Up _ _) = execState $ do 
    game <- get
    when (inInhaleRing (game ^. breath) && game ^. breath.direction == Inhale) 
         (breath.direction .= Exhale)
    keyState.spacePressed .= False
processInput _ = id

between :: Ord a => a -> a -> a -> Bool
between target low hi = low < target && target < hi

inInhaleRing :: BreathState -> Bool
inInhaleRing breath = between (breath ^. level) (inhaleTo - inhaleDiameter/2) (inhaleTo + inhaleDiameter/2)

inExhaleRing :: BreathState -> Bool
inExhaleRing breath = between (breath ^. level) (exhaleTo - exhaleDiameter/2) (exhaleTo + exhaleDiameter/2)

render :: GameState -> Picture
render game = mconcat  [ renderRiver game
                       , renderBreath game
                       ]

renderRiver :: GameState -> Picture
renderRiver game = mconcat [ translate 0 (-100) (rectangleSolid 800 5)
                           , translate 0 0 (rectangleSolid 800 3)
                           ]

renderBreath :: GameState -> Picture
renderBreath gameState = translate  (-350) (-250) (mconcat  [ renderInhaleRing gameState
                                                            , renderExhaleRing gameState
                                                            , thickCircle (gameState ^. breath.level) 1])

renderInhaleRing :: GameState -> Picture
renderInhaleRing gameState = case gameState ^. breath.direction of
                             Inhale -> color inhale (thickCircle inhaleTo inhaleDiameter)
                             Exhale -> color inhaleTransparent (thickCircle inhaleTo inhaleDiameter)

renderExhaleRing :: GameState -> Picture
renderExhaleRing gameState = case gameState ^. breath.direction of
                             Exhale -> color exhale (thickCircle exhaleTo exhaleDiameter)
                             Inhale -> color exhaleTransparent (thickCircle exhaleTo exhaleDiameter)
