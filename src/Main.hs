{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Juicy

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
                               , _longestStreak :: Int
                               , _currentStreak :: Int
                               }

data GameState = GameState { _keyState :: KeyboardState
                           , _breath :: BreathState
                           , _viewPort :: ViewPort
                           , _bird :: Bird
                           , _assets :: Assets
                           , _time :: Float
                           , _ripplePos :: Ripple
                           }

data KeyboardState = KeyboardState { _spacePressed :: Bool
                                   , _lastPress :: Float
                                   }

data Bird = Bird { _position :: (Float, Float)
                 , _rotation :: Float
                 }

data Assets = Assets { _birdDown :: Picture
                     , _birdUp :: Picture
                     , _ripple :: Picture
                     }
                     
type Ripple = (Float, Float)

makeLenses ''BreathState
makeLenses ''GameState
makeLenses ''KeyboardState
makeLenses ''Bird
makeLenses ''Assets

initialState :: Assets -> GameState
initialState assets = GameState (KeyboardState False 1000000) (BreathState 30 Exhale 0 0) viewPortInit (Bird (0,0) 0) assets 0 (-100,0)

type Time = Float

tick :: Float -> GameState -> GameState
tick f = execState $ do
    gameState <- get
    if view (keyState . spacePressed) gameState
      then breath.level += 8*f
      else breath.level -= 8*f
    breath.level %= max 0
    keyState.lastPress += f
    if gameState^.breath.level < exhaleTo - exhaleDiameter/2
       then breath.direction .= Inhale
       else if gameState^.breath.level > inhaleTo + inhaleDiameter/2
               then breath.direction .= Exhale
               else return ()
    time += f

viewTranslate :: Lens' ViewPort (Float, Float)
viewTranslate f (ViewPort t r s) = fmap (\t' -> ViewPort t' r s) (f t)

viewRotate :: Lens' ViewPort Float
viewRotate f (ViewPort t r s) = fmap (\r' -> ViewPort t r' s) (f r)

viewScale :: Lens' ViewPort Float
viewScale f (ViewPort t r s) = fmap (\s' -> ViewPort t r s') (f s)

-- It seems that my space bar sometimes will double-register for some reason... this prevents that from killing your streak.
guardEvent :: Event -> GameState -> GameState
guardEvent event state = if state^.keyState.lastPress < 0.10
                            then state
                            else processInput event state

processInput :: Event -> GameState -> GameState
processInput (EventKey (SpecialKey KeySpace) Down _ _) = execState $ do
    game <- get
    if (inExhaleRing (game ^. breath) && game ^. breath.direction == Exhale) 
      then do breath.currentStreak += 0
              breath.direction .= Inhale
      else when (not (inInhaleRing (game ^.breath))) (breath.currentStreak .= 0)
    keyState.spacePressed .= True
    keyState.lastPress .= 0
processInput (EventKey (SpecialKey KeySpace) Up _ _) = execState $ do 
    b <- fmap _breath get
    if inInhaleRing b && b^.direction == Inhale
       then do breath.currentStreak += 1
               breath.direction .= Exhale
       else when (not (inExhaleRing b)) (breath.currentStreak .= 0)
    keyState.spacePressed .= False
    keyState.lastPress .= 0
processInput _ = id

between :: Ord a => a -> a -> a -> Bool
between target low hi = low < target && target < hi

inInhaleRing :: BreathState -> Bool
inInhaleRing breath = between (breath ^. level) (inhaleTo - inhaleDiameter/2) (inhaleTo + inhaleDiameter/2)

inExhaleRing :: BreathState -> Bool
inExhaleRing breath = between (breath ^. level) (exhaleTo - exhaleDiameter/2) (exhaleTo + exhaleDiameter/2)

render :: GameState -> Picture
render game = applyViewPortToPicture (game^.viewPort) $ mconcat  [ renderRiver game
                                                                 , renderBreath game
                                                                 , renderStreak game
                                                                 , renderBird game
                                                                 , renderRipple game
                                                                 ]

renderStreak :: GameState -> Picture
renderStreak game = translate 350 (-200) $ scale 0.5 0.5 $ text . show $ game ^. breath.currentStreak

renderLastPress :: GameState -> Picture
renderLastPress game = text . show $ game ^. keyState.lastPress

renderRiver :: GameState -> Picture
renderRiver game = mconcat [ translate 0 (-100) (rectangleSolid 800 5)
                           , translate 0 0 (rectangleSolid 800 3)
                           ]

renderBreath :: GameState -> Picture
renderBreath gameState = translate  (-350) (-250) (mconcat  [ renderInhaleRing gameState
                                                            , renderExhaleRing gameState
                                                            , thickCircle (gameState ^. breath.level) 1])

renderInhaleRing :: GameState -> Picture
renderInhaleRing gameState = if gameState^.breath.direction == Inhale then  if inInhaleRing (gameState^.breath)
                                                                              then color inhale (thickCircle inhaleTo inhaleDiameter)
                                                                              else color inhaleTransparent (thickCircle inhaleTo inhaleDiameter) 
                                                                      else mempty

renderExhaleRing :: GameState -> Picture
renderExhaleRing gameState = if gameState^.breath.direction == Exhale then if inExhaleRing (gameState^.breath)
                                                                              then color exhale (thickCircle exhaleTo exhaleDiameter)
                                                                              else color exhaleTransparent (thickCircle exhaleTo exhaleDiameter) 
                                                                      else mempty

renderBird :: GameState -> Picture
renderBird game = game^.assets.birdUp

renderRipple :: GameState -> Picture
renderRipple game = translate x y $ game^.assets.ripple
                      where (x,y) = game^.ripplePos

main :: IO ()
main = do
  Just birdDown <- loadJuicyPNG "boid.png"
  Just birdUp <- loadJuicyPNG "boidup.png"
  Just ripple <- loadJuicyPNG "ripple.png"
  let assets = Assets  (scale 0.2 0.2 $ birdDown) (scale 0.2 0.2 $ birdUp) (scale 0.2 0.2 $ ripple)
  play (InWindow "Relax!" (800, 600) (0,0)) background 60 (initialState assets) render guardEvent tick
