{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Juicy

import System.Random
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

data BirdEvent = Curve { _duration :: Float
                       , _curvatureLinear :: Float
                       , _curvatureConstant :: Float
                       , _goalAngle :: Float
                       , _angularSpeed :: Float
                       , _airSpeed :: Float
                       , _deltaR :: Float
                       }
               | Straight { _duration :: Float
                          , _speed :: Float
                          }

data Bird = Bird { _position :: (Float, Float)
                 , _rotation :: Float
                 , _event :: BirdEvent
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
makeLenses ''BirdEvent
makePrisms ''BirdEvent            

initialState :: Assets -> GameState
initialState assets = GameState (KeyboardState False 1000000) (BreathState 30 Exhale 0 0) viewPortInit initialBird assets 0 (-200,-100)

initialBird :: Bird
--initialBird = Bird (-400,100) 0 (Straight 10 50)
initialBird = Bird (-400, 100) 0 (Curve 1 0.1 1 0 1 200 1)

type Time = Float

tick :: Float -> GameState -> IO GameState
tick f game = return $ flip execState game $ do
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
    case gameState^.bird.event of
      Curve dur lin con goal angspd airspd dr -> do
        bird.event.deltaR += lin * f
        bird.rotation += (dr + lin * f) * angspd
        bird.position._1 += f * airspd * cos (gameState^.bird.rotation / 180 * pi)
        bird.position._2 += f * airspd * sin (gameState^.bird.rotation / 180 * pi)
        bird.event %= (duration -~ f)
      Straight d s -> do 
        bird.position._1 += f * s * cos (gameState^.bird.rotation / 180 * pi)
        bird.position._2 += f * s * sin (gameState^.bird.rotation / 180 * pi)
        bird.event %= (duration -~ f)
    time += f
    viewPort.viewTranslate .= ((gameState^.bird.position) & each *~ (-1))

viewTranslate :: Lens' ViewPort (Float, Float)
viewTranslate f (ViewPort t r s) = fmap (\t' -> ViewPort t' r s) (f t)

viewRotate :: Lens' ViewPort Float
viewRotate f (ViewPort t r s) = fmap (\r' -> ViewPort t r' s) (f r)

viewScale :: Lens' ViewPort Float
viewScale f (ViewPort t r s) = fmap (\s' -> ViewPort t r s') (f s)

-- It seems that my space bar sometimes will double-register for some reason... this prevents that from killing your streak.
fastPress :: GameState -> Bool
fastPress game = game^.keyState.lastPress < 0.10

processInput :: Event -> GameState -> IO GameState
processInput (EventKey (SpecialKey KeySpace) Down _ _) game = return . flip execState game $ do
    game <- get
    if (inExhaleRing (game ^. breath) && game ^. breath.direction == Exhale) 
      then do breath.currentStreak += 0
              breath.direction .= Inhale
      else when (not (inInhaleRing (game ^.breath) || fastPress game)) (breath.currentStreak .= 0)
    keyState.spacePressed .= True
    keyState.lastPress .= 0
    breath.longestStreak %= max (game^.breath.currentStreak)

processInput (EventKey (SpecialKey KeySpace) Up _ _) game = return . flip execState game $ do 
    b <- fmap _breath get
    game <- get
    if inInhaleRing b && b^.direction == Inhale
       then do breath.currentStreak += 1
               breath.direction .= Exhale
       else when (not (inExhaleRing b || fastPress game)) (breath.currentStreak .= 0)
    keyState.spacePressed .= False
    keyState.lastPress .= 0
    breath.longestStreak %= max (game^.breath.currentStreak)
processInput _ game = return game

between :: Ord a => a -> a -> a -> Bool
between target low hi = low < target && target < hi

inInhaleRing :: BreathState -> Bool
inInhaleRing breath = between (breath ^. level) (inhaleTo - inhaleDiameter/2) (inhaleTo + inhaleDiameter/2)

inExhaleRing :: BreathState -> Bool
inExhaleRing breath = between (breath ^. level) (exhaleTo - exhaleDiameter/2) (exhaleTo + exhaleDiameter/2)

renderScene :: GameState -> Picture
renderScene game = applyViewPortToPicture (game^.viewPort) $ 
                   mconcat  [ renderRiver game
                            , renderBreath game
                            , renderBird game
                            --, renderRipple game
                            ]

renderUI :: GameState -> Picture
renderUI game = renderStreak game
                             
render :: GameState -> IO Picture
render game = return $ pictures [ renderScene game, renderUI game]

renderStreak :: GameState -> Picture
renderStreak game = translate 350 (-200) $ scale 0.5 0.5 $ text . show $ game ^. breath.currentStreak

renderLastPress :: GameState -> Picture
renderLastPress game = text . show $ game ^. keyState.lastPress

renderRiver :: GameState -> Picture
renderRiver game = mconcat [ translate 0 (-100) (rectangleSolid 80000 5)
                           , translate 0 0 (rectangleSolid 80000 3)
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
renderBird game = uncurry translate (game^.bird.position) $ 
                  rotate ((-1) * game^.bird.rotation) $
                  game^.assets.birdUp

renderRipple :: GameState -> Picture
renderRipple game = translate x y $ game^.assets.ripple
                      where (x,y) = game^.ripplePos

main :: IO ()
main = do
  Just birdDown <- loadJuicyPNG "boiddown.png"
  Just birdUp <- loadJuicyPNG "boidup.png"
  Just ripple <- loadJuicyPNG "ripple.png"
  g <- getStdGen
  let assets = Assets  (scale 0.2 0.2 $ birdDown) (scale 0.2 0.2 $ birdUp) (scale 0.2 0.2 $ ripple)
  playIO (InWindow "Relax!" (800, 600) (0,0)) background 60 (initialState assets) render processInput tick
