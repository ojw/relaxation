{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

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
                           , _camera :: Camera
                           , _bird :: Bird
                           , _assets :: Assets
                           , _time :: Float
                           , _ripplePos :: Ripple
                           , _gen :: StdGen
                           }

data Camera = Camera { _viewPort :: ViewPort
                     , _targetPos :: (Float, Float)
                     , _cameraSpeed :: Float
                     , _followBird :: Bool
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
                 , _flapDuration :: Float
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
makeLenses ''Camera

initialState :: StdGen -> Assets -> GameState
initialState gen assets = GameState (KeyboardState False 1000000) (BreathState 30 Exhale 0 0) initialCamera initialBird assets 0 (-200,-100) gen

initialCamera :: Camera
initialCamera = Camera viewPortInit (0,0) 10 False

initialBird :: Bird
initialBird = Bird (-500,100) 0 (Straight 4 100) (-1)
-- initialBird = Bird (-400, 100) 0 (Curve 1 0.1 1 0 1 200 1)

type Time = Float

randomizeBirdEvent :: State GameState ()
randomizeBirdEvent = do
    g <- fmap _gen $ get
    game <- get
    let (val :: Float, g') = randomR (0,2) g
    case game^.bird.event of
      Straight _ _ -> newCurve g
      Curve _ _ _ _ _ _ _ -> newStraight g
    bird.flapDuration .= 0.2
    camera.followBird .= True
  where
    newStraight g = let (d, g'') = randomR (2, 5) g
                        (s, g''') = randomR (50, 200) g''
                     in do
                        bird.event .= Straight d s
                        gen .= g'''
    newCurve g = let (d, g2) = randomR (1, 3) g
                     (cl, g3) = randomR (0,2) g2
                     (cc, g4) = randomR (1,3) g3
                     (ga, g5) = randomR (-20,200) g4
                     (angs, g6) = randomR (0.5, 3) g5
                     (airs, g7) = randomR (50,200) g6
                     dr = cc
                  in do
                     bird.event .= Curve d cl cc ga angs airs dr
                     gen .= g7

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
        e <- bird.event <%= (duration -~ f)
        when (e^.duration < 0 && abs (gameState^.bird.rotation - (_goalAngle e)) < 100) randomizeBirdEvent
        when (gameState^.bird.rotation > 360) (bird.rotation -= 360)
        when (gameState^.bird.rotation < 0) (bird.rotation += 360)
      Straight d s -> do 
        bird.position._1 += f * s * cos (gameState^.bird.rotation / 180 * pi)
        bird.position._2 += f * s * sin (gameState^.bird.rotation / 180 * pi)
        e <- bird.event <%= (duration -~ f)
        when (e^.duration < 0) randomizeBirdEvent
    time += f
    when (gameState^.camera.followBird) $
         do
           --let dx = gameState^.bird.position._1 + camera.viewPort.viewTranslate._1
             --  dy = gameState^.bird.position._2 + camera.viewPort.viewTranslate._2
           (camera.viewPort.viewTranslate .= ((gameState^.bird.position) & each *~ (-1)))
    bird.flapDuration -= f

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
      else when (not (inInhaleRing (game ^.breath) || fastPress game)) missBreath
    keyState.spacePressed .= True
    keyState.lastPress .= 0
    breath.longestStreak %= max (game^.breath.currentStreak)

processInput (EventKey (SpecialKey KeySpace) Up _ _) game = return . flip execState game $ do 
    b <- fmap _breath get
    game <- get
    if inInhaleRing b && b^.direction == Inhale
       then do breath.currentStreak += 1
               breath.direction .= Exhale
       else when (not (inExhaleRing b || fastPress game)) missBreath
    keyState.spacePressed .= False
    keyState.lastPress .= 0
    breath.longestStreak %= max (game^.breath.currentStreak)
processInput _ game = return game

between :: Ord a => a -> a -> a -> Bool
between target low hi = low < target && target < hi

missBreath :: State GameState ()
missBreath = do game <- get
                breath.currentStreak .= 0
                camera.viewPort.viewTranslate .= (0,0)
                when (game^.camera.followBird == True) (bird .= initialBird)
                camera.followBird .= False

inInhaleRing :: BreathState -> Bool
inInhaleRing breath = between (breath ^. level) (inhaleTo - inhaleDiameter/2) (inhaleTo + inhaleDiameter/2)

inExhaleRing :: BreathState -> Bool
inExhaleRing breath = between (breath ^. level) (exhaleTo - exhaleDiameter/2) (exhaleTo + exhaleDiameter/2)

renderScene :: GameState -> Picture
renderScene game = applyViewPortToPicture (game^.camera.viewPort) $ 
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
                  birdPic
  where
    birdPic = if game^.bird.flapDuration > 0
                then game^.assets.birdDown
                else game^.assets.birdUp

renderRipple :: GameState -> Picture
renderRipple game = translate x y $ game^.assets.ripple
                      where (x,y) = game^.ripplePos

main :: IO ()
main = do
  Just birdDown <- loadJuicyPNG "assets/boiddown.png"
  Just birdUp <- loadJuicyPNG "assets/boidup.png"
  Just ripple <- loadJuicyPNG "assets/ripple.png"
  g <- getStdGen
  let assets = Assets  (scale 0.2 0.2 $ birdDown) (scale 0.2 0.2 $ birdUp) (scale 0.2 0.2 $ ripple)
  playIO (InWindow "Breathe :)" (800, 600) (0,0)) background 60 (initialState g assets) render processInput tick
