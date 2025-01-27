-- | Animation examples.
module Example where

import Control.Applicative

import Signal  (Signal, constS, applyS, mapS, timeS, signal)
import Shape   (Shape, disc, square, difference,
                scale, translate, vec, angle, rotate)
import Animate (animate)
import Render  (defaultWindow)

-- | A rotating square
rotatingSquare :: Signal Shape
rotatingSquare = signal (rotate . angle) `applyS` constS square
              -- Using the Control.Applicative interface:
-- rotatingSquare = signal (rotate . angle) <*> pure square

-- | A stranger drop-in replacement for rotate in rotatingSquare
transmogrify :: Double -> Shape -> Shape
transmogrify d s = scale (vec (sin d) (cos d)) (rotate (angle d) s)

-- | A bouncing ball
bouncingBall :: Signal Shape
-- bouncingBall = constS translate `applyS` pos `applyS` constS ball
bouncingBall = translate <$> pos <*> pure ball
  where
    ball = scale (vec 0.5 0.5) disc
    pos  = constS vec `applyS` bounceX `applyS` bounceY
    bounceY = mapS ((0.8*) . sin . (3*)) timeS
--    bounceX = constS 0
    bounceX = mapS ((0.8*) . sin . (2*)) timeS
--    bounceX = mapS (0.3*) bounceY

-- | Combining the two
example :: Signal Shape
--example = constS difference `applyS` rotatingSquare `applyS` bouncingBall
example = difference <$> rotatingSquare <*> bouncingBall

{-
-- Illustrate type error and finding the solution
example2 = difference <$> one <*> two
    where one :: Signal Shape
          one = example
          two :: Signal Shape
          two = scale (vec (-1) (0.5)) one
-}

runExample :: IO ()
runExample = animate defaultWindow 0 endTime example
  where endTime = 15

-- main = runExample
