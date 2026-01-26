-- | Examples for simple shapes.
module ExampleSignal where

import Signal  (Time, Signal, constS, timeS, applyS, mapT, signal)
import Shape   (Shape, disc, square)
import Animate (animate)
import Render  (defaultWindow)

animateShape :: Signal Shape -> IO ()
animateShape sh = animate defaultWindow 0 endTime sh
  where endTime = 5

-- | It displays a shape on "odd times" and another one in "even times".
alternate :: Shape -> Shape -> Time -> Shape
alternate sh1 sh2 t
  | odd (floor t) = sh1
  | otherwise     = sh2

square_disc :: Signal Shape
square_disc = signal (alternate disc square)


to_zero :: Time -> Time
to_zero = const 0

always_square :: Signal Shape
always_square = mapT to_zero square_disc
