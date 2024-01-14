-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Graphics.Gloss

import Turtle

data Model = Model
    { current :: Picture
    , updates :: [Picture]
    }

runGraphical :: Program -> IO ()
runGraphical _ = simulate
    (InWindow "Turtle!" (300, 300) (10, 10))
    black
    1
    init
    draw
    evolve
  where
    init = Model { current = blank
                 , updates = map (translate (-150) (-150))
                                 [ color white $ polygon [(0, 0), (0, 300), (300, 300), (300, 0)]
                                 , color red   $ line [(100, 200), (200, 100)]
                                 , color green $ line [(100, 100), (200, 200)]
                                 , color blue  $ line [(150, 100), (150, 200)]
                                 , color black $ line [(100, 150), (200, 150)]
                                 ]
                 }
    draw       = fst . onTick
    evolve _ _ = snd . onTick

onTick :: Model -> (Picture, Model) -- (draw, evolve)
onTick m@(Model c [])     = (c, m)
onTick   (Model c (u:us)) = let c' = pictures [c, u] in (c', Model c' us)
