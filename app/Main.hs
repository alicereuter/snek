module Main where

import UI.NCurses
import Control.Concurrent
import Control.Monad.IO.Class


data Direction = Up | Down | Left | Right
data Segment = Segment Integer Integer Direction
data Snake = Snake [Segment]


start = Snake [Segment 5 12 Up, Segment 6 12 Up]

next (Snake s) = Snake $ map (\(Segment x y d) -> Segment (x+1) y d) s

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    loop w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q') start

loop w p s = do

  ev <- getEvent w Nothing
  updateWindow w $ do
      clear
      drawGrid
      drawSnake start

  render
  liftIO $ threadDelay 1000000
  case ev of
    Nothing -> loop w p s
    Just ev' -> if p ev' then loop w p $ next s else loop w p s


drawGrid = do
  moveCursor 1 10
  drawString "#################################################"
  mapM (\x -> drawSide x) [1..20]
  moveCursor 20 10
  drawString "#################################################"
  moveCursor 0 0
  where drawSide x = do
          moveCursor x 10
          drawString "#"
          moveCursor x 59
          drawString "#"
          
drawSnake (Snake segs) = do
  drawSeg "O" $ head segs
  mapM (drawSeg "o") $ tail segs
  pure ()
  where drawSeg c (Segment x y _) = do
          moveCursor x y
          drawString c

