module Main where

import UI.NCurses

data Direction = Up | Down | Left | Right
data Segment = Segment Int Int Direction
data Snake = Snake [Segment]

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
      drawGrid
      drawSnake
        
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')


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
          
drawSnake = do
  moveCursor 2 30
  drawString "O"

  
waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
