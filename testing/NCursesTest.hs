module Main where

import UI.HSCurses.Curses

-- | Useful for transforming a Char to a ChType. Not sure if this is
-- safe outside of the 7-bit ASCII range.
castEnum = toEnum . fromEnum

-- note that curses positions are (y, x) coordinates, with (0, 0)
-- being the upmost leftmost position
moveAbout pY pX = do
  erase -- clear curses's virtual screen but don't force a redraw
  mvAddCh pY pX (castEnum '@') -- place a character in curses's virtual screen
   refresh -- copy the virtual screen to the terminal
   c <- getCh
     case c of
       KeyUp    -> moveAbout (pY - 1) pX
       KeyDown  -> moveAbout (pY + 1) pX
       KeyLeft  -> moveAbout pY (pX - 1)
       KeyRight -> moveAbout pY (pX + 1)
        _       -> return ()

main = do
  initCurses
  keypad stdScr True -- make the cursor keys usable
  echo False -- disable terminal echo
  cursSet CursorInvisible
  (sizeY, sizeX) <- scrSize
  moveAbout (sizeY `div` 2) (sizeX `div` 2)
  endWin
