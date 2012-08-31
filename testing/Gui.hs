import qualified UI.HSCurses.CursesHelper as CursesH
import qualified UI.HSCurses.Curses as Curses
import UI.HSCurses.Widgets
import UI.HSCurses.Logging


screenSize = 
    do a <- initScr
       size <- scrSize
       putStrLn $ show size
       endWin

mkMainWidget =
    do tlw <- mkToplineWidget

mkToplineWidget =
    do opts <- lineOptions
       return $ newTextWidget (opts { twopt_haligne = AlignCenter })
                              title

