
module FileOperations where

import System.Directory
import System.FilePath.Posix
import System.Posix.Files
import Data.List
import Control.Monad

type PlainFileName = String
type FileContent = String

data SearchFile = SearchFile PlainFileName FileContent deriving Show

getSearchFileName (SearchFile fn _) = fn
getSearchFileContent (SearchFile _ co) = co

getAbsDirectoryContentsSave :: FilePath -> IO [FilePath]
getAbsDirectoryContentsSave dir = fileExist dir >>= getSave dir

getSave :: FilePath -> Bool -> IO [FilePath]
getSave dir False = return []
getSave dir _     = getAbsDirectoryContents dir

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir = 
  getDirectoryContents dir >>= mapM (canonicalizePath . (dir </>))

isValidFile filePath = fileExist filePath >>= isGoodFile filePath

isGoodFile :: FilePath -> Bool -> IO(Bool)
isGoodFile a False = return False
isGoodFile a _     = do isNoDir <- isNoDirectory
                        return $ isNoDir && isTxt && isNotHidden
        where plainFileName = reverse $ takeWhile (/= '/') $ reverse a
              isNotHidden   = '.' /= head plainFileName
              isTxt         = ".txt" `isSuffixOf` plainFileName
              fileStatus    = getFileStatus a
              isNoDirectory = fmap ( not . isDirectory ) fileStatus

cleanFileName :: FilePath -> String
cleanFileName = 
    reverse . takeWhile ( /= '/') . drop 1 . dropWhile (/= '.') . reverse

getFullFile :: FilePath -> IO SearchFile
getFullFile path = do cont <- readFile path
                      return $ SearchFile (cleanFileName path) cont


getSearchFiles :: FilePath -> IO [SearchFile]
getSearchFiles path = 
  getAbsDirectoryContentsSave path >>= filterM isValidFile >>= mapM getFullFile




