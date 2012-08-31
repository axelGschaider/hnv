
import Search
import FileOperations

nvData = "/home/axel/Dropbox/nvd"

searchTest = search nvData

justTitles x= 
  do results <- searchTest x
     mapM putStrLn $ map getSearchFileName $ map getSearchFile results

main = getLine >>= justTitles

