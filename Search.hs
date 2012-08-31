
module Search where

import FileOperations
import Data.List (isInfixOf)
import Data.Char (toLower)

search :: FilePath -> String -> IO([SearchResult])
search folder searchString = 
            do files <- getSearchFiles folder
               return $ searchFiles files searchString


searchFiles :: [SearchFile] -> String -> [SearchResult]
searchFiles files searchString = filterAndOrder $ map (lookUp searchString) files

filterAndOrder :: [SearchResult] -> [SearchResult]
filterAndOrder srs = titles ++ mixeds ++ bodys
      where (titles, mixeds, bodys, _) = foldr addToFitting ([],[],[],[]) srs

type SearchResultSet = ([SearchResult], [SearchResult], [SearchResult], [SearchResult])

addToFitting :: SearchResult -> SearchResultSet -> SearchResultSet
addToFitting x@(NoResult    _) (t,m,b,n) = (t  ,m  ,b  ,x:n)
addToFitting x@(TitleResult _) (t,m,b,n) = (x:t,m  ,b  ,n  )
addToFitting x@(MixedResult _) (t,m,b,n) = (t  ,x:m,b  ,n  )
addToFitting x@(BodyResult  _) (t,m,b,n) = (t  ,m  ,x:b,n  )

data SearchResult = NoResult SearchFile
                  | TitleResult SearchFile
                  | BodyResult SearchFile
                  | MixedResult SearchFile
                  deriving Show

getSearchFile :: SearchResult -> SearchFile
getSearchFile (NoResult sf)    = sf
getSearchFile (TitleResult sf) = sf
getSearchFile (BodyResult sf)  = sf
getSearchFile (MixedResult sf) = sf

getSearchResultFile = getSearchFileName . getSearchFile
getSearchResultContent = getSearchFileContent . getSearchFile

lookUp :: String -> SearchFile  -> SearchResult
lookUp s sf@(SearchFile n c) | isTitleResult = TitleResult sf
                             | isBodyResult  = BodyResult sf
                             | isMixedResult = MixedResult sf
                             | otherwise     = NoResult sf
  where ws                   = words $ map toLower s
        internalSearch space = map (`isInfixOf` space) ws
        titleResults         = internalSearch $ map toLower n
        bodyResults          = internalSearch $ map toLower c
        mixedResults         = zipWith (||) titleResults bodyResults
        isTitleResult        = and titleResults
        isBodyResult         = and bodyResults && not (or titleResults)
        isMixedResult        = and mixedResults

