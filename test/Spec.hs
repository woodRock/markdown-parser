import System.Directory
import Data.Maybe
import MarkdownParser

path = "/home/woodj/Documents/markdown-parser/test/markdown/"

-- Files systems use `.` and `..` as file names.
-- They represent the current and parent directory.
isFile :: FilePath -> Bool
isFile f = f /= "." && f /= ".."

-- Get the filenames for each test case.
getFileNames :: IO [FilePath]
getFileNames = do 
    filenames <- 
        filter isFile <$>  -- Remove spurious entries.
        map ((++) path ) <$>  -- Append full path to filename. 
        getDirectoryContents path -- List all files in folder.
    return filenames

-- Attempt to parse a file into Markdown.
runTest :: FilePath -> IO String
runTest testfile = do 
    Just xs <- parseFile testfile file
    return $ show $ xs

main :: IO ()
main = do
    filenames <- getFileNames
    res <- runTest $ filenames !! 0
    print $ res
