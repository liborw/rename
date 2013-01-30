-- Rename multiple files using editor.
--
module Main where

import System.Cmd
import System.Environment
import System.Directory
import Data.Maybe

main :: IO ()
main = do
    files <- getArgs
    files' <- edit ".RENAME_TMP" (unlines files)
    mapM_ (uncurry renameFile) $ zip files (lines files')
    return ()

edit :: FilePath -> String -> IO String
edit f s = do
    editor <- getEditorCmd
    writeFile f s
    _ <- rawSystem editor [f]
    content <- readFile f
    removeFile f
    return content

lookupEnv :: String -> IO (Maybe String)
lookupEnv s = do
    env <- getEnvironment
    return $ lookup s env

getEditorCmd :: IO String
getEditorCmd = do
    resources <- sequence [lookupEnv "EDITOR", lookupEnv "VISUAL"]
    return $ firstJust "vim" resources

renameFiles :: [(FilePath, FilePath)] -> IO ()
renameFiles =  mapM_ (uncurry renameFile)

firstJust :: a -> [Maybe a] -> a
firstJust a [] = a
firstJust a (x:xs) = fromMaybe (firstJust a xs) x


