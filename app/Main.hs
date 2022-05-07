module Main where

import Text.Pretty.Simple (pPrint)
import Text.Megaparsec (parse)
import Interpreter (run1)
import Parser (pDecls)
import System.Environment
import qualified Data.Text as T
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad (when)
import Data.Text.IO (hPutStrLn)
import System.IO (stderr)
import Control.Monad.Except (guard)

main :: IO ()
main = do
    args <- getArgs

    when (length args /= 1) $ do
        hPutStrLn stderr "Usage: [exe] file"
        guard False

    let (x:_) = args

    contents <- readFile x

    case parse pDecls x $ T.pack contents of 
        Left parseE -> 
            hPutStrLn stderr . T.pack $ errorBundlePretty parseE
        Right decls -> do
            -- pPrint decls
            ran <- run1 decls

            either pPrint pPrint ran
