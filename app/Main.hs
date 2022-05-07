module Main where

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( guard )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( hPutStrLn )
import           Interpreter                    ( run1 )
import           Parser                         ( pDecls )
import           System.Environment
import           System.IO                      ( stderr )
import           Text.Megaparsec                ( parse )
import           Text.Megaparsec.Error          ( errorBundlePretty )
import           Text.Pretty.Simple             ( pPrint )

main :: IO ()
main = do
    args <- getArgs

    when (length args /= 1) $ do
        hPutStrLn stderr "Usage: [exe] file"
        guard False

    let (x : _) = args

    contents <- readFile x

    case parse pDecls x $ T.pack contents of
        Left  parseE -> hPutStrLn stderr . T.pack $ errorBundlePretty parseE
        Right decls  -> do
            -- pPrint decls
            ran <- run1 decls

            either pPrint pPrint ran
