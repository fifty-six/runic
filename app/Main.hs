module Main where

import           Control.Applicative            ( (<**>) )
import           Control.Monad                  ( void )
import           Control.Monad.Except           ( guard )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Text.IO                   ( hPutStrLn )
import           Interpreter                    ( run1 )
import           Options.Applicative            ( Parser
                                                , ParserInfo
                                                , command
                                                , customExecParser
                                                , help
                                                , info
                                                , prefs
                                                , progDesc
                                                , showHelpOnError
                                                )
import           Options.Applicative.Builder    ( argument
                                                , fullDesc
                                                , metavar
                                                , str
                                                , subparser
                                                )
import           Options.Applicative.Extra      ( helper )
import           Parser                         ( Decl
                                                , pDecls
                                                )
import           Pretty                         ( render )
import           System.Environment
import           System.IO                      ( stderr )
import           Text.Megaparsec                ( parse )
import           Text.Megaparsec.Error          ( errorBundlePretty )
import           Text.Pretty.Simple             ( pPrint )
import           Typecheck                      ( runSemant
                                                , typecheck
                                                )

data Options = Options
    { path       :: FilePath
    , subcommand :: Command
    }

data Command = Run | Pretty | Typecheck

parseCommand :: Parser Command
parseCommand =
    subparser
        $  command "run" (info (pure Run <**> helper) (fullDesc <> progDesc "Run a program."))
        <> command
               "pretty"
               (info (pure Pretty <**> helper) (fullDesc <> progDesc "Pretty-print a program."))
        <> command
               "typecheck"
               (info (pure Typecheck <**> helper)
                     (fullDesc <> progDesc "Typecheck a program without running it.")
               )

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

parsePath :: Parser FilePath
parsePath = argument str (metavar "PATH" <> help "Input file.")

parseOptions :: Parser Options
parseOptions = flip Options <$> parseCommand <*> parsePath

typecheck' :: [Decl] -> IO Bool
typecheck' decls = do
    case fst $ flip runSemant M.empty $ typecheck decls of
        Left err -> do
            putStrLn . T.unpack . render $ err
            pure False
        Right sdecls -> pure True

run :: [Decl] -> IO ()
run decls = do
    typecheck' decls >>= guard

    ran <- run1 decls
    either pPrint pPrint ran

pretty :: [Decl] -> IO ()
pretty = putStrLn . T.unpack . render

main :: IO ()
main = do
    args    <- getArgs
    options <- showHelpOnErrorExecParser $ info (helper <*> parseOptions) mempty

    let sub = subcommand options

    contents <- T.pack <$> readFile (path options)

    case parse pDecls (path options) contents of
        Left  parseE -> hPutStrLn stderr . T.pack $ errorBundlePretty parseE
        Right decls  -> ($ decls) $ case sub of
            Run       -> run
            Pretty    -> pretty
            Typecheck -> void <$> typecheck'
