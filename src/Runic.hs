module Runic
    (
        main
    ) where

import           Control.Applicative            ( (<**>) )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Text.IO                   ( hPutStrLn )
import           Runic.Interpreter              ( run1 )
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
import           Runic.Parser                   ( Decl
                                                , pDecls
                                                )
import           Runic.Pretty                   ( render, Pretty )
import           System.Environment
import           System.IO                      ( stderr, IOMode (WriteMode), openFile, withFile )
import           Text.Megaparsec                ( parse )
import           Text.Megaparsec.Error          ( errorBundlePretty )
import           Text.Pretty.Simple             ( pPrint )
import           Runic.Typecheck                ( runSemant
                                                , typecheck, SemantError, SDecl
                                                )
import Runic.Codegen (codegenProg)
import LLVM.Pretty (ppllvm)
import qualified GHC.IO.Handle.Text as T
import Data.String.Conversions (cs)
import GHC.IO.Handle (hClose)
import Prelude hiding (mod)
import Control.Exception (bracket)
import System.Directory (removePathForcibly)
import GHC.Clock (getMonotonicTime)
import System.Process (callProcess)

data Options = Options
    { path       :: FilePath
    , subcommand :: Command
    }

data Command = Run | Pretty | Typecheck | Compile | IR

parseCommand :: Parser Command
parseCommand =
    subparser
        $  command "run" (info (pure Run <**> helper) (fullDesc <> progDesc "Run a program."))
        <> command "ir" (info (pure IR <**> helper) (fullDesc <> progDesc "Compile a program to LLVM IR"))
        <> command "compile" (info (pure Compile <**> helper) (fullDesc <> progDesc "Compile a program to an executable."))
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

typecheck' :: [Decl] -> Either SemantError [SDecl]
typecheck' decls = fst $ flip runSemant M.empty $ typecheck decls

runTypecheck :: [Decl] -> IO ()
runTypecheck decls = case typecheck' decls of
    Left err -> perr err
    Right _ -> pure ()

perr :: Pretty a => a -> IO ()
perr = putStrLn . cs . render

run :: [Decl] -> IO ()
run decls = do
    case typecheck' decls of
        Left err -> perr err
        Right _ -> pure ()

    ran <- run1 decls
    either pPrint pPrint ran

ir :: [Decl] -> IO ()
ir decls = do
    case typecheck' decls of
        Left err ->
            putStrLn . T.unpack . render $ err
        Right sdecls -> do
            handle <- openFile "./out.ll" WriteMode

            let mod = codegenProg sdecls
            T.hPutStrLn handle . cs . ppllvm $ mod

            hClose handle

            pure ()

compile :: [Decl] -> IO ()
compile decls = do
    case typecheck' decls of
        Left err -> putStrLn . T.unpack . render $ err
        Right sDecls -> do
            time <- getMonotonicTime

            -- generate a semi-unique name
            let name = "./out__" <> show time <> ".ll"

            bracket (pure name) removePathForcibly $ \p -> do
                withFile p WriteMode $ \handle -> do
                    T.hPutStrLn handle . cs . ppllvm $ codegenProg sDecls

                callProcess "clang" ["-Wno-override-module", "-lm", p, "-o", "a.out"]


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
            Compile   -> compile
            IR        -> ir
            Run       -> run
            Pretty    -> pretty
            Typecheck -> runTypecheck
