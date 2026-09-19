module Main.Syntax.Lexer.Parsing.LexerIO
  ( lexingIO,
    lexingText,
  )
where

import Control.Monad (Monad (return))
import Control.Monad.Except (ExceptT, mapExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (Either (Left, Right))
import Data.Function (($), (.))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Text (Text, pack, unpack)
import Data.Text.IO as DTIO (putStrLn, readFile)
import Main.Syntax.Lexer.LexerModel (LexerModel)
import Main.Syntax.Lexer.Parsing.Lexer (lexing)
import Shared.Conditional.Debug.Trace (trace)
import Shared.Errors (Error (Error), Errors (Errors, errors))
import Shared.Lexer.Data (Lexer)
import System.Directory (doesFileExist)
import System.IO (IO)
import Text.Megaparsec as MP (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Shakespeare.Text (st)

lexingText :: Text -> Text -> Lexer (p, Text) -> ExceptT Errors Identity (p, Text)
lexingText filePath fileBody lexer = case parse lexer (unpack filePath) fileBody of
  Left bundle -> throwError $ Errors {errors = [Error $ pack $ errorBundlePretty bundle]}
  Right res -> return res

lexingFile :: Text -> Text -> Lexer ([p], Text) -> ExceptT Errors IO ([p], Text)
lexingFile message filePath lexer = do
  let filePathT = unpack filePath
  liftIO $ DTIO.putStrLn [st|#{message}|]
  fileExists <- liftIO $ doesFileExist filePathT
  if fileExists
    then do
      fileBody <- liftIO $ readFile filePathT
      trace [st|source: #{fileBody}|] $ mapExceptT (return . runIdentity) $ lexingText filePath fileBody lexer
    else do
      liftIO $ DTIO.putStrLn [st|File '#{filePath}' was not found|]
      return ([], "")

lexingIO :: Text -> Text -> ExceptT Errors IO (LexerModel, Text)
lexingIO filePath fileURL =
  lexingFile "Lexing" filePath (lexing fileURL)
