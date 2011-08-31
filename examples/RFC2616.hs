{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Applicative
import Control.Exception (bracket)
import System.Environment (getArgs)
import System.IO (hClose, openFile, IOMode(ReadMode))
import Text.Trifecta.Parser.Class hiding (satisfy)
import Text.Trifecta.Parser.Token
import Text.Trifecta.Parser.ByteString
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Parser.Char8
import Text.Trifecta.Highlight.Prim
import qualified Text.Trifecta.ByteSet as S
import qualified Data.ByteString as B

infixl 4 <$!>

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
  a <- ma
  return $! f a

token :: MonadParser m => m Char
token = noneOf $ ['\0'..'\31'] ++ "()<>@,;:\\\"/[]?={} \t" ++ ['\128'..'\255']

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'

skipHSpaces :: MonadParser m => m ()
skipHSpaces = skipSome (satisfy isHorizontalSpace)

data Request = Request {
      requestMethod   :: String
    , requestUri      :: String
    , requestProtocol :: String
    } deriving (Eq, Ord, Show)

requestLine :: MonadParser m => m Request
requestLine = Request <$!> (highlight ReservedIdentifier (some token) <?> "request method")
                       <*  skipHSpaces 
                       <*> (highlight Identifier (some (satisfy (not . isHorizontalSpace))) <?> "url")
                       <*  skipHSpaces 
                       <*> try (highlight ReservedIdentifier (string "HTTP/" *> many httpVersion <* endOfLine) <?> "protocol")
 where
  httpVersion = satisfy $ \c -> c == '1' || c == '0' || c == '.'

endOfLine :: MonadParser m => m ()
endOfLine = (string "\r\n" *> pure ()) <|> (char '\n' *> pure ())

data Header = Header {
      headerName  :: String
    , headerValue :: [String]
    } deriving (Eq, Ord, Show)

messageHeader :: MonadParser m => m Header
messageHeader = (\h b c -> Header h (b : c)) 
            <$!> (highlight ReservedIdentifier (some token)  <?> "header name")
             <*  highlight Operator (char ':') <* skipHSpaces
             <*> (highlight Identifier (manyTill anyChar endOfLine) <?> "header value")
             <*> (many (skipHSpaces *> manyTill anyChar endOfLine) <?> "blank line")

request :: MonadParser m => m (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine

lumpy arg = do
  r <- parseFromFile (many request) arg
  case r of
    Nothing -> return ()
    Just rs -> print (length rs)

{-
chunky arg = bracket (openFile arg ReadMode) hClose $ \h ->
               loop 0 =<< B.hGetContents h
 where
  loop !n bs
      | B.null bs = print n
      | otherwise = case parse myReq arg bs of
                      Left err      -> putStrLn $ arg ++ ": " ++ show err
                      Right (r,bs') -> loop (n+1) bs'
  myReq :: Parser ((Request, [Header]), B.ByteString)
  myReq = liftA2 (,) request getInput
-}

main :: IO ()
main = mapM_ f =<< getArgs
  where
    f = lumpy
    -- f = chunky
