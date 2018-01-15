module Main where

import qualified RFC2616

import           Control.Monad.IO.Class (liftIO)
import           Test.Hspec
import           Text.Trifecta

-- Just [(Request {requestMethod = "GET", requestUri = "http://slashdot.org/", requestProtocol = "1.1"},[Header {headerName = "foo", headerValue = ["this is a test"]}]),(Request {requestMethod = "GET", requestUri = "http://slashdot.org/", requestProtocol = "1.0"},[Header {headerName = "foo", headerValue = ["of the emergency broadcast system"]}])]

main :: IO ()
main = hspec $ do
  describe "RFC2616.hs should be able to parse RFC2616" $ do
    it "parses the RFC2616.txt file successfully" $ do
      -- result :: Maybe [(RFC2616.Request, [RFC2616.Header])]
      -- Tests are intended to be run from the top level.
      result <- liftIO $ parseFromFile RFC2616.requests "RFC2616.txt"
      print result
      result `shouldNotBe` Nothing
