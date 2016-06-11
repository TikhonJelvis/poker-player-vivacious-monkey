module Main where

import           Control.Applicative        ((<$>))
import           Data.Aeson                 (Object, eitherDecode')
import           Data.ByteString.Lazy       (append, fromStrict)
import           Data.ByteString.Lazy.Char8 (pack)
import           Network.HTTP.Types         (hContentType, hServer, methodPost,
                                             status200, status400)
import           Network.Wai                (Application, requestMethod,
                                             responseLBS)
import           Network.Wai.Handler.Warp   (run)
import           Network.Wai.Parse          (lbsBackEnd, parseRequestBody)
import           System.Environment         (lookupEnv)

import           Player


main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  putStrLn $ "Listening on port " ++ show port ++ "..."
  run port handler

handler :: Application
handler request respond = if methodPost == requestMethod request
  then do
    (params, _) <- parseRequestBody lbsBackEnd request
    let getParam n v = maybe v id $ lookup n params
        action       = getParam "action" "version"
        state        = parseJSON $ getParam "game_state" "{}" :: Either String Object
        withState f  = either badRequest f state
    case action of
      "check"       -> sayVersion
      "version"     -> sayVersion
      "bet_request" -> withState $ \s -> betRequest s >>= ok . pack . show
      "showdown"    -> withState $ \s -> showdown s >> ok ""
      _             -> badRequest "unknown action"
  else sayVersion
  where
    parseJSON = eitherDecode' . fromStrict
    sayVersion = ok $ pack $ version
    ok = send status200
    badRequest = send status400 . append "Bad request: " . pack
    send status = respond . responseLBS status headers
    headers = [ (hServer, "Haskell Lean Poker Player")
              , (hContentType, "text/plain") ]

