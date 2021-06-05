{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Search   as S
import qualified Data.Yaml                     as Y
import qualified Data.Hashable                 as Hashable
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Text.IO                  as TIO
                                                ( putStrLn )
import           Data.Text
import           Data.Time.Clock.System         ( getSystemTime, systemSeconds )
import           Data.Default                   ( def )
import           Data.Either
import           Text.Pandoc
import           Data.Aeson
import           Control.Monad.Identity         ( Identity
                                                , runIdentity
                                                , forM_
                                                , void
                                                )
import           Text.Ginger
import           Text.Ginger.Html               ( htmlSource )
import           Text.Blaze
import qualified Data.Vector                   as Vector
import           System.Directory
import qualified Control.Monad.Extra           as Extra
import           Network.AWS.Auth
import qualified Network.AWS.Data              as Data
import           Network.AWS.S3
import           Network.AWS.CloudFront
import           Network.AWS.CloudFront.Types
import           Network.AWS.Env
import           Network.AWS.Types
import           Control.Lens.Lens
import           Control.Lens
import           System.Log.FastLogger
import           System.IO
import           System.FilePath
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.AWS
import           Control.Monad.IO.Class

data Content = FrontMatterContent { fmmetadata :: HashMap Text Y.Value , lit :: Text }

gingerResolver :: FilePath -> IO (Maybe String)
gingerResolver source = L.readFile source >>= return . Just . unpack . toText

template :: FilePath -> IO (Either ParserError (Text.Ginger.Template SourcePos))
template = parseGingerFile gingerResolver

scopeLookup :: (Hashable.Hashable k, Eq k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context = toGVal $ HashMap.lookup key context

render :: Text.Ginger.Template SourcePos -> HashMap VarName Value -> Text
render template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context = makeContextHtml contextLookup
  in htmlSource $ runGinger context template

loadSource :: FilePath -> IO Value
loadSource path = do
    event <- parseContent <$> L.readFile path
    html <- HashMap.fromList <$>
              either 
                (const [])
                (\x -> [("html", String x), ("url", String . pack $ pathToRealUrlPath path (Object $ fmmetadata event))])
              <$> toHtml event
    pure . Object $ fmmetadata event <> html

loadSourcesFrom :: FilePath -> IO [(FilePath, Value)]
loadSourcesFrom location = do
    events <- listDirectory location
    Extra.concatMapM (\x -> (\a -> [(path x, a)]) <$> (loadSource $ path x)) events
  where
    path x = location <> "/" <> x

toHtml :: Content -> IO (Either PandocError Text)
toHtml (FrontMatterContent _ lit) = runIO $ do
    doc <- readRST def lit
    writeHtml5String def doc

toText :: L.ByteString -> Text
toText = decodeUtf8 . L.toStrict

parseContent :: L.ByteString -> Content
parseContent content = maybe (FrontMatterContent HashMap.empty $ toText content) id $ do
                                rest <- L.stripPrefix "---" content
                                let (yaml', content') = S.breakOn "---" rest
                                body <- L.stripPrefix "---" content'
                                pure $ parseFrontMatterContent yaml' body

parseFrontMatterContent :: L.ByteString -> L.ByteString -> Content
parseFrontMatterContent yaml lit = FrontMatterContent yaml' (toText lit)
    where
        yaml' = either (const HashMap.empty) id $ Y.decodeEither' (L.toStrict yaml)

pathToLocalUrlPath :: String -> Value -> String
pathToLocalUrlPath a md = "html/" <> a -<.> "html"

pathToRealUrlPath :: String -> Value -> String
pathToRealUrlPath a md = a -<.> "html"

main :: IO ()
main = do
  eventItems    <- loadSourcesFrom "events"
  resourceItems <- loadSourcesFrom "resources"

  let context = HashMap.fromList
        [ ("events"   , Array $ Vector.fromList $ Prelude.map snd eventItems)
        , ("resources", Array $ Vector.fromList $ Prelude.map snd resourceItems)
        ]

  indexFile <- template "templates/index.ginger"
  either
    (\error -> do
      TIO.putStrLn "There was some error loading the template for index.ginger:"
      print error
      Prelude.putStrLn $ peErrorMessage error
    )
    (\x -> L.writeFile "html/index.html" . L.fromStrict . encodeUtf8 $ render x context)
    indexFile

  eventsTemplate <- template "templates/event.ginger"
  either
    (const $ pure ())
    (\x -> do
      createDirectoryIfMissing True "html/events"
      forM_ eventItems $ \((path, obj)) -> do
        L.writeFile
          (pathToLocalUrlPath path obj) 
          ( L.fromStrict . encodeUtf8 . (render x) $
              HashMap.fromList [ ("event", obj)
                               , ("indexUrl", "/index.html")
                               ]
          )
    )
    eventsTemplate

  env <- newEnv Discover <&> set envRegion NorthVirginia
  systemTime <- systemSeconds <$> getSystemTime

  let
    invalidationId = "invalidation " <> (pack . show $ systemTime)
    distributionId = "E1N5IONLD4WMAF"
    b = "auslansocial.com"
    html = "text/html"
    css = "text/css"
    files :: [ (String, String, Text) ]
    files = [ ("html/index.html", "index.html", html)
            , ("html/main.css", "main.css", css)
            ] <> Prelude.map (\((a, b)) -> (pathToLocalUrlPath a b, pathToRealUrlPath a b, html)) eventItems
    c = ChunkSize 1024*1024
    say = liftIO . TIO.putStrLn
    batch = paths ( Prelude.length files )
              & pItems .~ (Prelude.map (\((_, a, _)) -> "/" <> Data.toText a) files)
    invalidations = invalidationBatch batch invalidationId

  runResourceT . runAWST env $ do
    forM_ files $ \((f, k, m)) -> do
      bdy <- chunkedFile c f
      void . send $ (putObject b (ObjectKey $ pack k) bdy) & poContentType .~ Just m
      say $ "Successfully Uploaded: "
         <> Data.toText f <> " to " <> Data.toText b <> " - " <> Data.toText k
    send $ createInvalidation distributionId invalidations

  pure ()

