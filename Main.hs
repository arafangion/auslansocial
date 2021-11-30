{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Search   as S
import qualified Data.Yaml                     as Y
import qualified Data.Hashable                 as Hashable
import           Data.HashMap.Strict            ( HashMap, (!?), (!) )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Text.IO                  as TIO
                                                ( putStrLn )
import           Data.Text                     hiding (elem)
import           Data.Time.Clock.System         ( getSystemTime, systemSeconds )
import           Data.Default                   ( def )
import           Data.Either
import           Data.Time                     hiding (dayOfWeek)
import           Text.Pandoc
import           Data.Aeson
import           Data.Aeson.KeyMap             as KeyMap
import           Control.Monad.Identity         ( Identity
                                                , runIdentity
                                                , forM_
                                                , void
                                                )
import           Text.Ginger
import           Text.Ginger.Html               ( htmlSource )
import           Text.Blaze                    hiding ((!?), (!))
import           Text.Inflections              ( ordinalize )
import qualified Data.Vector                   as Vector
import           System.Directory
import qualified Control.Monad.Extra           as Extra
import           Control.Lens.Lens
import           Control.Lens
import           Data.Generics.Product        (field)
import           System.Log.FastLogger
import           System.IO
import           System.FilePath
import           System.Environment
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Control.Monad.Error.Class

import          Amazonka
import          Amazonka.S3
import          Amazonka.Env
import          Amazonka.Discovery
import          Amazonka.Data.Text (ToText(..))
import          Amazonka.CloudFront.Types         (paths, newInvalidationBatch, newPaths)
import          Amazonka.CloudFront         (newCreateInvalidation)

data Content = FrontMatterContent { fmmetadata :: HashMap Text Y.Value , lit :: Text }


gingerResolver :: FilePath -> IO (Maybe String)
gingerResolver source = L.readFile source >>= return . Just . unpack . toText . L.toStrict

template :: FilePath -> IO (Either ParserError (Text.Ginger.Template SourcePos))
template = parseGingerFile gingerResolver

scopeLookup :: (Hashable.Hashable k, Eq k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context = toGVal $ HashMap.lookup key context

render :: Text.Ginger.Template SourcePos -> HashMap VarName Value -> Text
render template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context = makeContextHtml contextLookup
  in htmlSource $ runGinger context template

fromValue :: Maybe Value -> Text
fromValue v = case v of
                Just (String v) -> v
                otherwise -> "(Missing)"

eventName :: HashMap Text Y.Value -> Text
eventName md = fromValue $ md !? "name"
address :: HashMap Text Y.Value -> Text
address md = fromValue $ md !? "address"
dayNameOfWeek :: Maybe LocalTime -> Text
dayNameOfWeek d =
    maybe "Missing" (\x -> pack $ formatTime defaultTimeLocale "%A" x) d
monthName :: Maybe LocalTime -> Text
monthName d =
    maybe "Missing" (\x -> pack $ formatTime defaultTimeLocale "%B" x) d
dayOrdOfWeek :: Maybe LocalTime -> Text
dayOrdOfWeek Nothing = ""
dayOrdOfWeek (Just d) = 
  let
    getOrdDay (year, month, day) = ordinalize day
  in
    getOrdDay $ toGregorian (localDay d)
dayOfWeek :: Maybe LocalTime -> Text
dayOfWeek Nothing = ""
dayOfWeek (Just d) = 
  let
    getOrdDay (year, month, day) = pack $ show day
  in
    getOrdDay $ toGregorian (localDay d)


loadSource :: FilePath -> IO Value
loadSource path = do
    event <- parseContent <$> L.readFile path
    let md = fmmetadata event
    let eventDate =
          parseTimeM True defaultTimeLocale "%Y-%-m-%-d" $ unpack (fromValue $ md !? "date") :: Maybe LocalTime
    let
      md' = md <> HashMap.fromList
              [ ("url", String . pack $ pathToRealUrlPath path (Object $ fromHashMapText md))
              , ("dayOrd", String $ dayOrdOfWeek eventDate)
              , ("dayName", String $ dayNameOfWeek eventDate)
              , ("day", String $ dayOfWeek eventDate)
              , ("month", String $ monthName eventDate)
              , ("summary", String $ eventName md 
                                     <> " at "
                                     <> address md
                                     <> " on "
                                     <> dayNameOfWeek eventDate
                                     <> " "
                                     <> dayOrdOfWeek eventDate
                                     <> " "
                                     <> monthName eventDate
                                     <> "!")
              ]
    html <- HashMap.fromList <$>
              (either (const []) (\x -> [ ("html", String x) ])
                <$> toHtml (FrontMatterContent md' (lit event))
              )
    pure . Object $ fromHashMapText md' <> fromHashMapText html

loadSourcesFrom :: FilePath -> IO [(FilePath, Value)]
loadSourcesFrom location = do
    events <- listDirectory location
    Extra.concatMapM (\x -> (\a -> [(path x, a)]) <$> (loadSource $ path x)) events
  where
    path x = location <> "/" <> x

toHtml :: Content -> IO (Either PandocError Text)
toHtml (FrontMatterContent fm lit) = runIO $ do
    doc <- readRST def lit
    html <- unpack <$> writeHtml5String def doc
    template <- liftIO $ parseGinger gingerResolver Nothing html
    case template of
      Left e -> do
        throwError $ PandocSomeError . pack $ show e
      Right t -> pure $ render t fm

parseContent :: L.ByteString -> Content
parseContent content = maybe (FrontMatterContent HashMap.empty $ toText $ L.toStrict content) id $ do
                                rest <- L.stripPrefix "---" content
                                let (yaml', content') = S.breakOn "---" rest
                                body <- L.stripPrefix "---" content'
                                pure $ parseFrontMatterContent yaml' body

parseFrontMatterContent :: L.ByteString -> L.ByteString -> Content
parseFrontMatterContent yaml lit = FrontMatterContent yaml' (toText $ L.toStrict lit)
    where
        yaml' = either (const HashMap.empty) id $ Y.decodeEither' (L.toStrict yaml)

valueToDateSuffix :: Value -> String
valueToDateSuffix (Object v) = unpack $ maybe "" (\(String x) -> "-" <> x) $ KeyMap.lookup "date" v

pathToLocalUrlPath :: String -> Value -> String
pathToLocalUrlPath a md = "html/" <> pathToRealUrlPath a md

pathToRealUrlPath :: String -> Value -> String
pathToRealUrlPath a md = (dropExtension a) <> valueToDateSuffix md <> ".html"

printTemplateError error = do
  TIO.putStrLn "There was some error loading the template:"
  print error
  Prelude.putStrLn $ peErrorMessage error

main :: IO ()
main = do
  eventItems    <- loadSourcesFrom "events"
  resourceItems <- loadSourcesFrom "resources"

  let context = HashMap.fromList
        [ ("events"   , Array $ Vector.fromList $ Prelude.map snd eventItems)
        , ("resources", Array $ Vector.fromList $ Prelude.map snd resourceItems)
        ]

  createDirectoryIfMissing True "html/events"
  indexFile <- template "templates/index.ginger"
  either printTemplateError
    (\x -> L.writeFile "html/index.html" . L.fromStrict . encodeUtf8 $ render x context)
    indexFile

  eventsTemplate <- template "templates/event.ginger"
  either printTemplateError
    (\x -> do
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

  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set (field @"envLogger") lgr . set (field @"envRegion") NorthVirginia
  systemTime <- systemSeconds <$> getSystemTime

  let
    invalidationId = "invalidation " <> (pack . show $ systemTime)
    distributionId = "E1N5IONLD4WMAF"
    b = "auslansocial.com" :: Text
    b' = BucketName b
    html = "text/html"
    css = "text/css"
    files :: [ (String, String, Text) ]
    files = [ ("html/index.html", "index.html", html)
            , ("html/sitemain.css", "sitemain.css", css)
            ] <> Prelude.map (\((a, b)) -> (pathToLocalUrlPath a b, pathToRealUrlPath a b, html)) eventItems
    c = ChunkSize 1024*1024

    say :: Text -> IO ()
    say = liftIO . TIO.putStrLn
    
    paths = (Prelude.map (\((_, a, _)) -> "/" <> toText a) files)

    invalidations = newInvalidationBatch
                      (newPaths (Prelude.length paths) & field @"items" .~ Just paths)
                      "anInvalidationID"

    invalidation = newCreateInvalidation distributionId invalidations


  args <- getArgs
  if "--push" `elem` args then do
    runResourceT $ do
      forM_ files $ \((f, k, m)) -> do
        bdy <- chunkedFile c f
        send env $ newPutObject b' (ObjectKey $ pack k) bdy & set (field @"contentType") (Just m)
        liftIO $ say $ "Successfully Uploaded: "
          <> toText f <> " to " <> toText b' <> " - " <> toText k
        send env invalidation

    pure ()
  else
    print "Run this with --push if you want to push the results up!"

  TIO.putStrLn $ "\n\thttps://" <> b <> "\n"

