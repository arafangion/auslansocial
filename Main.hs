{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Search as S
import qualified Data.Yaml as Y
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Encoding
import Data.Text.IO
import Data.Text
import Data.Default
import Data.Either
import Text.Pandoc
import Data.Aeson
import Control.Monad.Identity (Identity, runIdentity)
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Text.Blaze
import qualified Data.Vector as Vector
import System.Directory
import qualified Control.Monad.Extra as Extra

data Content = SimpleContent { lit :: Text }
             | FrontMatterContent { metadata :: HashMap Text Y.Value , lit :: Text }

gingerResolver :: FilePath -> IO (Maybe String)
gingerResolver source = L.readFile source >>= return . Just . unpack . toText

template :: FilePath -> IO (Either ParserError (Text.Ginger.Template SourcePos))
template = parseGingerFile gingerResolver

scopeLookup :: (Hashable k, Eq k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context = toGVal $ HashMap.lookup key context

render :: Text.Ginger.Template SourcePos -> HashMap VarName Value -> Text
render template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context = makeContextHtml contextLookup
  in htmlSource $ runGinger context template

loadSource :: FilePath -> IO Value
loadSource path = do
    event <- parseContent <$> (L.readFile path)
    html <- HashMap.fromList <$> either (const []) (\x -> [("html", String x)]) <$> toHtml event
    pure $ case event of
        (FrontMatterContent metadata _) -> Object $ metadata <> html
        _ -> Object html

loadSourcesFrom :: FilePath -> IO [Value]
loadSourcesFrom location = do
    events <- listDirectory location
    eventItems <- Extra.concatMapM (\x -> pure <$> (loadSource $ location <> "/" <> x)) events
    pure eventItems

main :: IO ()
main = do
    eventItems <- loadSourcesFrom "events"
    resourceItems <- loadSourcesFrom "resources"

    let context = HashMap.fromList [ ("events", Array $ Vector.fromList eventItems)
                                   , ("resources", Array $ Vector.fromList resourceItems) ]

    indexFile <- template "templates/index.ginger"
    case indexFile of
        Left _ -> pure ()
        Right idx -> L.writeFile "html/index.html" $ L.fromStrict . encodeUtf8 $ render idx context

toHtml :: Content -> IO (Either PandocError Text)
toHtml (SimpleContent lit) = runIO $ do
    doc <- readRST def lit
    writeHtml5String def doc

toHtml (FrontMatterContent _ lit) = toHtml (SimpleContent lit)

toText :: L.ByteString -> Text
toText = decodeUtf8 . L.toStrict

parseContent :: L.ByteString -> Content
parseContent content = case L.stripPrefix "---" content of
                        Just content -> f ( S.breakOn "---" content )
                        Nothing -> SimpleContent $ toText content
    where
        f (a, b) = case L.stripPrefix "---" b of
                    Just b -> parseFrontMatterContent a b
                    Nothing -> SimpleContent $ toText content

parseFrontMatterContent :: L.ByteString -> L.ByteString -> Content
parseFrontMatterContent yaml lit = FrontMatterContent yaml' (toText lit)
    where
        yaml' = either (const HashMap.empty) id $ Y.decodeEither' (L.toStrict yaml)
