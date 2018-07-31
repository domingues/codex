{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-
  Definitions for document and exercise pages
-}
module Codex.Page where

import           Text.Pandoc hiding (Code)
import qualified Text.Pandoc ( Inline(Code) )
import           Text.Pandoc.Walk
import           Text.XmlHtml
import           Text.Blaze.Renderer.XmlHtml

import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid
import           Control.Applicative

import           Data.List (intersperse)

import           Codex.Types
import           Codex.Time


--
-- a Page is a synonym for a Pandoc document 
-- either an active exercise or a passive document (e.g. an index)
--
type Page = Pandoc

emptyPage :: Page
emptyPage = Pandoc nullMeta []

-- extract metadata 
pageMeta :: Page -> Meta
pageMeta (Pandoc meta _) = meta

-- extract document description (blocks)
pageDescription :: Page -> [Block]
pageDescription (Pandoc _ blocks) = blocks

-- lookup title either as metadata or in the first header
pageTitle :: Page -> Maybe [Inline]
pageTitle p
  = case docTitle (pageMeta p) of
      [] -> firstHeader (pageDescription p)
      inlines -> Just inlines
  where
    firstHeader :: [Block] -> Maybe [Inline]
    firstHeader blocks = listToMaybe [h | Header _ _ h <- blocks]


-- list of accepted languages for an exercise
pageLanguages :: Page -> [Language]
pageLanguages = languages . pageMeta

languages :: Meta -> [Language]
languages meta =
  fromMaybe [] 
  (lookupFromMeta "languages" meta
   <|>
   do lang <- lookupFromMeta "language" meta
      return [lang])


-- text for submission filled in by default
pageDefaultText :: Page -> Maybe Text
pageDefaultText = lookupFromMeta "code" . pageMeta

-- is this an exercise page?
pageIsExercise :: Page -> Bool
pageIsExercise p
  = fromMaybe False $ lookupFromMeta "exercise" (pageMeta p)

-- time interval for valid submissions
pageInterval :: Page -> Interval Time
pageInterval = metaInterval . pageMeta

metaInterval :: Meta -> Interval Time
metaInterval meta
  = fromMaybe (Interval Nothing Nothing) $
    (lookupFromMeta "valid" meta >>= parseInterval)

-- feedback level for submissions
pageFeedback :: Page -> Int
pageFeedback p = fromMaybe 100 (lookupFromMeta "feedback" (pageMeta p))



-- | read from a metadata value
class FromMetaValue a where
  fromMeta :: MetaValue -> Maybe a

instance FromMetaValue Text where
  fromMeta = Just . metaText

instance FromMetaValue Bool where
  fromMeta (MetaBool b) = Just b
  fromMeta _            = Nothing

instance FromMetaValue Int where
  fromMeta (MetaString s) =
    case reads s of
      ((n,""):_) -> Just n
      _ -> Nothing
  fromMeta _ = Nothing

instance {-# OVERLAPPABLE #-} FromMetaValue a => FromMetaValue [a] where
  fromMeta (MetaList l) = mapM fromMeta l
  fromMeta _            = Nothing

instance {-# OVERLAPPING #-} FromMetaValue String where
  fromMeta = Just . T.unpack . metaText


instance FromMetaValue Language where
  fromMeta v = fmap (Language . T.toLower) (fromMeta v)



-- | lookup from metadata value
lookupFromMeta :: FromMetaValue a => String -> Meta -> Maybe a
lookupFromMeta tag meta = lookupMeta tag meta >>= fromMeta


-- collect text in inline and block elements
inlineText :: Inline -> Text
inlineText (Str s)   = T.pack s
inlineText Space     = " "
inlineText LineBreak  = "\n"
inlineText (Math _ s) = T.pack s
inlineText (Text.Pandoc.Code _ s) = T.pack s
inlineText (RawInline _ s) = T.pack s
inlineText (Quoted qt l) =  quote <> T.concat (map inlineText l) <> quote
  where quote = case qt of
            SingleQuote -> "\'"
            DoubleQuote -> "\""
inlineText (Emph l)  = T.concat (map inlineText l)
inlineText (Strong l)  = T.concat (map inlineText l)
inlineText (Superscript l)  = T.concat (map inlineText l)
inlineText (Subscript l)  = T.concat (map inlineText l)
inlineText (SmallCaps l)  = T.concat (map inlineText l)
inlineText (Span _ l)  = T.concat (map inlineText l)
inlineText _         = T.empty

blockText :: Block -> Text
blockText (Plain l)       = query inlineText l
blockText (Para p)        = query inlineText p
blockText (Header _ _ l)  = query inlineText l
blockText (CodeBlock _ s) = T.pack s
blockText (RawBlock  _ s) = T.pack s
blockText _               = T.empty

inlineString :: Inline -> String
inlineString = T.unpack . inlineText


-- | collect text from a meta value
metaText :: MetaValue -> Text
metaText (MetaString s) =
  T.pack s
metaText (MetaBool b) =
  T.pack (show b)
metaText (MetaInlines l) =
  T.concat (map inlineText l)
metaText (MetaBlocks l) =
  T.concat (map blockText l)
metaText (MetaList l) =
  T.concat (intersperse "," $ map metaText l)
metaText (MetaMap m) =
  T.concat $
  intersperse ","  [T.concat [T.pack k, ":", metaText v] | (k,v)<- Map.assocs m]


-- | render a page as a list of HTML nodes
pageToHtml :: Page -> [Node]
pageToHtml = renderHtmlNodes . writeHtml opts
  where
    opts = def { writerExtensions = pandocExtensions
               , writerHTMLMathMethod = MathJax "/mathjax",
                 writerHighlight = True
               }

blocksToHtml :: [Block] -> [Node]
blocksToHtml blocks = pageToHtml (Pandoc nullMeta blocks)


-- | read a file and parse markdown to a Pandoc document
readMarkdownFile :: FilePath -> IO Pandoc
readMarkdownFile fp = do
  r <- readMarkdown opts <$> readFile fp
  case r of
    Left err -> ioError (userError $ show err)
    Right doc -> return doc
  where
    opts = def { readerExtensions = pandocExtensions
               , readerSmart = True
               }

 
