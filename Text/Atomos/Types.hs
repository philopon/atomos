{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Atomos.Types where

import qualified Data.Text as T
import qualified Data.ByteString as S
import Data.Typeable
import Data.Time
import Data.List.NonEmpty
import Text.HTML.TagSoup

type URI   = S.ByteString
type EMail = S.ByteString

-- | RFC3066 language code
type Lang  = S.ByteString

data TextElement
    = PlainText { plainText :: T.Text }
    | HtmlText  { htmlText  :: T.Text }
    | XHtmlText { xHtmlText :: [Tag S.ByteString] }
    deriving (Show, Typeable, Eq)

data Person = Person
    { personName  :: T.Text
    , personUri   :: Maybe URI
    , personEmail :: Maybe EMail
    }
    deriving (Show, Read, Typeable, Eq)

data Category = Category
    { categoryTerm   :: S.ByteString
    , categoryScheme :: Maybe URI
    , categoryLabel  :: Maybe T.Text
    }
    deriving (Show, Read, Typeable, Eq)

data Generator = Generator
    { generatorUri     :: Maybe URI
    , generatorVersion :: Maybe S.ByteString
    , generatorText    :: S.ByteString
    }
    deriving (Show, Read, Typeable, Eq)

data Link_ = Link_
    { linkHref     :: URI
    , linkType     :: Maybe S.ByteString
    , linkHrefLang :: Maybe Lang
    , linkTitle    :: Maybe T.Text
    , linkLength   :: Maybe S.ByteString
    }
    deriving (Show, Read, Typeable, Eq)

data Link = Link { linkRel   :: S.ByteString
                 , linkAttrs :: Link_
                 }
    deriving (Show, Read, Typeable, Eq)

data Feed = Feed
    { feedCategories     :: [Category]
    , feedContributors   :: [Person]
    , feedGenerator      :: Maybe Generator
    , feedIcon           :: Maybe URI
    , feedId             :: URI
    , feedSelfLink       :: Maybe Link_
    , feedOtherLinks     :: [Link]
    , feedLogo           :: Maybe URI
    , feedRights         :: Maybe TextElement
    , feedSubtitle       :: Maybe TextElement
    , feedTitle          :: TextElement
    , feedUpdated        :: ZonedTime
    , feedAuthorsEntries :: Either (NonEmpty Person, [Entry []]) ([Entry NonEmpty])
    }
    deriving (Show, Typeable)

data InlinePlain = InlinePlain
    { inlineTextType    :: Maybe S.ByteString
    , inlineText        :: T.Text
    , inlineTextSummary :: Maybe TextElement
    }
    deriving (Show, Typeable, Eq)


data InlineXml = InlineXml
    { inlineXmlType    :: Maybe S.ByteString
    , inlineXml        :: [Tag S.ByteString]
    , inlineXmlSummary :: Maybe TextElement
    }
    deriving (Show, Typeable, Eq)

data InlineBase64 = InlineBase64
    { inlineBase64Type    :: Maybe S.ByteString
    , inlineBase64        :: S.ByteString
    , inlineBase64Summary :: TextElement
    }
    deriving (Show, Typeable, Eq)

data OutOfLine = OutOfLine
    { outOfLineType    :: Maybe S.ByteString
    , outOfLineSrc     :: URI
    , outOfLineSummary :: TextElement
    }
    deriving (Show, Typeable, Eq)

data NoContent = NoContent
    { noContentAltLinks :: NonEmpty Link_
    , noContentSummary  :: Maybe TextElement
    }
    deriving (Show, Typeable, Eq)

data Content
    = ContentInlinePlain  InlinePlain
    | ContentInlineXml    InlineXml
    | ContentInlineBase64 InlineBase64
    | ContentOutOfLine    OutOfLine
    deriving (Show, Typeable, Eq)

data Entry f = Entry
    { entryAuthors      :: f Person
    , entryCategories   :: [Category]
    , entryContent      :: Either Content NoContent
    , entryContributors :: [Person]
    , entryId           :: URI
    , entryLinks        :: [Link]
    , entryPublished    :: Maybe ZonedTime
    , entryRights       :: Maybe TextElement
    , entrySource       :: Maybe Feed
    , entryTitle        :: TextElement
    , entryUpdated      :: ZonedTime
    } deriving (Typeable)

instance Show (f Person) => Show (Entry f) where
    show (Entry author category content contributor idnt other pub rights src title updated) =
        "Entry { entryAuthors = " ++ show author ++
        ", entryCategories = "    ++ show category ++
        ", entryContentAlts = "   ++ show content ++
        ", entryContributors = "  ++ show contributor ++
        ", entryId = "            ++ show idnt ++
        ", entryLinks = "         ++ show other ++
        ", entryPublished = "     ++ show pub ++
        ", entryRights = "        ++ show rights ++
        ", entrySource = "        ++ show src ++
        ", entryTitle = "         ++ show title ++
        ", entryUpdated = "       ++ show updated ++
        "}"
