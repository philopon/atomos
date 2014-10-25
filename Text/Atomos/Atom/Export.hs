{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
module Text.Atomos.Atom.Export where

import Data.DList (DList)
import qualified Data.DList as D
import qualified Data.List.NonEmpty as N
import Data.Monoid
import Text.Atomos.Types
import Text.HTML.TagSoup

import Data.Time
import qualified Data.Foldable as F

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text.Encoding as T

textElement :: S.ByteString -> TextElement -> DList (Tag S.ByteString)
textElement n (PlainText t) = D.fromList [TagOpen n [("type", "text")],  TagText $ T.encodeUtf8 t, TagClose n]
textElement n (HtmlText  t) = D.fromList [TagOpen n [("type", "html")],  TagText $ T.encodeUtf8 t, TagClose n]
textElement n (XHtmlText t) = D.singleton (TagOpen n [("type", "xhtml")]) <> D.fromList t <> D.singleton (TagClose n)

person :: S.ByteString -> Person -> DList (Tag S.ByteString)
person t (Person n mbu mbe) =
    D.singleton (TagOpen t []) <>
    D.fromList [TagOpen "name" [], TagText (T.encodeUtf8 n), TagClose "name"] <>
    maybe D.empty (\u -> D.fromList [TagOpen "uri" [], TagText u, TagClose "uri"]) mbu <>
    maybe D.empty (\e -> D.fromList [TagOpen "email" [], TagText e, TagClose "email"]) mbe <>
    D.singleton (TagClose t)

category :: Category -> DList (Tag S.ByteString)
category (Category t mbs mbl) = D.fromList [ TagOpen "category" attrs, TagClose "category" ] where
  attrs =
      (:) ("term", t) $
      maybe id (\s -> (:) ("scheme", s)) mbs $
      maybe id (\l -> (:) ("label", T.encodeUtf8 l)) mbl []

generator :: Generator -> DList (Tag S.ByteString)
generator (Generator mbu mbv t) = D.fromList [ TagOpen "generator" attrs, TagText t, TagClose "generator"]
  where
    attrs =
        maybe id (\u -> (:) ("uri", u)) mbu $
        maybe id (\v -> (:) ("version", v)) mbv []

link_ :: S.ByteString -> Link_ -> DList (Tag S.ByteString)
link_ rel (Link_ h mby mbl mbt mbn) = D.fromList [TagOpen "link" attrs, TagClose "link"] where
  attrs =
      (:) ("href", h) $
      (:) ("rel", rel) $
      maybe id (\y -> (:) ("type", y)) mby $
      maybe id (\l -> (:) ("hreflang", l)) mbl $
      maybe id (\t -> (:) ("title", T.encodeUtf8 t)) mbt $
      maybe id (\n -> (:) ("length", n)) mbn []

link :: Link -> DList (Tag S.ByteString)
link (Link r l) = link_ r l

inlinePlain :: Maybe S.ByteString -> T.Text -> DList (Tag S.ByteString)
inlinePlain mby t = D.fromList
    [TagOpen "content" $ maybe [] ((:[]) . ("type",)) mby, TagText $ T.encodeUtf8 t, TagClose "content"]

inlineXML :: Maybe S.ByteString -> [Tag S.ByteString] -> DList (Tag S.ByteString)
inlineXML mby x =
    D.singleton (TagOpen "content" $ maybe [("type", "xhtml")] ((:[]) . ("type",)) mby) <>
    D.fromList x <> D.singleton (TagClose "content")

inlineBASE64 :: Maybe S.ByteString -> S.ByteString -> DList (Tag S.ByteString)
inlineBASE64 mby b = D.fromList
    [ TagOpen "content" $ maybe [("type", "application/octet-stream")] ((:[]) . ("type",)) mby
    , TagText $ Base64.encode b, TagClose "content"]

outOfLine :: Maybe S.ByteString -> S.ByteString -> DList (Tag S.ByteString)
outOfLine mby s = D.fromList
    [ TagOpen "content" . (:) ("src", s) $ maybe [] ((:[]) . ("type",)) mby, TagClose "content"]

contentSummary :: Content -> (DList (Tag S.ByteString), DList (Tag S.ByteString))
contentSummary (ContentInlinePlain  (InlinePlain  mby t s)) = (inlinePlain  mby t, maybe mempty (textElement "summary") s)
contentSummary (ContentInlineXml    (InlineXml    mby x s)) = (inlineXML    mby x, maybe mempty (textElement "summary") s)
contentSummary (ContentInlineBase64 (InlineBase64 mby b s)) = (inlineBASE64 mby b, textElement "summary" s)
contentSummary (ContentOutOfLine    (OutOfLine    mby r s)) = (outOfLine    mby r, textElement "summary" s)

noContSummary :: NoContent -> (DList (Tag S.ByteString), DList (Tag S.ByteString))
noContSummary (NoContent a s) = (mconcat . map (link_ "alternate") $ N.toList a, maybe mempty (textElement "summary") s)

linkContSummary :: Either Content NoContent -> (DList (Tag S.ByteString), DList (Tag S.ByteString), DList (Tag S.ByteString))
linkContSummary = either ((\(c, s) -> (mempty, c, s)) . contentSummary) ((\(l, s) -> (l, mempty, s)) . noContSummary)

simple :: S.ByteString -> S.ByteString -> DList (Tag S.ByteString)
simple n i = D.fromList [TagOpen n [], TagText i, TagClose n]

formatRfc3339Date :: ZonedTime -> S.ByteString
formatRfc3339Date = S8.pack . formatTime defaultTimeLocale "%FT%T%Q%z"

dateElem :: S.ByteString -> ZonedTime -> DList (Tag S8.ByteString)
dateElem e z = D.fromList [TagOpen e [], TagText $ formatRfc3339Date z, TagClose e]

entry :: F.Foldable f => Entry f -> DList (Tag S.ByteString)
entry (Entry as cats cnt cbrs i ls pub rs src ttl upd) =
    let (alts, contents, summary) = linkContSummary cnt in
        D.singleton (TagOpen "entry" []) <>

        textElement "title" ttl <>
        mconcat (map category cats) <>
        contents <>
        summary <>
        maybe mempty feed src <>

        simple "id" i <>

        F.foldl' (\j a -> j <> person "author" a) mempty as <>
        mconcat (map (person "contributor") cbrs) <>
        maybe mempty (textElement "rights") rs <>

        mconcat (map link ls) <> alts <>

        maybe mempty (dateElem "published") pub <>
        dateElem "updated" upd <>

        D.singleton (TagClose "entry")

authorEntries :: Either (N.NonEmpty Person, [Entry []]) ([Entry N.NonEmpty])
              -> (DList (Tag S.ByteString), DList (Tag S.ByteString))
authorEntries = either
    (\(as, es) -> (mconcat . map (person "author") $ N.toList as, mconcat $ map entry es))
    (\es       -> (mempty, mconcat $ map entry es))

feed :: Feed -> DList (Tag S.ByteString)
feed (Feed cats cbrs gen icon i self other logo rs sub ttl upd ae) =
    let (authors, entries) = authorEntries ae in
        D.singleton (TagOpen "?xml" [("version", "1.0"), ("encoding", "utf-8")]) <>
        D.singleton (TagOpen "feed" [("xmlns", "http://www.w3.org/2005/Atom")]) <>
        textElement "title" ttl <>
        maybe mempty (textElement "subtitle") sub <>
        mconcat (map category cats) <>

        maybe mempty (simple "icon") icon <>
        maybe mempty (simple "logo") logo <>

        authors <>
        mconcat (map (person "contributor") cbrs) <>
        maybe mempty (textElement "rights") rs <>

        simple "id" i <>
        maybe mempty generator gen <>

        maybe mempty (link_ "self") self <>
        mconcat (map link other) <>

        dateElem "updated" upd <>

        entries <>

        D.singleton (TagClose "feed")

renderAtom :: Feed -> L.ByteString
renderAtom f = renderTags (map (fmap L.fromStrict) $ D.toList $ feed f)
