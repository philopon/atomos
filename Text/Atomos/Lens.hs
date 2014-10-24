{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Atomos.Lens where

import Control.Applicative

import Data.Time
import Data.Profunctor
import qualified Data.Text as T
import qualified Data.ByteString as S

import Data.List.NonEmpty
import Text.HTML.TagSoup
import qualified Text.Atomos.Types as A

type Lens  s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s   a   = Lens s s a a

type Prism  s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t) 
type Prism' s   a   = Prism s s a a

type Iso  s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t) 
type Iso' s   a   = Iso s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b 
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

_PlainText :: Prism' A.TextElement T.Text
_PlainText = prism A.PlainText to where
  to (A.PlainText b) = Right b
  to              x  = Left x
{-# INLINE _PlainText #-}

_HtmlText :: Prism' A.TextElement T.Text
_HtmlText = prism A.HtmlText to where
  to (A.HtmlText b) = Right b
  to             x  = Left x
{-# INLINE _HtmlText #-}

_XHtmlText :: Prism' A.TextElement [Tag S.ByteString]
_XHtmlText = prism A.XHtmlText to where
  to (A.XHtmlText b) = Right b
  to             x  = Left x
{-# INLINE _XHtmlText #-}

class HasURI a b | a -> b where
    uri :: Lens' a b

name :: Lens' A.Person T.Text
name f (A.Person n u e) = fmap (\n' -> A.Person n' u e) (f n)
{-# INLINE name #-}

instance HasURI A.Person (Maybe A.URI) where
    uri f (A.Person n u e) = fmap (\u' -> A.Person n u' e) (f u)
    {-# INLINE uri #-}

email :: Lens' A.Person (Maybe A.EMail)
email f (A.Person n u e) = fmap (\e' -> A.Person n u e') (f e)
{-# INLINE email #-}

term :: Lens' A.Category S.ByteString
term f (A.Category t s l) = fmap (\t' -> A.Category t' s l) (f t)
{-# INLINE term #-}

scheme :: Lens' A.Category (Maybe A.URI)
scheme f (A.Category t s l) = fmap (\s' -> A.Category t s' l) (f s)
{-# INLINE scheme #-}

label :: Lens' A.Category (Maybe T.Text)
label f (A.Category t s l) = fmap (\l' -> A.Category t s l') (f l)
{-# INLINE label #-}

instance HasURI A.Generator (Maybe A.URI) where
    uri f (A.Generator u v t) = fmap (\u' -> A.Generator u' v t) (f u)
    {-# INLINE uri #-}

version :: Lens' A.Generator (Maybe S.ByteString)
version f (A.Generator u v t) = fmap (\v' -> A.Generator u v' t) (f v)
{-# INLINE version #-}

class HasText a b | a -> b where
    text :: Lens' a b

instance HasText A.Generator S.ByteString where
    text f (A.Generator u v t) = fmap (\t' -> A.Generator u v t') (f t)
    {-# INLINE text #-}

class HasLink_ a where
    link_ :: Lens' a A.Link_

    href :: Lens' a A.URI
    href = link_ . g where g f (A.Link_ h y l t n) = fmap (\h' -> A.Link_ h' y l t n) (f h)
    {-# INLINE href #-}

    linkType :: Lens' a (Maybe S.ByteString)
    linkType = link_ . g where g f (A.Link_ h y l t n) = fmap (\y' -> A.Link_ h y' l t n) (f y)
    {-# INLINE linkType #-}

    hrefLang :: Lens' a (Maybe A.Lang)
    hrefLang = link_ . g where g f (A.Link_ h y l t n) = fmap (\l' -> A.Link_ h y l' t n) (f l)
    {-# INLINE hrefLang #-}

    linkTitle :: Lens' a (Maybe T.Text)
    linkTitle = link_ . g where g f (A.Link_ h y l t n) = fmap (\t' -> A.Link_ h y l t' n) (f t)
    {-# INLINE linkTitle #-}

    linkLength :: Lens' a (Maybe S.ByteString)
    linkLength = link_ . g where g f (A.Link_ h y l t n) = fmap (\n' -> A.Link_ h y l t n') (f n)
    {-# INLINE linkLength #-}

instance HasLink_ A.Link_ where
    link_ = id
    {-# INLINE link_ #-}

instance HasLink_ A.Link where
    link_ f (A.Link r a) = fmap (\a' -> A.Link r a') (f a)
    {-# INLINE link_ #-}

rel :: Lens' A.Link S.ByteString
rel f (A.Link r a) = fmap (\r' -> A.Link r' a) (f r)
{-# INLINE rel #-}

class HasMimeType a where
    mimeType :: Lens' a (Maybe S.ByteString)

class HasSummary a b | a -> b where
    summary :: Lens' a b

_InlinePlain :: Prism' A.Content A.InlinePlain
_InlinePlain = prism A.ContentInlinePlain to where
  to (A.ContentInlinePlain b) = Right b
  to                       x  = Left x
{-# INLINE _InlinePlain #-}

instance HasMimeType A.InlinePlain where
    mimeType f (A.InlinePlain m t s) = fmap (\m' -> A.InlinePlain m' t s) (f m)
    {-# INLINE mimeType #-}

instance HasText A.InlinePlain T.Text where
    text f (A.InlinePlain m t s) = fmap (\t' -> A.InlinePlain m t' s) (f t)
    {-# INLINE text #-}

instance HasSummary A.InlinePlain (Maybe A.TextElement) where
    summary f (A.InlinePlain m t s) = fmap (\s' -> A.InlinePlain m t s') (f s)
    {-# INLINE summary #-}

_InlineXml :: Prism' A.Content A.InlineXml
_InlineXml = prism A.ContentInlineXml to where
  to (A.ContentInlineXml b) = Right b
  to                     x  = Left x
{-# INLINE _InlineXml #-}

instance HasMimeType A.InlineXml where
    mimeType f (A.InlineXml m x s) = fmap (\m' -> A.InlineXml m' x s) (f m)
    {-# INLINE mimeType #-}

xml :: Lens' A.InlineXml [Tag S.ByteString]
xml f (A.InlineXml m x s) = fmap (\x' -> A.InlineXml m x' s) (f x)
{-# LANGUAGE xml #-}

instance HasSummary A.InlineXml (Maybe A.TextElement) where
    summary f (A.InlineXml m x s) = fmap (\s' -> A.InlineXml m x s') (f s)
    {-# INLINE summary #-}

_InlineBase64 :: Prism' A.Content A.InlineBase64
_InlineBase64 = prism A.ContentInlineBase64 to where
  to (A.ContentInlineBase64 b) = Right b
  to                        x  = Left x
{-# INLINE _InlineBase64 #-}

instance HasMimeType A.InlineBase64 where
    mimeType f (A.InlineBase64 m b s) = fmap (\m' -> A.InlineBase64 m' b s) (f m)
    {-# INLINE mimeType #-}

base64 :: Lens' A.InlineBase64 S.ByteString
base64 f (A.InlineBase64 m b s) = fmap (\b' -> A.InlineBase64 m b' s) (f b)
{-# INLINE base64 #-}

instance HasSummary A.InlineBase64 A.TextElement where
    summary f (A.InlineBase64 m t s) = fmap (\s' -> A.InlineBase64 m t s') (f s)
    {-# INLINE summary #-}

_OutOfLine :: Prism' A.Content A.OutOfLine
_OutOfLine = prism A.ContentOutOfLine to where
  to (A.ContentOutOfLine b) = Right b
  to                     x  = Left x
{-# INLINE _OutOfLine #-}

instance HasMimeType A.OutOfLine where
    mimeType f (A.OutOfLine m u s) = fmap (\m' -> A.OutOfLine m' u s) (f m)
    {-# INLINE mimeType #-}

instance HasURI A.OutOfLine A.URI where
    uri f (A.OutOfLine m u s) = fmap (\u' -> A.OutOfLine m u' s) (f u)
    {-# INLINE uri #-}

instance HasSummary A.OutOfLine A.TextElement where
    summary f (A.OutOfLine m u s) = fmap (\s' -> A.OutOfLine m u s') (f s)
    {-# INLINE summary #-}

alternates :: Lens' A.NoContent (NonEmpty A.Link_)
alternates f (A.NoContent a s) = fmap (\a' -> A.NoContent a' s) (f a)
{-# INLINE alternates #-}

instance HasSummary A.NoContent (Maybe A.TextElement) where
    summary f (A.NoContent a s) = fmap (\s' -> A.NoContent a s') (f s)
    {-# INLINE summary #-}

class HasAuthors a b | a -> b where
    authors :: Lens' a b

instance HasAuthors (A.Entry f) (f A.Person) where
    authors f e = fmap (\a' -> e { A.entryAuthors = a' } ) (f $ A.entryAuthors e)
    {-# INLINE authors #-}

class HasCategories a where
    categories :: Lens' a [A.Category]

instance HasCategories (A.Entry f) where
    categories f e = fmap (\c' -> e { A.entryCategories = c' } ) (f $ A.entryCategories e)
    {-# INLINE categories #-}

content :: Lens' (A.Entry f) (Either A.Content A.NoContent)
content f e = fmap (\c' -> e { A.entryContent = c' } ) (f $ A.entryContent e)
{-# INLINE content #-}

class HasContributors a where
    contributors :: Lens' a [A.Person]

instance HasContributors (A.Entry f) where
    contributors f e = fmap (\c' -> e { A.entryContributors = c' } ) (f $ A.entryContributors e)
    {-# INLINE contributors #-}

class HasId a where
    ident :: Lens' a A.URI

instance HasId (A.Entry f) where
    ident f e = fmap (\i' -> e { A.entryId = i' } ) (f $ A.entryId e)
    {-# INLINE ident #-}

class HasLinks a where
    links :: Lens' a [A.Link]

instance HasLinks (A.Entry f) where
    links f e = fmap (\l' -> e { A.entryLinks = l' } ) (f $ A.entryLinks e)
    {-# INLINE links #-}

published :: Lens' (A.Entry f) (Maybe ZonedTime)
published f e = fmap (\p' -> e { A.entryPublished = p' } ) (f $ A.entryPublished e)
{-# INLINE published #-}

class HasRights a where
    rights :: Lens' a (Maybe A.TextElement)

instance HasRights (A.Entry f) where
    rights f e = fmap (\r' -> e { A.entryRights = r' } ) (f $ A.entryRights e)
    {-# INLINE rights #-}

source :: Lens' (A.Entry f) (Maybe A.Feed)
source f e = fmap (\s' -> e { A.entrySource = s' } ) (f $ A.entrySource e)
{-# INLINE source #-}

class HasTitle a where
    title :: Lens' a A.TextElement

instance HasTitle (A.Entry f) where
    title f e = fmap (\t' -> e { A.entryTitle = t' } ) (f $ A.entryTitle e)
    {-# INLINE title #-}

class HasUpdated a where
    updated :: Lens' a ZonedTime

instance HasUpdated (A.Entry f) where
    updated f e = fmap (\u' -> e { A.entryUpdated = u' } ) (f $ A.entryUpdated e)
    {-# INLINE updated #-}

instance HasCategories A.Feed where
    categories f a = fmap (\c' -> a { A.feedCategories = c' }) (f $ A.feedCategories a)
    {-# INLINE categories #-}

instance HasContributors A.Feed where
    contributors f a = fmap (\c' -> a { A.feedContributors = c' }) (f $ A.feedContributors a)
    {-# INLINE contributors #-}

generator :: Lens' A.Feed (Maybe A.Generator)
generator f a = fmap (\g' -> a { A.feedGenerator = g' }) (f $ A.feedGenerator a)
{-# INLINE generator #-}

icon :: Lens' A.Feed (Maybe A.URI)
icon f a = fmap (\i' -> a { A.feedIcon = i' }) (f $ A.feedIcon a)
{-# INLINE icon #-}

instance HasId A.Feed where
    ident f a = fmap (\i' -> a { A.feedId = i' }) (f $ A.feedId a)
    {-# INLINE ident #-}

self :: Lens' A.Feed (Maybe A.Link_)
self f a = fmap (\l' -> a { A.feedSelfLink = l' }) (f $ A.feedSelfLink a)
{-# INLINE self #-}

instance HasLinks A.Feed where
    links f a = fmap (\l' -> a { A.feedOtherLinks = l' }) (f $ A.feedOtherLinks a)
    {-# INLINE links #-}

logo :: Lens' A.Feed (Maybe A.URI)
logo f a = fmap (\l' -> a { A.feedLogo = l' }) (f $ A.feedLogo a)
{-# INLINE logo #-}

instance HasRights A.Feed where
    rights f a = fmap (\r' -> a { A.feedRights = r' }) (f $ A.feedRights a)
    {-# INLINE rights #-}

subtitle :: Lens' A.Feed (Maybe A.TextElement)
subtitle f a = fmap (\t' -> a { A.feedSubtitle = t' }) (f $ A.feedSubtitle a)
{-# INLINE subtitle #-}

instance HasTitle A.Feed where
    title f a = fmap (\t' -> a { A.feedTitle = t' }) (f $ A.feedTitle a)
    {-# INLINE title #-}

instance HasUpdated A.Feed where
    updated f a = fmap (\u' -> a { A.feedUpdated = u' }) (f $ A.feedUpdated a)
    {-# INLINE updated #-}

authorsEntries :: Lens' A.Feed (Either (NonEmpty A.Person, [A.Entry []]) ([A.Entry NonEmpty]))
authorsEntries f a = fmap (\a' -> a { A.feedAuthorsEntries = a' }) (f $ A.feedAuthorsEntries a)
{-# INLINE authorsEntries #-}
