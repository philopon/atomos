{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Atomos.Atom.Import where

import Control.Applicative ((<*), (*>), (<$>), (<$))
import qualified Control.Applicative as A
import Text.Atomos.Types
import Text.HTML.TagSoup
import Text.Parsec

import Data.List
import qualified Data.List.NonEmpty as N
import Data.Time
import Data.Char

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

type TagParser a = Parsec [Tag S.ByteString] ParseMode a

raw :: (Tag S.ByteString -> Maybe a) -> TagParser a
raw f = tokenPrim show next test
  where
    test t       = f t
    next pos _ _ = incSourceColumn pos 1

rawBool :: (Tag S.ByteString -> Bool) -> TagParser (Tag S.ByteString)
rawBool f = raw $ \t -> if f t then Just t else Nothing

rawBool_ :: (Tag S.ByteString -> Bool) -> TagParser ()
rawBool_ f = raw $ \t -> if f t then Just () else Nothing

tagOpen :: TagParser (Tag S.ByteString)
tagOpen = rawBool isTagOpen

tagOpenName :: S.ByteString -> TagParser [Attribute S.ByteString]
tagOpenName n = raw $ \tag -> case tag of
    TagOpen t a | t == n -> Just a
    _                    -> Nothing

tagOpenName_ :: S.ByteString -> TagParser ()
tagOpenName_ n = rawBool_ $ isTagOpenName n

tagText_ :: TagParser ()
tagText_ = () <$ tagText

tagText :: TagParser S.ByteString
tagText = raw $ \tag -> case tag of
    TagText t -> Just t
    _         -> Nothing

skipText :: TagParser ()
skipText = optional tagText

tagCloseName :: S.ByteString -> TagParser ()
tagCloseName n = rawBool_ (isTagCloseName n) <* many tagText

anyTag_ :: TagParser ()
anyTag_ = rawBool_ (const True)

unknownTag :: TagParser ()
unknownTag = do
    TagOpen o _ <- tagOpen
    loop o
  where
    loop o = tagCloseName o <|> (anyTag_ >> loop o)

inTagText :: S.ByteString -> TagParser S.ByteString
inTagText t = tagOpenName_ t *> tagText <* tagCloseName t

newtype IV a = IV { getIV :: Maybe a }

newIV :: IV a
newIV = IV Nothing

setIV :: String -> IV a -> a -> TagParser (IV a)
setIV _ (IV Nothing) a = return . IV $ Just a
setIV f iv _           = getState >>= \case
    Strict  -> fail $ "Attempt to set to an IV twice: " ++ f
    Fuzzy{} -> return iv

tagTextSetIV :: S.ByteString -> IV S.ByteString -> TagParser (IV S.ByteString)
tagTextSetIV n iv = inTagText n >>= \v' -> setIV (S8.unpack n) iv v'

person :: S.ByteString -> TagParser Person
person t = tagOpenName_ t >> skipText >> loop newIV newIV newIV
  where
    loop n u e = choice
        [ tagTextSetIV "name"  n >>= \n' -> loop n' u e
        , tagTextSetIV "uri"   u >>= \u' -> loop n u' e
        , tagTextSetIV "email" e >>= \e' -> loop n u e'
        , do tagCloseName t
             name <- case getIV n of
                 Nothing -> getState >>= \case
                     Strict               -> fail "person without name."
                     Fuzzy{defaultPerson} -> return $ personName defaultPerson
                 Just nm -> return $ T.decodeUtf8 nm
             return $ Person name (getIV u) (getIV e)
        , unknownTag >> loop n u e
        ]

author :: TagParser Person
author = person "author"

category :: TagParser Category
category = do
    attrs <- tagOpenName "category"
    optional tagText_
    tagCloseName "category"
    term <- case lookup "term" attrs of
        Nothing -> getState >>= \case
            Strict             -> fail "category without term."
            Fuzzy{defaultTerm} -> return defaultTerm
        Just c  -> return c
    return $ Category term (lookup "scheme" attrs)
        (T.decodeUtf8 <$> lookup "label" attrs)

contributor :: TagParser Person
contributor = person "contributor"

generator :: TagParser Generator
generator = do
    attrs <- tagOpenName "generator"
    gtxt  <- tagText
    tagCloseName "generator"
    return $ Generator (lookup "uri" attrs) (lookup "version" attrs) gtxt

icon :: TagParser URI
icon = inTagText "icon"

ident :: TagParser URI
ident = inTagText "id"

link :: TagParser Link
link = do
    attrs <- tagOpenName "link"
    optional tagText_
    tagCloseName "link"
    href <- case lookup "href" attrs of
        Nothing -> getState >>= \case
            Strict -> fail "link without href."
            Fuzzy{defaultHref} -> return defaultHref
        Just h  -> return h
    return $ Link
        (maybe "alternate" id $ lookup "rel" attrs) $ Link_ href
        (lookup "type" attrs)
        (lookup "hreflang" attrs)
        (T.decodeUtf8 <$> lookup "title" attrs)
        (lookup "length" attrs)

logo :: TagParser URI
logo = inTagText "logo"

published :: TagParser ZonedTime
published = inTagText "published" >>= \d -> getState >>= \case
    Strict             -> parseRfc3339Date d
    Fuzzy{defaultTime} -> parseRfc3339Date d <|> return defaultTime

textElement :: S.ByteString -> TagParser TextElement
textElement t = do
    attrs <- tagOpenName t
    let typ = maybe "text" id $ lookup "type" attrs
    case typ of
        "text"  -> PlainText <$> plain
        "html"  -> HtmlText  <$> plain
        "xhtml" -> xml
        e -> getState >>= \case
            Strict  -> fail $ "unknown text element type: " ++ show e
            Fuzzy{} -> case (isXmlMimeType e, e == "text/html") of
                (True, _)      -> xml
                (False, True)  -> HtmlText <$> plain
                (False, False) -> HtmlText <$> plain
  where
    plain = T.decodeUtf8 <$> tagText <* tagCloseName t
    xml = do
        ts <- many $ rawBool (not . isTagCloseName t)
        tagCloseName t
        return $ XHtmlText ts

rights :: TagParser TextElement
rights = textElement "rights"

subtitle :: TagParser TextElement
subtitle = textElement "subtitle"

summary :: TagParser TextElement
summary = textElement "summary"

parseRfc3339Date :: (Monad m, A.Alternative m) => S.ByteString -> m ZonedTime
parseRfc3339Date s = p "%FT%TZ" A.<|> p "%FT%T%z" A.<|> p "%FT%T%Q%z" A.<|> p "%FT%T%QZ"
    A.<|> fail "Date format incorrect."
  where
    p f = parseTimeM False defaultTimeLocale f (S8.unpack s)

title :: TagParser TextElement
title = textElement "title"

updated :: TagParser ZonedTime
updated = inTagText "updated" >>= \d -> getState >>= \case
    Strict             -> parseRfc3339Date d
    Fuzzy{defaultTime} -> parseRfc3339Date d <|> return defaultTime

content :: TagParser (Maybe TextElement -> Maybe Content)
content = do
    attrs <- tagOpenName "content"
    let typ = maybe "text" id $ lookup "type" attrs
    case lookup "src" attrs of
        Just src -> do
            tagCloseName "content"
            return $ fmap (ContentOutOfLine . OutOfLine (Just typ) src)
        Nothing -> do 
            r <- case S8.map toLower typ of
                t | isPlain t -> do
                      b <- tagText
                      return $ \s -> Just . ContentInlinePlain $ InlinePlain (Just typ) (T.decodeUtf8 b) s
                  | isXml   t -> do
                      ts <- many $ rawBool (not . isTagCloseName "content")
                      return $ \s -> Just . ContentInlineXml $ InlineXml (Just typ) ts s
                  | otherwise -> Base64.decode <$> tagText >>= \case
                      Right b -> return $ fmap (ContentInlineBase64 . InlineBase64 (Just typ) b)
                      Left  e -> getState >>= \case
                        Strict -> fail e
                        Fuzzy{base64DecodeFailed} -> return base64DecodeFailed
            tagCloseName "content"
            return r
  where
    isPlain s = s == "text"  || s == "html" || isPlainMimeType s
    isXml   s = s == "xhtml" || isXmlMimeType s

isPlainMimeType :: S.ByteString -> Bool
isPlainMimeType s = "text/" `S8.isPrefixOf` s && s /= "text/xml"

isXmlMimeType :: S.ByteString -> Bool
isXmlMimeType s = "+xml" `S8.isSuffixOf` s || "/xml" `S8.isSuffixOf` s

data EntryState = EntryState
    { esAuthor      :: [Person] -> [Person]
    , esCategory    :: [Category] -> [Category]
    , esContent     :: IV (Maybe TextElement -> Maybe Content)
    , esSummary     :: IV TextElement
    , esContributor :: [Person] -> [Person]
    , esId          :: IV URI
    , esLinks       :: [Link] -> [Link]
    , esPublished   :: IV ZonedTime
    , esRights      :: IV TextElement
    , esSource      :: IV Feed
    , esTitle       :: IV TextElement
    , esUpdated     :: IV ZonedTime
    }

completeId :: Maybe URI -> TagParser URI
completeId (Just i) = return i
completeId Nothing  = getState >>= \mode -> case mode of
    Strict -> fail "id element required"
    Fuzzy{} -> let idnt = defaultInitialId mode
               in idnt <$ modifyState (\s -> s { defaultInitialId = updateId mode idnt } )


entry :: TagParser (Entry [])
entry = tagOpenName "entry" >> skipText >> loop (EntryState id id newIV newIV id newIV id newIV newIV newIV newIV newIV)
  where
    loop es = choice
        [ author      >>= \a -> loop es { esAuthor      = (esAuthor      es . (a:)) }
        , category    >>= \c -> loop es { esCategory    = (esCategory    es . (c:)) }
        , contributor >>= \c -> loop es { esContributor = (esContributor es . (c:)) }
        , link        >>= \l -> loop es { esLinks       = (esLinks       es . (l:)) }
        , content     >>= setIV "content"   (esContent   es) >>= \c -> loop es { esContent   = c }
        , summary     >>= setIV "summary"   (esSummary   es) >>= \s -> loop es { esSummary   = s }
        , ident       >>= setIV "id"        (esId        es) >>= \i -> loop es { esId        = i }
        , published   >>= setIV "published" (esPublished es) >>= \p -> loop es { esPublished = p }
        , rights      >>= setIV "rights"    (esRights    es) >>= \r -> loop es { esRights    = r }
        , source      >>= setIV "source"    (esSource    es) >>= \s -> loop es { esSource    = s }
        , title       >>= setIV "title"     (esTitle     es) >>= \t -> loop es { esTitle     = t }
        , updated     >>= setIV "updated"   (esUpdated   es) >>= \u -> loop es { esUpdated   = u }
        , tagCloseName "entry" >> result es
        , unknownTag  >> loop es
        ]

    result es = do
        mode <- getState
        let fuz f = case mode of
                Strict  -> Nothing
                Fuzzy{} -> Just $ f mode
        (contf, links) <- case getIV (esContent es) of
            Nothing -> case partition (("alternate" ==) . linkRel) (esLinks es []) of
                ([], ls) -> case mode of
                    Strict                  -> fail "alternate link element required"
                    Fuzzy{defaultAlternate} -> return (Just . Right . NoContent (N.fromList [defaultAlternate]), ls)
                (as, ls) -> return (Just . Right . NoContent (N.fromList $ map linkAttrs as), ls)
            Just cf -> return (fmap Left . cf, esLinks es [])
        cnts <- maybe (fail "summary element required") return $ contf $ (getIV $ esSummary es) A.<|> fuz defaultSummary
        idnt <- completeId $ getIV (esId es)
        titl <- maybe (fail "title element required") return $ getIV (esTitle es) A.<|> fuz defaultTitle
        upd  <- maybe (fail "updated element required") return $ getIV (esUpdated es) A.<|> fuz defaultTime
        return $ Entry (esAuthor es []) (esCategory es []) cnts (esContributor es [])
            idnt links (getIV $ esPublished es) (getIV $ esRights es) (getIV $ esSource es) titl upd

data FeedState = FeedState
    { fsCategories   :: [Category] -> [Category]
    , fsContributors :: [Person] -> [Person]
    , fsGenerator    :: IV Generator
    , fsIcon         :: IV URI
    , fsId           :: IV URI
    , fsLinks        :: [Link] -> [Link]
    , fsLogo         :: IV URI
    , fsRights       :: IV TextElement
    , fsSubtitle     :: IV TextElement
    , fsTitle        :: IV TextElement
    , fsUpdated      :: IV ZonedTime
    , fsAuthors      :: [Person] -> [Person]
    , fsEntries      :: [Entry []] -> [Entry []]
    }

feedLike :: S.ByteString -> TagParser Feed
feedLike n = tagOpenName_ n >> skipText >> loop (FeedState id id newIV newIV newIV id newIV newIV newIV newIV newIV id id)
  where
    loop fs = choice
        [ category    >>= \c -> loop fs { fsCategories   = (fsCategories   fs . (c:)) }
        , contributor >>= \c -> loop fs { fsContributors = (fsContributors fs . (c:)) }
        , link        >>= \l -> loop fs { fsLinks        = (fsLinks        fs . (l:)) }
        , author      >>= \a -> loop fs { fsAuthors      = (fsAuthors      fs . (a:)) }
        , entry       >>= \e -> loop fs { fsEntries      = (fsEntries      fs . (e:)) }

        , generator >>= setIV "generator" (fsGenerator fs) >>= \g -> loop fs { fsGenerator = g }
        , icon      >>= setIV "icon"      (fsIcon      fs) >>= \i -> loop fs { fsIcon      = i }
        , ident     >>= setIV "id"        (fsId        fs) >>= \i -> loop fs { fsId        = i }
        , logo      >>= setIV "logo"      (fsLogo      fs) >>= \l -> loop fs { fsLogo      = l }
        , rights    >>= setIV "rights"    (fsRights    fs) >>= \r -> loop fs { fsRights    = r }
        , subtitle  >>= setIV "subtitle"  (fsSubtitle  fs) >>= \s -> loop fs { fsSubtitle  = s }
        , title     >>= setIV "title"     (fsTitle     fs) >>= \t -> loop fs { fsTitle     = t }
        , updated   >>= setIV "updated"   (fsUpdated   fs) >>= \u -> loop fs { fsUpdated   = u }
        , tagCloseName n >> result fs
        , unknownTag  >> loop fs
        ]
    result fs = do
        mode <- getState
        let fuz f = case mode of
                Strict  -> Nothing
                Fuzzy{} -> Just $ f mode
        aes <- case N.nonEmpty $ fsAuthors fs [] of
            Nothing -> case sequence $ map requireEntryAuthor (fsEntries fs []) of
                Nothing -> case mode of
                    Strict  -> fail "all entry require author element."
                    Fuzzy{} -> return . Right $
                        map (requireEntryAuthorDef (defaultPerson mode)) (fsEntries fs [])
                Just es -> return (Right es)
            Just as -> return $ Left (as, fsEntries fs [])
        (self, other) <- case partition (("self" ==) . linkRel) (fsLinks fs []) of
            (s:_, o) -> return (Just $ linkAttrs s, o)
            ([],  o) -> return (Nothing, o)
        idnt <- completeId $ getIV (fsId fs)
        titl <- maybe (fail "title element required.")   return $ getIV (fsTitle fs)   A.<|> fuz defaultTitle
        upd  <- maybe (fail "updated element required.") return $ getIV (fsUpdated fs) A.<|> fuz defaultTime
        return $ Feed (fsCategories fs []) (fsContributors fs []) (getIV $ fsGenerator fs)
            (getIV $ fsIcon fs) idnt self other (getIV $ fsLogo fs) (getIV $ fsRights fs)
            (getIV $ fsSubtitle fs) titl upd aes

feed, source :: TagParser Feed
feed   = feedLike "feed"
source = feedLike "source"

requireEntryAuthor :: Entry [] -> Maybe (Entry N.NonEmpty)
requireEntryAuthor e = (\a -> e { entryAuthors = a } ) <$> N.nonEmpty (entryAuthors e)

requireEntryAuthorDef :: Person -> Entry [] -> Entry N.NonEmpty
requireEntryAuthorDef p e = case N.nonEmpty (entryAuthors e) of
    Nothing -> e { entryAuthors = p N.:| [] }
    Just p' -> e { entryAuthors = p' }

atomP :: TagParser Feed
atomP = many (rawBool (not . isTagOpenName "feed")) >> feed

data ParseMode
    = Strict
    | Fuzzy { defaultPerson      :: Person
            , defaultTerm        :: S8.ByteString
            , defaultHref        :: URI
            , defaultTime        :: ZonedTime
            , base64DecodeFailed :: Maybe TextElement -> Maybe Content
            , defaultAlternate   :: Link_
            , defaultSummary     :: TextElement
            , defaultTitle       :: TextElement
            , defaultInitialId   :: URI
            , updateId           :: URI -> URI
            }

defaultFuzzy :: ParseMode
defaultFuzzy = Fuzzy
    { defaultPerson      = Person "unknown" Nothing Nothing
    , defaultTerm        = "unknown"
    , defaultHref        = "http://example.com"
    , defaultTime        = ZonedTime (LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0)) utc
    , base64DecodeFailed = \s -> Just . ContentInlinePlain $ InlinePlain Nothing "base64 decode failed" s
    , defaultAlternate   = Link_ "http://example.com" Nothing Nothing Nothing Nothing
    , defaultSummary     = PlainText "no summary"
    , defaultTitle       = PlainText "no title"
    , defaultInitialId   = "no-id-0"
    , updateId           = \i -> let i' = S8.pack . show . succ . maybe 0 fst $ S8.readInt (S.drop 6 i)
                                 in "no-id-" `S.append` i'
    }

parseAtom :: ParseMode -> SourceName -> S8.ByteString -> Either ParseError Feed
parseAtom m s f = runParser atomP m s (parseTags f)
