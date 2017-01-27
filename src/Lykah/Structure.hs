{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings
, TypeSynonymInstances
, FlexibleInstances
, Rank2Types
, TemplateHaskell
, MultiParamTypeClasses
-- ,NoMonomorphismRestriction
 #-}

-- | Types for the website's structure: Website, Theme, Pages, Types
module Lykah.Structure
       (module Lykah.Structure
       ,module Lykah.Assets
       ) where

import           Control.Monad.Identity
import           Control.Monad.Trans.Writer
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import           Data.Traversable hiding (sequence)
import           Lykah.Assets
import           Text.BlazeT.Html (Html, string)
import           Lykah.EHtml


data Website = Website { wTheme  :: Theme
                       , wRoot   :: String
                       , googleAnalyticsId :: Text
                       , wMenu  :: [PathedPage ()]
                       , wPosts  :: [PathedPage Post]
                       , wAssets :: Assets
                       }

-- | eigentlich sollte das  `HTML Template` heißen.  TODO
type SHtml2 a b = EHtml (Website,PathedPage a) b
type SHtml a = SHtml2 a ()

data Theme = Theme { tAssets :: Assets
                   , tPage :: forall a . SHtml a ->  SHtml a
                     -- ^ the first argument is an explicit
                   --     `body`. This is needed because the body of
                   --     current asset (in the enviromnent) should be
                   --     used in a static fashion and not used for
                   --     thinks like `tPage . tPost`
                   , tPost :: Bool -> SHtml Post
                   -- ^ Template for post rendering, with the first
                   -- argument indicating full page or single blog
                   -- entry
                   , tMenuItem :: forall a .
                                   Int 
                                -> Bool 
                                -> PathedPage ()
                                -> SHtml a
                   -- ^ first arg: one-based index of the current item
                   --
                   -- second arg: menu item for current page?
                   }

instance Show Theme where
  show a = "Theme"

data Page a = Page { pTitle :: Html
                   , pBody :: SHtml a
                   , pDetails :: a
                   }

type PathedPage a = Pathed (Page a)

-- version ..._old : withPathed sonst sind die links z.b. auf blog posts falsch?

mapSHtml_old :: (Page b -> Page a) -> SHtml a -> SHtml b
mapSHtml_old f = localEHtml $ second $ fmap f

withPage_old :: Page a -> SHtml a -> SHtml ()
withPage_old p = mapSHtml_old (const p)
      
capture_old :: Page a -> Page ()
capture_old p = p{pDetails=()
                 ,pBody= withPage_old p $ pBody p}


                
localSHtml :: (Pathed (Page b) -> Pathed (Page a)) -> SHtml a -> SHtml b
localSHtml f = localEHtml $ second f

withPathed :: Pathed (Page a) -> SHtml a -> SHtml b
withPathed p = localSHtml (const p)

withPage :: Page a -> SHtml a -> SHtml b
withPage p x = do pathed <- askPathed
                  withPathed (pathed{aContent=p}) x

-- runs a template with a given `pBody`
withBody :: SHtml a -- ^ Body
         -> SHtml a -- ^ Template that uses `Body`
         -> SHtml a  -- ^ Result
withBody b x = do p <- askPage 
                  withPage (p{pBody=b}) x
      
capture :: Pathed (Page a) -> Pathed (Page ())
capture pth = fmap capture' pth
  where capture' p = p{pDetails=()
                      ,pBody= withPathed pth $ pBody p}

relax :: SHtml () -> SHtml a
relax = localSHtml capture
              
data Post = Post {poDate  :: ZonedTime
                 -- ,  poTags :: ()
                 }
              deriving Show
                     
renderWebsite :: Website -> Assets
renderWebsite website@Website{wRoot = root , wMenu = pages, wPosts = posts} =
  sitemap' : (onlyUsed used $ assets' ++ rendered)
  where s = fmap strip
        env = s pages ++ s posts ++ s assets'
        (rendered,used) = mconcat $ (renderPage askBody <$> pages)
                          ++ (renderPage (tPost th True) <$> posts)
        renderPage :: (SHtml a) -> PathedPage a -> (Assets,[Identifier])
        renderPage templ p = renderEHtml (website,p) env $  tPage th templ
        assets' = wAssets website ++ tAssets th
        th = wTheme website
        sitemap' = sitemap root $ onlyUsed used rendered

instance Show a => Show (Page a) where
  show p = "Page { pTitle = "++ T.unpack(renderHtml $ pTitle p)++", pBody, pDetails = "
           ++ show(pDetails p)++"}"

readDetails f = readPage $ f . pDetails

readTheme f = readWebsite $ f . wTheme


askPage  ::  SHtml2 b (Page b)
askPage = readPathed $ aContent


askTheme  ::  SHtml2 b (Theme)
askTheme = readWebsite $ wTheme

askBody :: SHtml b
askBody = join $ readPage pBody


page ::
  Identifier
  -> Html -- ^ title
  -> FilePath -- ^ path
  -> Maybe (Pathed ()) -- ^ parent
  -> SHtml () -- ^ body
  -> PathedPage ()
page i t p pa c = Pathed p i (Just $ renderHtml t) pa $ Page t c ()

               
post ::
  Identifier
  -> FilePath -- ^ path
  -> String -- ^ zoned time in "%Y-%m-%d %H:%M %Z" format
  -> Html  -- ^ title
  -> Maybe (Pathed a) -- ^ parent
  -> SHtml Post -- ^ body
  -> PathedPage Post
post i p time t pa c = Pathed p i (Just $ renderHtml t) (strip <$> pa)
                       $ Page t c $
                       Post $ parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M %Z" time

-- ^ renders a subset of posts
postList :: (Page Post -> Bool) -> SHtml ()
postList f = do pb <- readTheme $ flip tPost False
                posts <- readWebsite (filter (f . aContent) . wPosts)
                forM_ posts $ flip withPathed pb
                  


-- | Generates the Menu
menu :: SHtml a
menu = do
  theme <- readTheme tMenuItem
  cur <- readPathed $ aId . root
  let g ix pp = theme ix (cur == aId pp) pp
  zipWithM_ g [1..] =<< readWebsite wMenu
           
