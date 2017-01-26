{-# LANGUAGE OverloadedStrings
, TypeSynonymInstances
, FlexibleInstances
, Rank2Types
, TemplateHaskell
, MultiParamTypeClasses
-- ,NoMonomorphismRestriction
 #-}
-- | Themes are used to describe how to render a website/structure
-- into HTML
module Lykah.Theme
  (theme
  ,sect
  ,loremMore
  ) where

import           Control.Monad.Identity
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import           Data.Traversable hiding (sequence)
import           Lykah.Structure
import           Lykah.BuildingBlocks
import           Lykah.EHtml
import qualified Lykah.Style as S
import           Text.BlazeT
import qualified Text.BlazeT as B
import           Text.BlazeT.Html5 hiding (head)
import qualified Text.BlazeT.Html5 as H
import           Text.BlazeT.Html5.Attributes hiding (id)
import qualified Text.BlazeT.Html5.Attributes as A


-- * Static Assets

theme :: Theme
theme = Theme ([myCss,js] ++ fmap fst bgVideo) t1page bentry

myCss = Pathed "style.css" "myCss" Nothing $ Write $ T.fromStrict S.myCss
        
js = Pathed "main.js" "js" Nothing  $ Copy "assets/js.js"

bgVideo :: [(Pathed Asset, String)]
bgVideo = b <$> ["jpg","mp4","webm"]
  where b a = (Pathed f a Nothing $ Copy $ "assets/videos/" </> f, a)
          where f = "Dragonflyinultraslowmotion" <.> a

t1page :: SHtml a -- ^ Body
       -> SHtml a
t1page body = do
  Page t _ _ <- readPage id
  docTypeHtml $ do
    H.head $ do
        meta ! charset "UTF-8"
        path' myCss >>= stylesheet'
        javascript "//code.jquery.com/jquery-1.11.1.min.js"
        path' js >>= javascript'
        javascript "//ajax.googleapis.com/ajax/libs/webfont/1.4.7/webfont.js"
        readPage pTitle >>= H.title
    H.body $ do
      H.div ! A.id "content" $ do
        H.div ! A.id "menu" $ do
          mapM_ ((! onclick "//return false;") . autoLink') =<< readWebsite wMenu
          clear
        clear
        body
      H.div !  A.id "v" $ do
        p <- path' $ fst $ head bgVideo
        video ! poster' p
          $ mapM_ (\(x,y) -> path' x >>= (source ! type_' ("video" </> y) !).src') $ tail bgVideo
        H.span ! A.id "schaap" $ do
          tell "\"Dragonfly\" by Joris Schaap ("
          blank $ a ! href "http://creativecommons.org/licenses/by/3.0/" $ "CC BY"
          ")"
  
      p $ "This website was created TODO" 
      googleAnalytics =<< readWebsite googleAnalyticsId 


     
-- | Blog Entry
bentry ::  Bool -- ^ full page and not single blog entry
       -> SHtml Post
bentry single = do
  Page t body (Post dat) <- askPage
  let d = string $ formatTime defaultTimeLocale "%x %R %Z" (dat ::ZonedTime)
  H.div ! class_' (postClass "entry" ::String) $ do
    H.div ! class_ "title" $ do
      postTitle t
      H.div ! class_ "date" $ d
      clear
    H.div ! class_ "body" $ body
    clear
  where
    (postClass,postTitle) = if single then ((++" single"),id)
                            else (id, \n -> do
                                     p <- join $ readPathed path'
                                     H.a ! A.href (toValue p) $ n)

-- | Section
sect :: Html -- ^ Title
     -> Html -- ^ Body
     -> Html
sect t b = H.div ! class_ "section" $ do
  H.div ! class_ "title" $ t
  H.div ! class_ "body" $ b 
