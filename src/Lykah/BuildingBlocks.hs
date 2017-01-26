{-# LANGUAGE OverloadedStrings
 #-}
module Lykah.BuildingBlocks
  where

import           Lykah.Structure
import           Lykah.EHtml
import           Text.BlazeT
import qualified Text.BlazeT as B
import qualified Text.BlazeT.Html5 as H
import           Text.BlazeT.Html5 hiding (head,link)
import qualified Text.BlazeT.Html5.Attributes as A
import           Text.BlazeT.Html5.Attributes hiding (id)

more :: Html
more = ((tell "...") >>) $ H.a ! href "#" ! class_ "more" $ "(more)" 

clear :: Html
clear = H.div ! A.style "clear:both" $ ""
     
loremMore i = mapM_ (p.(>> more).tell) $ take i lorem

autoRelLink :: Identifier -> EHtml (Website,PathedPage a) ()
autoRelLink id = do Pathed{aPath=p, aName=n} <- lift $ lookupId id 
                    readPathed aId
                    maybe (error $ printf "Identifier %s provides no name." id)
                      (link' p . (mappend $ fromString p )) $  n


-- | Correct base meta tag pointing to top directory
autoBase :: SHtml a
autoBase = do
  curPath <- readPathed aPath
  let curBase = concat $ replicate
        (pred $ length $ takeWhile (/= ".") $ iterate takeDirectory curPath) "../"
  base ! href' curBase
