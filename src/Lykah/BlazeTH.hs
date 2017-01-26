{-# LANGUAGE TemplateHaskell
,FlexibleInstances
,Rank2Types
, MultiParamTypeClasses
, OverloadedStrings
 #-}
module Lykah.BlazeTH where

import           Language.Haskell.TH
import           Text.BlazeT


-- usage:
--
--   `dash [|toValue|] undefined ['href]`
--
--  defines `href' x = href $ toValue x` 
--
dash :: Q Exp -> Q Type -- ^ this is (currently) ignored
     -> [Name] -> Q [Dec]
dash f _ = mapM $
  \n -> funD (mkName $ nameBase n ++ "'")
        [clause [varP x] --pattern
          (normalB --body
            [| $(varE n) $ $f $(varE x) -- :: $t , where t is the second argument above
             |]) []]
  where x = mkName "x"

dashAttr :: [Name] -> Q [Dec]
dashAttr = dash [|toValue|] [t|Attribute|]

dashTag :: [Name] -> Q [Dec]
dashTag = dash [|toMarkup|] [t|forall m. Monad m => MarkupT m ()|]


-- instance Attributable (a -> MarkupM b) where
--   h ! f = (B.! f) . h
          
-- asd = html $ "asd"
