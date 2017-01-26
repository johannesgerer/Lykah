{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE 
 FlexibleInstances
,NoMonomorphismRestriction
 #-}
module Lykah.Pandoc (t2)
       where

import qualified Data.Map as M
import           Lykah.Structure hiding (Space)
import           Text.BlazeT
import           Text.Pandoc


class FromPandoc a where
  fromPandoc :: Pandoc -> Either String a

instance FromPandoc (Pathed (Page Post)) where
  fromPandoc doc@(Pandoc m blocks) =  
    post <$> f "id" <*> f "slug" <*> f "date" <*> s <*> pure (toHtmlT doc)
    where f x = getInlines m x >>= maybe (pError x m) Right
                . fmap concat . traverse fromInline
          s :: Either String (forall m . Monad m => HtmlT m ())
          -- s :: Monad m => Either String (HtmlT m ())
          s = Right $ undefined -- (undefined :: Html)-- :: Monad m => [Inline] -> MarkupT m ()) <$> getMetaString m "title"

b :: Either String Html -> Html
b (Left a) = return ()
b (Right a) = a

type ReturnNull = forall m. Monad m => m ()

id2 :: ReturnNull -> ReturnNull
id2 = id

testId2 :: ReturnNull
testId2 = id2 $ return ()

liftId :: Functor f => f ReturnNull -> f ReturnNull
liftId = fmap id

liftId3 :: (Monad m,Functor f) => f ReturnNull -> f (m ())
liftId3 = fmap id2

consume :: Functor f => f ReturnNull -> f ()
consume = fmap $ const ()

testLift = consume . liftId



g :: [Inline] -> Html
g = wrapMarkupT . writeHtml def . Pandoc (Meta def) . (:[]) . Plain

getInlines :: PrintfType r =>
  Meta -> String -> Either r [Inline]
getInlines m x = case lookupMeta x m of
  Just (MetaInlines ls) -> Right ls
  Just _ -> pError x m
  Nothing -> Left $ printf "Could not find Meta Field '%s' in\n%s"
             x $ unlines $ show <$> M.toList (unMeta m)

pError x m = Left $ printf "MetaSimple Parse Error for '%s':\n%s" x $ show m
  
toHtmlT = wrapMarkupT . writeHtml def 
                   
fromInline (Str s) = Just s
fromInline Space   = Just " "
fromInline _       = Nothing
                        

-- * Debugging Stuff

t2 :: IO (Either String (Pathed (Page Post)))
t2 = fromPandoc <$> test
    

test :: IO Pandoc
test = either (error . show) id . readMarkdown def <$> readFile "assets/a.md"  

mt (Pandoc (Meta m) _) = unlines $ show <$> M.toList m
mt2 = putStrLn . mt =<<test 
