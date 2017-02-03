{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings
, TypeSynonymInstances
, FlexibleInstances
, TemplateHaskell
, NoMonomorphismRestriction
 #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | A writer monad that writes the assets needed for the current
-- asset (internal monad) while producing the current asset's HTML
-- (monad transformer)
module Lykah.EHtml
       (module Lykah.EHtml
       ,module Text.BlazeT.Renderer.Text
         )
       where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ListLike as L
import qualified Data.ListLike.String as L
import qualified Data.Text.Lazy as T
import           Lykah.Assets
import           Lykah.BlazeTH
import qualified Text.Blaze as Bl
import           Text.Blaze.Internal (Attributable)
import           Text.BlazeT
import qualified Text.BlazeT as B
import qualified Text.BlazeT.Html5 as H
import           Text.BlazeT.Html5 hiding (link, object)
import qualified Text.BlazeT.Html5.Attributes as A
import           Text.BlazeT.Html5.Attributes hiding (id)
import           Text.BlazeT.Renderer.Text


-- | Html monad with inner Reader (See Env) and Writer Monad writing a list of identifiers of actually used assets
type EHtml self a = HtmlT (RWS (Env self) [Identifier] ()) a

readWebsite f = lift $ reader $ f . fst . eCur
readPathed f = lift $ reader $ f . snd. eCur
readPage f = readPathed $ f . aContent

askPathed  ::  EHtml (a,b) (b)
askPathed = lift $ reader $ snd . eCur
  
localEHtml :: (a -> b) -> EHtml b c -> EHtml a c
localEHtml f = mapMarkupT $ withRWS $ \r s -> (f <$> r,s) 

-- | render the given EHtml to the given path
renderEHtml :: (env,Pathed a) -- ^ current env and current asset to be rendered
            -> [Pathed ()] -- ^ All pathed entities that can be referenced
            -> EHtml (env,Pathed a) () -> (Assets,[Identifier])
renderEHtml cur refs x = ([fmap (const $ Write t) $ snd cur], ids)
  where (t,ids) = evalRWS (execWith renderHtml x) (uniquify refs){eCur=cur} ()
               
poster :: AttributeValue -> Attribute
poster = customAttribute "poster"

stylesheet :: Monad m => AttributeValue -> MarkupT m ()
stylesheet x = H.link ! rel "stylesheet" ! type_ "text/css" ! href x

javascript :: Monad m => AttributeValue -> MarkupT m ()
javascript x = script ! src x $ mempty

jsonLd' :: Monad m => MarkupT m () -> MarkupT m ()
jsonLd' = script ! type_ "application/ld+json"

jsonLd :: Monad m => [Pair] -> MarkupT m ()
jsonLd = jsonLd' . unsafeLazyByteString . encode . object

dashAttr ['href,'A.id,'A.style,'A.type_,'A.src,'poster,'A.for,'value,'class_, 'stylesheet, 'javascript]
dashTag ['a,'b,'h1,'video,'H.label, 'H.title,'script]

link' :: (ToValue h, Bl.ToMarkup t, Monad m) => h -> t -> MarkupT m ()
link' url = a' ! href' url 

link :: Monad m => AttributeValue -> MarkupT m () -> MarkupT m ()
link url = a ! href url

linkB' :: (ToValue h, Bl.ToMarkup t, Monad m) => h -> t -> MarkupT m ()
linkB' url = blank . link' url

linkB :: Monad m => AttributeValue -> MarkupT m () -> MarkupT m ()
linkB url = blank . link url

path' :: IDO a => a -> EHtml b FilePath
path' = path . getId

autoLink' :: IDO a1 => a1 -> EHtml a ()
autoLink' = autoLink . getId
  
path :: Identifier -> EHtml b FilePath
path = lift . fmap aPath . lookupId

-- | Link with given name and href from identifier plus optional
-- fragment
namedLinkF :: Maybe String -- ^ Fragment
           -> EHtml a ()
           -> Identifier
           -> EHtml a ()
namedLinkF fr name pId = do
  o <- lift $ lookupId pId 
  link (toValue $ maybe id (flip (#)) fr $ aPath o) name

namedLink :: EHtml a () -> Identifier -> EHtml a ()
namedLink = namedLinkF Nothing

namedLinkB :: EHtml a () -> Identifier -> EHtml a ()
namedLinkB n = blank . namedLink n

namedLinkF' :: IDO b => String -- ^ Fragment
            -> EHtml a () -> b -> EHtml a ()
namedLinkF' fr n = namedLinkF (Just fr) n . getId

namedLink' :: IDO b => EHtml a () -> b -> EHtml a ()
namedLink' n = namedLink n . getId

namedLinkB' :: IDO b => EHtml a () -> b -> EHtml a ()
namedLinkB' n = blank . namedLink' n

-- | creates a link with name from object
autoLink ::  Identifier -> EHtml a ()
autoLink id = do
  Pathed{aPath=p, aName=(Just n)} <- lift $ lookupNamedId id 
  link' p n
                       
anchor :: Monad m => AttributeValue -> MarkupT m ()
anchor n = H.div ! class_ "anchor" $ a ! name n  $ preEscapedText "&nbsp;" 

(!#) :: Attributable h => h -> AttributeValue -> h
a !# b= a ! A.id b

(!$) :: (ToValue a, Attributable h) => h -> a -> h
a !$ b= a ! id' b

-- | append fragment
(#)
  :: (L.ListLike full item, IsString full) => full -> full -> full
a # b = L.concat [a,"#",b]

quote :: Monad m => MarkupT m () -> MarkupT m ()
quote x = mconcat [preEscapedText "&ldquo;"
                  ,x
                  ,preEscapedText "&rdquo;"
                  ]


lorem = [" Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin a nunc tortor. Praesent sagittis accumsan felis. Curabitur venenatis purus mollis augue consectetur euismod. Quisque mattis, tortor sit amet imperdiet vulputate, massa enim vestibulum nulla, eget aliquam nisi libero eu dui. Morbi imperdiet tortor eu augue lacinia, quis tristique diam placerat. Nunc quis lobortis est, quis adipiscing risus. Pellentesque nisi sem, convallis at posuere egestas, bibendum id turpis. Ut vestibulum sodales massa, quis egestas velit aliquam non."
                ,"Suspendisse eleifend quis quam pulvinar molestie. Fusce viverra blandit neque, nec fermentum odio ullamcorper in. Vivamus vel purus at mi imperdiet semper. Suspendisse sodales, elit in aliquet sodales, erat tortor gravida ipsum, sit amet consectetur mi ipsum sed justo. Etiam nisl justo, varius quis dolor a, adipiscing pharetra tortor. Aenean ultrices, est et volutpat pellentesque, nisi quam malesuada massa, vel bibendum nisl dui non neque. Nullam eu lacus a nibh vestibulum iaculis venenatis id diam. Aenean congue lacus et leo faucibus consectetur. Pellentesque id sem eget sem pellentesque sagittis. Integer non pretium nulla. Donec pellentesque urna in suscipit varius. Curabitur sollicitudin fermentum faucibus. Nunc sollicitudin ornare neque nec vestibulum. Nunc gravida consectetur lacinia. Duis semper aliquam vestibulum. Sed eleifend, nisi vel ultricies volutpat, dui tellus dictum orci, nec pulvinar turpis leo sed felis."
                ,"Interdum et malesuada fames ac ante ipsum primis in faucibus. Donec dignissim, urna quis aliquam pretium, magna justo consequat sem, eu dapibus odio urna a tortor. Nunc consectetur rutrum felis, ut blandit metus venenatis sit amet. Nullam blandit risus vitae imperdiet accumsan. Nam lobortis, ligula vitae hendrerit vehicula, mauris leo consequat felis, eget porttitor est ligula in dolor. Curabitur accumsan elit iaculis libero ultricies, et mattis eros pretium. Nam odio odio, dictum non nibh id, porta porta mi. Proin eu dolor cursus, ullamcorper diam ut, fringilla urna. Nulla tempor purus vel erat vulputate varius. Praesent sagittis fermentum mollis. Ut vestibulum gravida tristique. In hac habitasse platea dictumst. In eu tortor semper, pretium nisi vitae, euismod risus. Suspendisse nunc tortor, porta non aliquam ac, pulvinar et mauris. Duis at dolor sit amet elit sollicitudin mattis sed suscipit nunc. Nulla justo augue, feugiat condimentum pellentesque at, consequat vitae odio."
                 ,"Donec nunc mauris, dapibus eu orci at, lacinia iaculis nulla. Maecenas in velit lacus. Duis sodales nisi eu arcu auctor volutpat. Sed id risus quis dolor fringilla commodo nec id dolor. Aenean id justo nec neque dignissim fringilla vitae sed est. Proin eu pellentesque dolor. Duis hendrerit ipsum non rhoncus euismod. Quisque elementum aliquet velit, eu fermentum augue rhoncus non. Nam eu sem tempor, ultrices libero sit amet, rutrum odio. Donec auctor, elit vitae gravida vestibulum, arcu quam tristique urna, eu pulvinar quam leo non leo."
                  ,"Sed eget magna eros. Aliquam pellentesque felis quis pharetra faucibus. Suspendisse dignissim erat ut orci adipiscing, vel adipiscing mauris facilisis. Suspendisse mattis suscipit justo in scelerisque. Proin nec euismod neque, sit amet rhoncus nisl. Vestibulum in tristique velit. Mauris dignissim posuere metus, ut vehicula orci egestas sed. Quisque mattis massa eget lorem pharetra tincidunt. Interdum et malesuada fames ac ante ipsum primis in faucibus. Vivamus molestie ultrices tortor, ac consectetur velit hendrerit sed. Donec eleifend pellentesque ipsum, id euismod ante mattis cursus. Proin consequat dapibus fermentum. Nunc sagittis suscipit tortor, eu congue sem ullamcorper elementum. Cras in ipsum dictum, adipiscing felis ullamcorper, varius dui."]


googleAnalytics id = script' $ T.concat 
                  ["(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create', '"
                  ,id
                  ,"', 'auto');"
                   -- do this after page load:
                 -- ,"ga('require', 'displayfeatures');ga('send', 'pageview');"
                  ]

stackExchange
  :: Monad m => AttributeValue -> AttributeValue -> MarkupT m ()
stackExchange id name = link ("http://stackexchange.com/users/" <> id) $
  img ! src' ("http://stackexchange.com/users/flair/" <> id <>".png")
  ! width "208" ! height "58" ! alt tt ! A.title tt
  where tt = "profile for " <> name <> " on Stack Exchange, a network of free, community-driven Q&amp;A sites"
       
blank :: Attributable h => h -> h
blank x = x ! target "_blank"
