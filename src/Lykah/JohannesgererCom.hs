{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings
, TypeSynonymInstances
, FlexibleInstances
, MultiParamTypeClasses
, NoMonomorphismRestriction
 #-}

-- | Lykah 'website' powering http://johannesgerer.com
module Lykah.JohannesgererCom
  where

import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Lykah.EHtml
import           Lykah.Formatting (sformat, (%), (%.))
import qualified Lykah.Formatting as F
import           Lykah.Structure
import           Lykah.Theme2
import           Text.BlazeT
import qualified Text.BlazeT as B
import qualified Text.BlazeT.Html5 as H
import           Text.BlazeT.Html5 hiding (head,link,main)
import qualified Text.BlazeT.Html5.Attributes as A
import           Text.BlazeT.Html5.Attributes hiding (id)
-- import           Lykah.Pandoc

-- todo: use Shake to leverage things like newCache
-- http://declaredvolatile.org/blog/2014-09-14-writing-a-simple-blog-with-shake-and-pandoc/

-- against automatic slug creation: a filename should NEVER change. so
-- produce a slug once and keep it in the meta data. do not use the
-- filename of the post or derive it from the title with (changing
-- implementations,...)

-- contents in markdown? file:///usr/share/doc/terminator/html/advancedusage.html

website = do
  posts <- myPosts
  return $ Website theme "http://johannesgerer.com" "UA-27460943-1"
    pages posts assets

main :: IO ()
main = generateAssets True "output". renderWebsite =<< website

assets :: [Pathed Asset]
assets@(photo:photoLR:wincor:diss:diplom:tensor:dirac:memisScreen:
        haxCharts:_) =
  [Pathed "img/me.jpg" "meJpg" Nothing Nothing
    $ Copy "assets/me1.jpg"
  ,Pathed "img/me_low_res.jpg" "meJpgLowRes" Nothing Nothing
    $ Copy "assets/me2.jpg"
  ,Pathed "img/wincor_nixdorf_keyboard.jpg" "wincor" Nothing Nothing
    $ Copy "assets/wincor_nixdorf_keyboarb.jpg"
  ,Pathed "pdf/diss_gerer.pdf" "diss" Nothing Nothing
    $ Copy "assets/diss_online.pdf"
  ,Pathed "pdf/diploma_thesis_gerer.pdf" "diplom" Nothing Nothing
    $ Copy "assets/Diplom.pdf"
  ,Pathed "pdf/tensor_charge_gerer.pdf" "tensor" Nothing Nothing
    $ Copy "assets/Tensorladung.pdf"
  ,Pathed "pdf/dirac_inversion_gerer.pdf" "dirac" Nothing Nothing
    $ Copy "assets/DOInversion.pdf"
  ,Pathed "img/memis.png" "memisScreen" Nothing Nothing
    $ Copy "assets/memis.png"
  ]
  <> fmap fst (hedgefundScreens :: [(Pathed Asset, MarkupT Maybe ())])
  <> engineScreenshots
  <> dissChaps

hedgefundScreens :: Monad m => [(Pathed Asset, MarkupT m ())]
hedgefundScreens = (g *** p) <$>
  [("setup", "The marginal distributions are fitted using the " <> code "R" <> " executable and historic price data.")
  ,("preferences", "The user enters their current portfolio, transaction costs and risk measure and then starts the calculation.")
  ,("optimal_portfolios", "The result is an interactive plot of the efficient frontier. (The discontinuities are due to short selling and borrowing restrictions.)")
  ]
  where g src = Pathed ("img" <> src2) src Nothing Nothing $ Copy $ "assets" <> src2
          where src2 = "/hedgefund/" <> src <.> "jpg"

dissChaps :: [Pathed Asset]
dissChaps = g . show <$>  [1..4]
  where g src = Pathed ("img/diss_johannes_gerer_" <> src <.> "pdf") src Nothing Nothing
          $ Copy $ "assets/diss_johannes_gerer_" <> src <.> "pdf"

engineScreenshots :: [Pathed Asset]
engineScreenshots = g <$>
  ["2003_v1"
  ,"2004"
  ,"2005_1"
  ,"2005_2"
  ,"2006_rotation"
  ,"2007"
  ]
  where g src = Pathed ("img/61engine-" <> src <> ".png") src Nothing Nothing
          $ Copy $ "assets/screenshots/" <> src <> ".png"

  
pages@[about,finance,software,physics] = [
    page "about"    "About me" "" Nothing $ do
      photoUrl <- ("https://johannesgerer.com" </>) <$> path' photo
      jsonLd
        ["@context" .= ("http://schema.org" :: String)
        ,"@type" .= ("Person" :: String)
        ,"name" .= ("Johannes Gerer" :: String)
        ,"url" .= ("https://johannesgerer.com" :: String)
        ,"image" .= photoUrl
        ]
      H.div ! class_ "body" $ do
        floatingImage 2 True "" (Just photoLR) photo $ Just $ mapM_ p
            ["I am a finance scholar, a software engineer and a physicist. My passion is tackling problems that can only be solved with the use of mathematics and/or technology."
            ,do
              text "Recently, I have received a PhD (Dr. rer. pol) in mathematical finance from the University of Regensburg, Germany. "
              more finance
            ,do
              text "Before that, I pursued a Master of Science degree (Diplom) in theoretical particle physics. "
              more physics
            ,do
              text "In addition to my formal education, I have gained significant experience in software development. My research both in finance and physics is computationally challenging. Furthermore, I used my skills as a freelance developer to finance my studies. "
              more software
            ]
        subSect H.h3 "Contact" $ do
          table ! class_ "keyValue" $ mapM_ (tr . mapM_ td) $
            [["Email"
             ,emailAddress
             ]
            ,["Social"
             ,sequence_ $ intersperse " "
               [linkB "https://www.linkedin.com/in/gerer"
                 "LinkedIn"
               ,linkB "https://github.com/johannesgerer/"
                 "Github"
               ,linkB "http://stackoverflow.com/users/578832/johannes-gerer?tab=profile"
                 "StackOverflow"
               ]
             ]
            ,["Residence"
             ,"Munich, Germany"
             ]
            ]
    ,page "finance" "Finance"       "finance/" Nothing $ do
      H.div ! class_ "body" $ do
        p $ do
          text "In November 2016, I received my PhD (Dr. rer. pol.) in mathematical finance summa cum laude from the "
          linkB "http://www.uni-regensburg.de/business-economics-and-management-information-systems/business-dorfleitner/index.html"
            "Department of Finance"
          " at the University of Regensburg, Germany."
        subSect (H.h2 ! dataAttribute "title" "Dissertation")
          (anchor dissA <>
           "Dissertation: Essays on derivatives pricing in incomplete markets") $ do
          mapM_ p
            [do text "My dissertation can be downloaded "
                blank $ namedLink' "here" diss
                " (PDF, 134 Pages, 0.9 MB). It is cumulative and consists of the following four research articles:"
            ,"The first three articles take a theoretical perspective on the pricing of derivatives with embedded decisions and the associated aspect of dynamic hedging. The fourth article is an empirical study on the pricing of exchange-traded commodities (ETCs)."
            ]
        publication 1 "Time consistent pricing of options with embedded decisions"
          ["G. Dorfleitner"]
          ["Under review for publication in ", em "Mathematical Finance"] $ mapM_ p $
          ["Aiming to establish new methods for handling decisions embedded in derivative contracts that help to overcome the shortcomings of existing approaches, this article lays the foundation and derives a pricing principle for options with decisions."
          ,abstract
           ["Many financial contracts are equipped with exercise rights or other features enabling the parties to actively shape the contract's payoff. These decisions pose a great challenge for the pricing and hedging of such contracts. Yet, the literature lacks a consistent way of dealing with these decisions, and instead only provides methods for specific contracts and not transferable to other models."
           ,"In this paper we present a framework that allows us to separate the treatment of the decisions from the pricing problem and derive a general pricing principle for the price of an option with decisions by both parties. To accomplish this we present a general version of the duality between acceptance sets and pricing functions, and use it to translate the pricing problem into the language of acceptance. Expressing certain aspects of economic behavior in this language is sufficient to fully eliminate the decisions from the problem."
           ,"Further, we demonstrate why time consistent pricing functions are crucial when dealing with options with embedded decisions and how the ad-hoc pricing functions used in many contributions can be derived if time consistency is added to our minimal set of assumptions."
           ]
          ]
        publication 2 (anchor amop <> "Optimal discrete hedging of American options")
          ["G. Dorfleitner"]
          ["Accepted for publication in ", em "Review of Derivatives Research"
          , " subject to minor revisions"
          ] $ do
          mapM_ p $
            ["This article extends the above mentioned principle to the problem of realistic hedging and applies it to American options."
            ,"It contains numerical results that have been obtained using my C++ pricing software "
              <> namedLinkF' fipster "fipster" software
              <> "."
            , abstract
              ["In order to solve the problem of optimal discrete hedging of American options, this paper utilizes an integrated approach in which the writer's decisions (including hedging decisions) and the holder's decisions are treated on equal footing. From basic principles expressed in the language of acceptance sets we derive a general pricing and hedging formula and apply it to American options. The result combines the important aspects of the problem into one price. It finds the optimal compromise between risk reduction and transaction costs, i.e. optimally placed rebalancing times. Moreover, it accounts for the interplay between the early exercise and hedging decisions."
              ,"We then perform a numerical calculation to compare the price of an agent who has exponential preferences and uses our method of optimal hedging against a delta hedger. The results show that the optimal hedging strategy is influenced by the early exercise boundary and that the worst case holder behavior for a sub-optimal hedger significantly deviates from the classical Black-Scholes exercise boundary."
              ]
            ]
        publication 3 "A note on utility indifference pricing"
          ["G. Dorfleitner"]
          [linkB "http://www.worldscientific.com/doi/abs/10.1142/S0219024916500370?src=recsys&"
            "Published"
          , em " in the International Journal of Theoretical and Applied Finance"
          ] $ mapM_ p $
          ["The third article addresses problems with many utility functions that are used to derive prices in incomplete markets; problems encountered during the work on the second article. It reveals severe limitations to the practical applicability of two well-established parts of the pricing and hedging literature, namely 'utility indifference pricing' and so-called 'utility-based pricing'."
          ,abstract $ pure $ "Utility-based valuation methods are enjoying growing popularity among researchers as a means to overcome the challenges in contingent claim pricing posed by the many sources of market incompleteness.  However, we show that under the most common utility functions (including CARA and CRRA), any realistic and actually practicable hedging strategy involving a possible short position has infinitely negative utility. We then demonstrate for utility " <> em "indifference prices" <> " (and also for the related so-called " <> em "utility-based (marginal) prices" <> ") how this problem leads to a severe divergence between results obtained under the assumption of continuous trading and realistic results. The combination of continuous trading and common utility functions is thus not justified in these methods, raising the question of whether and how results obtained under such assumptions could be put to real-world use."
          ]
        publication 4 (anchor etcs <> "The pricing efficiency of exchange-traded commodities")
          ["G. Dorfleitner", "A. Gerl"]
          [linkB "http://link.springer.com/article/10.1007/s11846-016-0221-0"
           "Published"
           , em " in Review of Managerial Science"
          ] $ p $
          abstract $ pure "Exchange-traded commodities (ETCs) open the commodity markets to both private and institutional investors. This paper is the first to examine the pricing efficiency and potential determinants of price deviations of this new class of derivatives based on daily data of 237 ETCs traded on the German market from 2006 to 2012. Given the unique size of the sample, we employ the premium/discount analysis, quadratic and linear pricing methods, as well as regression models. We find that the ETCs incur, on average, price deviations in their daily trading and are more likely to trade at a premium from their net asset values than at a discount. In addition, we examine the influence of certain factors such as management fees, commodity sectors, issuers, spread, assets under management, investment strategies, replication and collateralization methods on quadratic and linear price deviations."
        subSect ((anchor pfa <>) . (H.h2 ! dataAttribute "title" ""))
          "Postbank Finance Award 2008" $ do
          p $ do
            text "During my physics studies, I took part in a project together with "
              <> "three finance students to gauge my interest in the field. It won the first prize of the "
            linkB "https://www.postbank.de/postbank/ka_finance_award_2008.html"
              "Postbank Finance Award 2008"
            text " and € 10.000 for the university and has been published in "
            linkB "http://epub.uni-regensburg.de/11449/"
              "Hegde-Fonds – Chancen und Risiken"
            " under the following title:"
          chapter "How can private investors profit from investing in hegde funds?" $ do
            mapM_ p
              ["(original German title: Wie können Privatanleger durch den Einsatz von Hedge-Fonds profitieren?)"
              , sequence_
                ["My contribution consists of a Java program that calculates and shows optimal portfolios under different risk measures—namely Sigma, VaR, CVaR, Shortfall probability—that can be achieved by adding one hedgefund tracking product to a given portfolio. It employs a nonparametric empirical Copula Approach (Williams & Ioannidis 2007) and normal-inverse Gaussian distributions as marginal distributions fitted using "
                ,code "nigFit" <> " from the " <> code "R" <> "-package " <> code "fBasics" <> "."
                ]
              ,"Screenshots:"
              ]
            mapM_ (uncurry (floatingImage 1 True "" Nothing) . second Just) hedgefundScreens
    ,page "software" "Software"       "software/" Nothing $ do
      H.div ! class_ "body" $ mapM_ p 
        ["Writing software is and has always been an important role in my research in physics and finance, in my side job as freelance software engineer and in my private life. I am proficient in the languages listed below."
        ,"I have contributed to a handful of open source projects. "
        <> "Some of my own projects are published under an open source license and presented on this page."
        ]
      sect H.h1 "Haskell" $ do
        mapM_ p $
          [ "After stumbling upon Haskell in 2012 it quickly became my preferred "
            <> "language for everything that cannot be done in a few lines in the shell."
          , sequence_
            ["In my opinion there is no better way to write effectful programs than by using a side effect-free functional language: compose in a pure, mathematical manner a value of type "
            ,code "Program" <> ", which in Haskell is called " <> code "IO ()"
            ,", and pass it to the runtime."]
          ]
        softwareProject "Buchhaltung" (Just "buchhaltung") $ do
          p $ "A set of tools for creating a complete ledger of all your bank and savings accounts', credit cards', and other transactions, in a "
            <> linkB "http://plaintextaccounting.org/" "text-based ledger format"
            <> ". Key features:"
          ul $ mapM_ li
            ["Import transactions via HBCI/OFX interfaces or from CSV files and handle duplicates comfortably."
            ,"Match transactions to accounts with high accuracy through the "
            <> "use of a Bayes classifier."
            ,"Manually enter transactions using an intelligent "
            <> "editor which reduces the required effort to a minimum."
            ]
        softwareProject (anchor lykah <> "Lykah") (Just "lykah") $ p $ mconcat
          ["A static website and blog generator powering "
          ,linkB "http://johannesgerer.com" "johannesgerer.com"
          ,". It allows you to create websites with the "
          ,"full power and expressivity of Haskell. "
          ,"Its name, Lykah, is " <> linkB "https://jaspervdj.be/hakyll/" "Hakyll"
          ," in reverse because it solves the same problem using quite a different approach."]
        softwareProject "BlazeT" (Just "blazet") $ p $ mconcat
          [text "A true monad (transformer) version of the great "
          ,linkB "https://jaspervdj.be/blaze/" "blaze libraries"
          ,". I started this project to be able to make "
          ,namedLinkF' lykah "Lykah" software
          ,"."
          ]
        softwareProject "Doi" (Just "doi") $ mapM_ p $
          ["After I successfully switched my research/citation management from Mendeley to "
           <> linkB "http://joostkremers.github.io/ebib/" "ebib"
           <> " (BibTeX database manager for Emacs), the system still lacked an easy way to download or extract citation information. "
          ,code "doi" <> " fills this gap. Given a DOI or URL, it will generate its BibTeX entry (both from the official source and from Bibsonomy) and can download the fulltext PDF, all automatically."    
          ]
        softwareProject "Google Code Jam" Nothing $
          p $ "This package makes writing concise and fast solutions to Google Code Jam problems easy, including automatic parallelization if possible. The Github "
          <> linkB "https://github.com/johannesgerer/googleCodeJam" "repository"
          <> " also contains my Code Jam submissions."
        softwareProject "Memis – efficient manual image sorting" (Just "memis") $ do
          floatingImage 1 True "halfBorder" Nothing memisScreen Nothing
          p $ "In order to make the paperless office feasable, I needed a fast way to categorize and sort the scanned documents. The existing solutions were not satisfactory, which lead to the creation of Memis."
        softwareProject "Hax" (Just "hax") $ do
          floatingImage 1 True "border" Nothing haxCharts Nothing
          p $ "A library for cash-flow and tax simulations. The type system is used to ensure correctness e.g. of double-entry accounting, and the correct calculation of taxes. The use of Haskell's " <> code "do" <> "-notation and type-classes permit an almost verbatim translation of the tax code into the program."
          p "It currently includes personal income tax (Einkommensteuer), corporate tax (Körperschaftsteuer) and trade/business tax (Gewerbesteuer), but could easily be extended to other legal systems."
      sect H.h1 "C++" $ do
        H.div $ mapM_ p $
          [ "What I appreciate about C++ is its expressiveness, while still being close enough to the machine to enable zero-cost abstractions. This makes it suitable for my use case: numerical software."
          ]
        softwareProject (anchor fipster <> "Fipster") (Just "fipster") $ do
          p $ "Option pricing software for complex financial options on arbitrary dimensional state space using finite differences. "
            <> " It powers the numerical results published in my paper on "
            <> namedLinkF' amop "American options" finance
          ul $ mapM_ li
            ["Expectation based pricing (risk neutral valuation, utility indifference pricing, etc.)"
            ,"Derive optimal/pessimal strategies for decision dependent payoffs (like exercise decisions in American options or optimal hedging decisions)"
            ,"Efficient solver for linear complementary problems"
            ,"Rannacher smoothing"
            ,"Implemented for arbitrary dimensional state space"
            ,"Automatic handling of any combination of boundary conditions"
            ,"Automatic parallelization"
            ]
        softwareProject "Enhance" (Just "enhance") $ do
          mapM_ p 
            ["When writing Fipster (see above) I had to write comparison operator for 20+ classes, which felt very repetitive. Then, I switched from "
             <> code "std::map" <> " to " <> code "std::unordered_map"
             <> ", which instead of comparison operators requires completely analogous hash functions for all classes."
            ,"To spare myself all this boilerplate I wrote an early version of Enhance. Over the years, Enhance was expanded in functionality, polished and published. Now, it can "
            <> quote "enhance" <> " your classes: with a minimum of code and no runtime overhead it will provide you with"
            ]
          ul $ mapM_ li
            ["comparison operators,"
            ,"assignment operators,"
            ,"hash functions,"
            ,"swap functions,"
            ,"serialization,"
            ,"pretty printing,"
            ,"etc."
            ]
        softwareProject "61 Engine" (Just "61-Engine") $ do
          p "The 61 Engine is an OpenGL based 3D (hexadecimal for 61) and basic physics (rigid body and gravitation) engine written for simple games and educational purposes. It was one of my first larger projects, started at age 15 with the goal of writing a more realistic version of GTA3. ;-)" 
          zipWithM_ (floatingImage 0 True "" Nothing) engineScreenshots $ repeat Nothing
      sect H.h1 "Java" $ do
        H.div $ mapM_ p $
          [ do
              quote ("I had a problem so I thought to use Java. Now I have a " <> code "ProblemFactory")
                <> br
              H.span ! class_ "right" $ "—  Cpt. Picard"
          ]
        softwareProject "Portfolio Optimization" Nothing $ p $ 
          "My contribution to the winning entry of the "
          <> namedLinkF' pfa "Postbank Finance Award 2008" finance
          <> "."
        softwareProject "Unicenta POS — keyboard version" (Just "unicenta") $ do
          floatingImage 1 True "" Nothing wincor $ Nothing
          p $ "For a client, I changed the Unicenta point of sale software from "
            <> "touchscreen to an entirely keyboard based user interface. "
            <> "It now supports the typical POS keyboards for highest data input speeds."
      sect H.h1 "Fortran" $ do
        softwareProject "Lattice quantum chromodynamics" Nothing $ p $ do
          "For my " <> namedLink' "physics thesis" physics <> " I extended existing "
          linkB "https://en.wikipedia.org/wiki/Lattice_QCD" "Lattice QCD"
            ! target "_blank"
          "-progams written in Fortran and ran them on a high performance computing cluster."
      sect H.h1 "Miscellaneous" $ do
        softwareProject "Peekattack.com — P2P video chat platform" (Just "peekattack") $
          p "Full stack: Streaming Server in Java, Realtime Push Server in C and JavaScript, Flex/ActionScript client, and of course client-side JavaScript"
        softwareProject "Mathematica" (Just "MathematicaNotes") $
          p "Used for most finance/maths/physics problems during my studies."
        softwareProject "C#" Nothing $
          p "Business software, like client-side data entry of invoices and server-side centralized reports."
        softwareProject "Perl, PHP, SQL and JavaScript" Nothing $
          p "Many years of web developement"
        softwareProject "Python" Nothing $
          p "I use python to hack on open-source code bases of programs that I use myself."
        softwareProject "Linux and Shell" Nothing $
          p "Since 2012, I am an Arch Linux/Xmonad/emacs power user that switched from Windows 7 and never looked back."
        softwareProject "R Statistical Software" Nothing $
          p $ "Used for statistical analysis in my finance research, e.g. for the results of my paper on "
            <> namedLinkF' etcs "ETCs" finance
            <> "."
        -- softwareProject "Emacs Lisp" Nothing $
        --   p "a"
    ,page "physics" "Physics"       "physics/" Nothing $ 
      H.div ! class_ "body" $ do
        p $ do
          text "I graduated with a Master of Science (Diplom-Physiker) from the  "
          linkB "https://www.elitenetzwerk.bayern.de/elitestudiengaenge/elite-graduate-programs-according-to-fields-of-study/physics-with-integrated-doctorate-program/?L=2"
            "elite graduate program in physics "
          " at the University of Regensburg and the Univerity of Erlangen–Nuremberg, Germany. My main field of study was elementary particle physics."
        subSect (H.h2 ! dataAttribute "title" "Thesis")
          "Thesis: Lattice QCD test of the hypothesis of an effective restauration of chiral symmetry for hadron resonances" $ mapM_ p $
          -- "Thesis: Is chiral symmetry effectively restored for hadrons on the lattice?" $ mapM_ p $
          [ do text "The thesis can be downloaded "
               blank $ namedLink' "here" diplom
               " (PDF, 82 Pages, 0.8 MB)."
          ,"To answer the above question, properties of the constituents of the nucleons' parity chiral doublet have to be calculated using lattice quantum chromodynamics (QCD)."
          ,"Lattice QCD calculations require the inversion of a discretized version of the "
            <> "Dirac operator"
            <> "—in my case a matrix with 1.5 million rows—and thus can only be performed on high performance computing clusters."
          ,"I extended preexisting Fortran programs that used MPI for parallelization and ran them on the 3000 core " <> linkB "http://www.uni-regensburg.de/EDV/kurs_info/brf09510/hpc/hpc13.html" "Athene cluster"
            <> " at the University of Regensburg."
          ,"Prior to this thesis I completed the following two introductory projects:"
          ]
        chapter (namedLinkB'
                 "Calculating the tensor charge of the nucleon using lattice QCD"
                 tensor <> br <> " (PDF, 22 pages, 0.3 MB)")
          $ mempty
        chapter (namedLinkB'
                 "Parallel inversion of the dirac matrix" 
                 dirac <> br <> " (PDF, 14 pages, 0.2 MB)")
          $ mempty
    ]

blog :: PathedPage ()
blog = page "blog"     "Blog"           "blog/" Nothing $ postList $ const True

fragments@[pfa,lykah,etcs,fipster,amop,dissA] =
  ["pfa"
  ,"lykah"
  ,"etcs"
  ,"fipster"
  ,"amop"
  ,"diss"
  ]

softwareProject
  :: Monad m
  => MarkupT m () -- ^ title
  -> Maybe AttributeValue -- ^ github repo name
  -> MarkupT m () -- ^ body
  -> MarkupT m ()
softwareProject title repo body = subSect H.h3 title $ do
  body
  maybe mempty
    (\repo' ->
     p $ " More on " <> linkB
       ("https://github.com/johannesgerer/" <> repo') "Github.")
    repo
  
chapter :: Monad m => MarkupT m () -> MarkupT m () -> MarkupT m ()
chapter t b = subSect H.h4 t $ do
  b

myPosts = do
  -- a <- either error id <$> t2
  return
    [ -- a
      post "annBlazeT" "blog/blazet.html" "2016-06-13 20:50 CEST"
      "This is my second blog post" (Just blog)
      $ loremMore 2
    ,post "annfipster2" "blog/ann_fipster.html" "2014-06-13 20:50 CEST"
      "My first post" (Just blog)
      $ loremMore 2
    ]


-- | My email address obfuscated using http://www.mailtoencoder.com/
emailAddress :: Markup
emailAddress = do
  script ! A.type_ "text/javascript" $
    textComment "\nvar fscuanb = ['=','o','>','<','a','o','r','<','g','f','\"','j','l','a','e','e','c','g','m','r','n','/','.','m','>','o','l','a','\"','a','e','c','j','s',' ','h','=','s',':','h','j','@','r','o','r','t',' ','s','a','i','e','h','m','a','\"','s','r','j','e','o','e','@','a','n','n','n','c','e','m','e','i','\"','l','.'];var mvfrrrw = [42,54,73,0,21,33,63,70,61,6,49,51,48,1,44,29,67,26,45,30,22,71,31,69,50,14,38,10,8,46,27,32,18,40,36,55,7,41,15,3,53,52,65,68,28,13,2,25,56,47,62,20,34,39,35,60,4,16,24,19,64,17,72,57,23,58,37,5,9,59,11,43,12,66];var bhufbnr= new Array();for(var i=0;i<mvfrrrw.length;i++){bhufbnr[mvfrrrw[i]] = fscuanb[i]; }for(var i=0;i<bhufbnr.length;i++){document.write(bhufbnr[i]);}\n"
  noscript $ "Please enable JavaScript to see the email address"

-- publication :: Monad m
--             => Int -- ^ essay number
--             -> MarkupT m () -- ^ title
--             -> [T.Text] -- ^ coauthors
--             -> [MarkupT m ()] -- ^ status
--             -> MarkupT m () -- ^ body
--             -> MarkupT m ()
publication n title coauths status body =
  chapter title $ do
  p $ do
    text $ "(with " <> T.intercalate ", " coauths <> ". "
    mconcat status
    ". " <> namedLink' "Download" (dissChaps !! pred n) <> " chapter.)"
  body

more :: IDO b => b -> EHtml a ()
more pId = "See " <> namedLink' "more" pId <> "."

abstract :: Monad m => [MarkupT m ()] -> MarkupT m ()
abstract x = do
  p $ em "Abstract:"
  sequence_ $ intersperse br x
