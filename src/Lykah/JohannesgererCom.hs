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

hedgefundScreens :: Monad m => [(Pathed Asset, MarkupT m ())]
hedgefundScreens = (g *** p) <$>
  [("setup", "The marginal distributions are fitted using the " <> code "R" <> " executable and historic price data.")
  ,("preferences", "The user enters their current portfolio, transaction costs and risk measure and then starts the calculation.")
  ,("optimal_portfolios", "The result is an interactive plot of the efficient frontier. (The discontinuities are due to short selling and borrowing restrictions.)")
  ]
  where g src = Pathed ("img" <> src2) src Nothing Nothing $ Copy $ "assets" <> src2
          where src2 = "/hedgefund/" <> src <.> "jpg"

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
    page "about"    "About me" "" Nothing $
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
               [linkB "https://www.linkedin.com/in/dr-hans-wurscht-8827694/"
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
          "Dissertation: Essays on derivatives pricing in incomplete markets" $ do
          mapM_ p
            [do text "My dissertation can be downloaded "
                blank $ namedLink' "here" diss
                " (PDF, 134 Pages, 0.9 MB). It is cumulative and consists of the following four research articles:"
            ,"The first three articles take a theoretical perspective on the pricing of derivatives with embedded decisions and the associated aspect of dynamic hedging. The fourth article is an empirical study on the pricing of exchange-traded commodities (ETCs)."
            ]
        publication "Time consistent pricing of options with embedded decisions"
          ["G. Dorfleitner"]
          ["Under review for publication in ", em "Mathematical Finance"] $ p
          "Aiming to establish new methods for handling decisions embedded in derivative contracts that help to overcome the shortcomings of existing approaches, this article lays the foundation and derives a pricing principle for options with decisions"
        publication (anchor amop <> "Optimal discrete hedging of American options")
          ["G. Dorfleitner"]
          ["Accepted for publication in ", em "Review of Derivatives Research"
          , " subject to minor revisions"
          ] $ do
          mapM_ p
            ["This article extends the above mentioned principle to the problem of realistic hedging and applies it to American options."
            ,"It contains numerical results that have been obtained using my C++ pricing software "
              <> namedLinkF' fipster "fipster" software
              <> "."
            ]
        publication "A note on utility indifference pricing"
          ["G. Dorfleitner"]
          [linkB "http://www.worldscientific.com/doi/abs/10.1142/S0219024916500370?src=recsys&"
            "Published"
          , em " in the International Journal of Theoretical and Applied Finance"
          ] $ p
            "The third article addresses problems with many utility functions that are used to derive prices in incomplete markets; problems encountered during the work on the second article. It reveals severe limitations to the practical applicability of two well-established parts of the pricing and hedging literature, namely 'utility indifference pricing' and so-called 'utility-based pricing'."
        publication "The pricing efficiency of exchange-traded commodities"
          ["G. Dorfleitner", "A. Gerl"]
          [linkB "http://link.springer.com/article/10.1007/s11846-016-0221-0"
           "Published"
           , em " in Review of Managerial Science"
          ] $ p
            "This article examines daily pricing data of 237 ETCs traded on the German market from 2006 to 2012 using different measures for price deviations and pricing efficiency. It is the first study to systematically explore the pricing efficiency of ETCs and its sample is unique in size and regional focus. It finds that, on average, ETCs trade at a premium over their fair price. Furthermore, nine hypotheses on factors that are expected to influence pricing efficiency are formulated and tested using regression analysis. Statistical evidence is found for seven of the nine hypotheses."
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
          p "Full stack: Streaming Server in Java, Realtime Push Server in C and Javascript, Flex/ActionScript client, and of course client-side Javascript"
        softwareProject "Mathematica" (Just "MathematicaNotes") $
          p "Used for most finance/maths/physics problems during my studies."
        softwareProject "C#" Nothing $
          p "Business software, like client-side data entry of invoices and server-side centralized reports."
        softwareProject "Perl, PHP, SQL and Javascript" Nothing $
          p "Many years of web developement"
        softwareProject "Python" Nothing $
          p "I use python to hack on open-source code bases of programs that I use myself."
        softwareProject "Linux and Shell" Nothing $
          p "Since 2012, I am an Arch Linux/Xmonad/emacs power user that switched from Windows 7 and never looked back."
    ,page "physics" "Physics"       "physics/" Nothing $ 
      H.div ! class_ "body" $ do
        p $ do
          text "I graduated with a Master of Science (Diplom-Physiker) from the  "
          linkB "https://www.elitenetzwerk.bayern.de/elitestudiengaenge/elite-graduate-programs-according-to-fields-of-study/physics-with-integrated-doctorate-program/?L=2"
            "elite graduate program in physics "
          " at the University of Regensburg and the Univerity of Erlangen–Nuremberg, Germany. My main field of study was elementary particle physics."
        subSect (H.h2 ! dataAttribute "title" "Thesis")
          "Thesis: Is chiral symmetry effectively restored for hadrons on the lattice?" $ mapM_ p $
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

fragments@[pfa,lykah,fipster,amop] =
  ["pfa"
  ,"lykah"
  ,"fipster"
  ,"amop"
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
  noscript $ "Please enable Javascript to see the email address"

publication :: Monad m
            => MarkupT m () -- ^ title
            -> [T.Text] -- ^ coauthors
            -> [MarkupT m ()] -- ^ status
            -> MarkupT m () -- ^ body
            -> MarkupT m ()
publication title coauths status body =
  chapter title $ do
  p $ do
    text $ "(with " <> T.intercalate ", " coauths <> ". "
    mconcat status
    ".)"
  body

more :: IDO b => b -> EHtml a ()
more pId = "See " <> namedLink' "more" pId <> "."
