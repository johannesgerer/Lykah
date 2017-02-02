{-# LANGUAGE OverloadedStrings
 #-}
module Lykah.Style where

import qualified Clay as C
import           Clay hiding(table)
import qualified Clay.Media as M
import           Clay.Stylesheet
import           Control.Monad
import           Data.Function hiding ((&))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import           Prelude hiding (div,span)

render' :: Css -> TL.Text
render' = renderWith compact []

pa :: Integer -> Clay.Stylesheet.StyleM ()
pa = (position absolute >>).zIndex
          
myCss :: T.Text
myCss = TL.toStrict $ render myCss'

smallScreen :: Css -> Css
smallScreen = queryOnly M.screen [M.maxWidth $ px threshold]

threshold :: Num a => a
threshold = 480

-- bw = True
bw = False

myCss' :: Clay.Stylesheet.StyleM ()
myCss' = do
  html ? do
    overflowY scroll
  body ? do
    margin nil nil nil nil
    fAbel
    -- color white
    fontSize $ px 16
    fontWeight $ weight 300
  ".clearfix" & "::after" & do
    clear both
    display block
    content $ stringContent ""
  let clf w mw = do
        img ?  do
          width $ px w
          maxWidth $ pct 98
          queryOnly M.screen [M.maxWidth $ px mw] $
            float none
  ".imgP" & do
    ".halfBorder" & img ? do
      borderRight solid (px 1) "#ccc"
      borderBottom solid (px 1) "#ccc"
    ".border" & img ? do
      border solid (px 1) "#ccc"
    ".clearfix" & do
      marginBottom $ px 16
    marginBottom $ px 0
    img ? do
      float floatLeft
      marginRight $ px 20
      marginBottom $ px 10
    ".normal" & clf 300 495
    ".small" & clf 220 415
    ".tiny" & clf 163 265
  ".right" & do
    textAlign $ alignSide sideRight
    marginRight $ px 5
    display block
  ".keyValue" & do
    td ? do
      firstChild & do
        width $ px 104
      verticalAlign vAlignTop
      a ? do
        paddingRight $ px 7
  "#v" & do position fixed
            left nil >> right nil >> top nil
            overflow hidden
            height $ pct 100
            video ? do pa 0
            span ? do pa 1
                      bottom $ px 10
                      right $ px 10
  "#content" & do
    zIndex 1
    -- left $ px 100
    marginTop $ px 100
    smallScreen $ do
      marginTop $ px 0
    marginBottom $ px 30
    -- display table
    marginLeft auto
    marginRight auto
    maxWidth $ px 670
  ".anchor" & do
    position relative
    a ? do
      position absolute
      top $ em $ -2
      visibility hidden
  ".menu" & do
    fontSize $ px 20
    fJul
    marginBottom $ px 4
    let fe x = div ? ".fixedExtra" & x
    fe $ do
      paddingBottom $ px 11
      paddingLeft $ px 9
      display none
      ".title" & do
        cursor pointer
    ".fixed" ? do
      fe $ display block
      (".collapsed" &) $ fe $ display none
    div ? ".collapsedExtra" & do
      float floatLeft
      whiteSpace nowrap
      ".title" & do
        paddingLeft $ px 5
        display none
        float none
        overflow hidden
        textOverflow overflowEllipsis
        cursor pointer
      ".hamburger" & do
        -- visibility hidden
        display none
        span ? do
          -- top $ px $ -2
          -- left $ px $ -15
          padding'' 1 4 16 4
          verticalAlign $ vAlignTop
          fontSize $ px 12
          hover & cursor pointer
    ".menuItem1" ? do
      clear clearLeft
    ".collapsed" ? do
      fontSize $ px 16
      ".menuItem1" ? do
        clear none
    span ?".preload" & do
      position fixed
      left $ px $ -100
      visibility hidden
    ".hidden" <? do
      visibility hidden
      -- a ? fontWeight bold
      -- div <? a ? firstChild & do
      --   fontWeight normal
    a ? ".name" & ".short" <? do
      display none
    ".visible" <? do
      position relative
      div <? do
        boxShadow (px 1) (px 1) (px 2) $ setA 0.85 $ white
        position absolute
        background white
        width $ pct 100
      ".fixed" & do
        zIndex 3
        top $ px 0
        position fixed
        width $ pct 100
        -- left $ px 0
      ".collapsed" & do
        a ? ".name" & do
          marginRight $ px $ -2
          ".long" <? display none
          ".short" <? display block
        a ? ".inactive" & do
          opacity 0
          paddingLeft $ px 0
          paddingRight $ px 0
          marginLeft $ px 0
          marginRight $ px 0
        a ? ".active" & do
          width auto
          marginRight $ px 5
          marginLeft $ px 5
          -- paddingRight $ px 0
        --   borderRight solid (px 1) "#ccc"
        --   content $ stringContent ""
        --   height $ px 50
        --   position relative
        --   left 
        div ? ".collapsedExtra" & do
          ".hamburger" & do
            -- visibility visible
            display block
    div <? div <? do
      paddingTop $ px 11
      a <? do

        queryOnly M.screen [M.maxWidth $ px threshold] $ do
          width $ px 101
          textAlign $ alignSide sideLeft
          ".finance" & do
            width $ px 83
  
          

        width auto
        textAlign center
    
        whiteSpace nowrap
        
        let t = 0.5
            tp = [("width",    t, ease, 0)
                 ,("padding",  t, ease, 0)
                 ,("margin",   t, ease, 0)
                 ,("opacity",  t, easeOut, 0)
                 ]
        transitions tp
        display inline
        float floatLeft
  
        marginRight $ px 14
        marginLeft $ px 9
        paddingLeft $ px 0
        paddingRight $ px 0
        paddingBottom $ px 0
        marginBottom $ px 11

        firstChild & do
          width auto
          transitionDuration 0
          paddingRight $ px 8
          -- this was needed to visually align the first letter when
          -- it was still a 'J'
          -- marginLeft $ px 11
          cursor cursorDefault
          -- background $ t c19
          -- hover & do
          --   fontWeight normal
          fontWeight bold
        hover & do
           fontWeight bold
        ".active" & do
          transitionDuration 0
          fontWeight bold
        "::after" & do
          display block
          content $ attrContent "data-text"
          fontWeight bold;
          height $ px 1
          color transparent
          overflow hidden
          visibility hidden
          marginBottom $ px $ -1
  ".body" & do
    -- textAlign justify
    -- background $ rgba 139 160 68 98
  -- rgba(4, 24, 65, 0.38) -- dunkel
    -- background $ rgba 9 8 5 148
      -- rgba 138 156 73 128
   -- rgba(116, 145, 27, 0.58)
  
    float floatLeft
    -- width $ px 610
    -- height $ px 800
    padding' $ px 11
    p ? marginTop (px 0)
    marginBottom $ px 4
    maxWidth $ px 550
  ".section" & do
    float floatLeft
  "#contentBody" & do
    marginTop $ px 42
    smallScreen $ do
      marginTop $ px 22
    a ? do
      textDecoration underline
    ".title" ? do
      fJul
      -- background $ setA 0.352 $  c263
      clear clearLeft
      marginRight $ px 4
      marginBottom $ px 4
      padding' $ px 11
      -- "word-break" -: "break-all"
      -- width $ px 740
      cursor cursorDefault
        -- textShadow (px $ -2) (px 4) (px 3) "#000000"
      fontWeight normal
    h1 ? ".title" & do
      fontSize $ px 41
      marginTop $ px 0
      maxWidth $ px 590
    h2 ? ".title" & do
      fontSize $ px 32
      marginTop $ px 0
      paddingLeft $ px 0
      maxWidth $ px 590
    h3 ? ".title" & do
      fontSize $ px 23
      marginTop $ px 0
      paddingLeft $ px 0
    h4 ? ".title" & do
      fontSize $ px 19
      marginTop $ px 0
      paddingLeft $ px 0
  ".none" & display none
  ".entry" & do
    paddingRight $ px 40
    float floatLeft
    position relative
    -- ".body" ? do
    ".title" ? do 
      fontSize $ px 35
      paddingBottom $ px 5
    ".single" & ".title" ? do 
      fontSize $ px 45
    ".date" ? do 
      fAbel
      paddingTop $ px 2
      -- pa 2
      fontSize $ px 13
      textAlign end
    
  footer ? do
    fontSize $ px 11
    textAlign center
    paddingTop $ px 40
    clear both
  a ? do
    textDecoration none
    color black

  ".more" ? do
    -- color "#FF7171"
    fontWeight $ weight 700
  

  unless bw $ do
    ".menu" & do
      ".letterSpace" ? do
        letterSpacing $ px 1
    ".menu" & do
      ".menuItem1" ? color
        -- "#1AE800"
        -- "#00c100"
        "#00b922"
      ".menuItem2" ? color
        "#0098FF"
        -- "#0070FF"
      ".menuItem4" ? color
        "#E30053"
        -- "#E8009B"
      ".menuItem3" ? color
        -- "#FFA600"
        "#deb200"
    ".menuFiveElement" & do
      ".menuItem1" ? color
          -- "#007a4b"
        "#00c100"
        --  "#009100"
      ".menuItem2" ? color
        -- "#b500b5"
        "#a100ff"
      ".menuItem3" ? color
        -- "#007faa"
        "#00b7e4"
       -- "#0083a3"
      ".menuItem4" ? color "#e00000"
      ".menuItem5" ? color -- "#d4be00"
                       -- "#eabd07"
                      "#deb200"
        -- "##9f8000"
                    -- "#8d6708"
    -- ".aboutColor" & color (cb 1)
    -- ".financeColor" & color (cb 2)
    -- ".softwareColor" & color (cb 3)
    -- ".physicsColor" & color (cb 4)
    -- ".blogColor" & color (cb 5)
  code ? do
    padding' $ em 0.2
    quad borderRadius $ px 3 
    backgroundColor "#f3f3f3"
    fontSize $ pct 85

t = setA 0.625
shad = textShadow 0 0 (px 2) "#000000"
                 
juliusFace = fontFace $ do
  fontFamily ["Julius"] []
  fontFaceSrc [FontFaceSrcUrl "fonts/JuliusSansOne-Regular.ttf"
               $ Just TrueType
              ,FontFaceSrcLocal "Julius Sans One"
              ,FontFaceSrcLocal "JuliusSansOne-Regular"
              ]

fJul = 
  fontFamily ["Julius Sans One"] [sansSerif] -- google
  -- fontFamily ["Julius"] [sansSerif]
fAbel = fontFamily ["Merriweather"] [serif]

        
main = TL.putStr $ flip TL.snoc '\n' $ render myCss'

padding' = quad padding
quad y x = y x x x x
padding'' a b = on padding px a b `on` px
margin' x = margin x x x x

c1  = "#566f11"
c2  = "#729415"
c3  = "#3a500c"
c4  = "#1f3009"
c5  = "#8aa31b"
c6  = "#9ea856"
c7  = "#7f9b4b"
c8  = "#eff2a8"
c9  = "#f5fbe4"
c10 = "#9a722b"
c11 = "#6d754f"
c12 = "#583410"
c13 = "#dbb964"
c14 = "#e5de76"
c15 = "#afb697"
c16 = "#b9d471"
c17 = "#997b4b"
c18 = "#bfd8a5"
c19 = "#993511"
c20 = "#d86c20"
c201 ="#eb7523"
c21 = "#dbbb94"
c22 = "#d99529"
c23 = "#2f5546"
c24 = "#659488"
c25 = "#cc4444"
c26 = "#88cccc"
c261= "#00cccc"
c262= "#1480d0"
c263= "#0998f8"
 
ca 1 = "#00ff22"
ca 2 = "#0085e8"
ca 3 = "#ae00ff"
ca 4 = "#e82c0c"
ca 5 = "#ffcd00"

cb 1 = "#0cf574" 
cb 2 = "#118ab2" 
cb 3 = "#00ffe7" 
cb 4 = "#ef476f" 
cb 5 = "#ffe716" 


    
pos [t,l,w,h] =  top (px t) >> left (px l) >> width (px w) >> height (px h)
