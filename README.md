# Lykah  [![Build Status](https://travis-ci.org/johannesgerer/Lykah.svg?branch=master)](https://travis-ci.org/johannesgerer/Lykah) [![Hackage](https://img.shields.io/hackage/v/Lykah.svg)](https://hackage.haskell.org/package/Lykah)

100% native Haskell, static (for now) website and blog generator library.

Build websites in a HTML monad with access to the site's and other context information (powered by [BlazeT](https://github.com/johannesgerer/blazet)).

Among other things, this enables *smart links*. The software will automatically include files that were actually referenced in the HTML; making broken (internal) links impossible.

## Comparison with other projects

In contrast to [yst](https://hackage.haskell.org/package/yst), there is no (yaml) configuration, instead the website is build in actual Haskell, as a value of type [Website](https://hackage.haskell.org/package/Lykah/docs/Lykah-Structure.html#t:Website). 

[hakyll](https://jaspervdj.be/hakyll/) treats the content — be it HTML or any other format — as a blackbox, which leaves a lot of Haskell's potential untapped. Lykah could, however, profit from hakyll's comfortable routing and file copying.

## Current state

It is an experimental state and only powers my own website [johannesgerer.com](http://johannesgerer.com). I would be delighted to hear back from other users.

Currently, it tries to find a good balance between generality and conciseness. Other things, like processing of external (static) files, were of no priority to me, but can be easily accomplished since the website can to anything you can to with Haskell ...

At this stage, code says more than a thousand READMEs, so simply check it out.


## Usage and Example

```
stack build  #only once to install dependencies

stack runghc -- -isrc src/Main
```

This will build the website [johannesgerer.com](http://johannesgerer.com) in the `output` folder.

## Documentation

See source and docs on [Hackage](https://hackage.haskell.org/package/Lykah).



