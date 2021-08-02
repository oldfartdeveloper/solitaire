# solitaire.hs

> HINT: branch "work" is currently the branch you should use if you want to work
> with the most recent working version.

<img src="/doc/img/ScottSolitaireScreenShot.png" alt="first attempt at rendering facedown" width="55%" height="55%">

## OldFartDeveloper's Intended Feature Changes

> UNDER CONSTRUCTION! But, you can install this and it is fully running.

@ambuc's original effort is extremely useful as a strong start for what I'm trying to acheive.
My intended changes are chronicled **[here](/doc/proposedFeatureChanges.md)**.

There are the following finished features:

1. The facedown cards in the *tableau* are now showing their values.  They still may not be moved while "facedown".
1. The *stock*/*waste* pair has been simplifed to simply a *waste* since all cards are now displayed simultaniously
   in the *waste* alone. You may move any card in the *waste* to any valid location in the *tableau* or the *foundation*.
1. Since if you reveal all the cards in the *tableau*, this effectively wins the game
   since it is trivial to move all the remaining cards in the *waste* and *tableau* into the *foundation*.  Hence, the game now declares that you win as soon as there
   are no facedown cards in the *tableau*.

## Essay

For more background on the original project by jbuckland, [read the blog
post](https://jbuckland.com/2017/12/02/solitaire.html) he wrote about developing it.

## Prerequisites

If you haven't already, you'll need to install:

- [haskell](https://www.haskell.org/platform/), a standardized, general-purpose
  purely functional programming language, with non-strict semantics and strong
  static typing.  Use the **`ghcup`** procedue mentioned in this link.
- [stack](https://docs.haskellstack.org/en/stable/README/), a cross-platform
  program for developing Haskell projects.

## Playing `solitaire`

You can clone this repo and use `stack` to build and run the executable like so:

```zsh
git clone https://github.com/oldfartdeveloper/solitaire.git
cd solitaire
gco -b work
git pull origin work
stack run
```

You start the game by clicking cards with borders.  (Facedown cards in the *tableau* can not be moved, but their values are shown without card borders; this makes for a more logic-oriented game.)  Furthermore, you don't have to flip 3 cards from the *stock* to the *clone*; all cards not in the *tableau* or the *foundation* are now all displayed in the *waste*, and you can move any of them if the game rules allow it.

The game layout is broken down from left to right:

- The *waste* (there is no *stock*).  You don't to flip cards; you can move any card directly.
- The *tableau* shows the "facedown" cards w/o borders.  You can see their values, but you cannot move them.  You win the game when there are no longer any remaining "facedown" cards in the *tableau*.
- The *foundation* where you build each suit.
- The *information panel* where you can see your score and manipulate the game.

Note: if you find some of the cards on the right don't respond to mousedown, make your terimal window smaller.

## Links

- [brick](https://hackage.haskell.org/package/brick), a Haskell terminal user
  interface programming library.
  - [guide.rst](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst),
    the Brick User Guide
  - [snake](https://samtay.github.io/articles/brick.html), a walkthrough of
    writing a snake game in Brick
- [microlens](https://hackage.haskell.org/package/microlens), a small extract of
  the larger [`Control.Lens`](http://hackage.haskell.org/package/lens) library,
  which implements functional references.
  - [`Control.Lens.Tutorial`](https://hackage.haskell.org/package/lens-tutorial/docs/Control-Lens-Tutorial.html),
    a great tutorial for understanding and using Haskell lenses or microlenses.
