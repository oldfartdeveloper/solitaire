# Proposed Feature Changes TODO

## First generation of changes

### Have stock turn over 1 card instead of 3

This will bring this game into closer alignment with my objectives of teaching this program how to play Solitaire w/o having to worry about the 3-card turn.

* DONE! This will bring this game into closer alignment with my objectives of teaching this program how to play Solitaire w/o having to worry about the 3-card turn.
* Ultimately, I simply want to display all the of the cards in the *waste*, and not have a *stock* at all.

### Show unrevealed cards in the tableau

STARTED 2021-7-2: They should show their values but also reveal that they cannot be accessed.

After many false starts, this is where I am now:

<img src="/doc/img/oldFacedown.jpg" alt="first attempt at rendering facedown" width="30%" height="30%">

The above doesn't set off the face-up vs face-down status of the cards in the tableau very well.  My next try will be to remove any borders from the hidden cards altogether; see how that looks.

#### Better Look for Tableau:

Check this out:

<img src="/doc/img/newFaceDown.png" alt="better attempt at rendering facedown" width="30%" height="30%">

## Second generation of changes

### Display all the cards in the waste as wished for above

Specifically, just sort the cards in the waste and display them across 4 columns each, one for each card suit.  When a card is moved out of the waste, remove its waste entry.  There is no further need for the *stock*, as all cards are always visible simultaneously.

### Internal Changes

Internally, Here's what needs to be done:

1. DONE: Remove `stock` and dependent variables.
1. DONE: Change waste from `Sp3` to `Splayed`: will have have 4 columns, one for each suit
1. DONE (Uses existing code): Be able to locate card in `waste` from mouse clickdown position.
1. DONE (Uses existing code): Be able to perform move testing and operation.

Hence, the waste contains 4 piles, one for each suit.  Each pile is rendered as an `NS` vector growing down from the top.

### Remaining Waste Bugs

1. FIXED: Clicking Ace in *waste* that is not at the bottom does not move to the *foundation*. Fixed by after sorting, Ace is always
   at the bottom.
1. DONE: Need to sort *waste* columns.
1. FIXED: Clicking a moveable card in the *waste* that is not at the bottom of its column correctly moves the card but removes the card at the bottom.  Hence, the moveable card is both in the *waste* and the *tableau*.  Am guessing that this is a missing feature in the Splayed
feature: doesn't assume that clicking the middle of the tableau ever wants to move just the card clicked on.  I think I need a new
`DisplayMode`.
1. FIXED: In tableau, when King and card below it were moved to empty tableau column, the card below was removed.

## Other Desired Features

### ~~Consider Using Keyboard Modifier Keys~~

~~Consider using keyboard modifier keys to add functionality (such as pressing 'shift' to show available targets for the current terminal position).~~ *Okay, I've considered; bad idea for touchpad devices like smart phones.*

### Attempt to improve Haskell integration with VSCode

Although I have all these Haskell plug-ins installed, much of the functionality doesn't seem to be working.  Would be worth a go-around to try to make it better.

Also would be good to write up.

### Update Naming

Although I appreciate the original author's coding skill and design, I don't agree w/ his cryptic naming, especially because I think it makes comprehension more challenging for newbie coders.

Also, I think he truncated name lengths to make code formatting line up.

### Upon randomizing the deal, immediately save it

...and have a process for launching Solitaire with that deal.  Reason: We will probably want to repeat the game for any number of reasons:

* testing
* analysis
* scoring difficulty.
* distribute from web site when starting games.

Which leads to...

#### Dump undo history to obtain solution

In order to be complete, we may have to dump it as a "redo" history.  I.e. add redo.  Actually, the original author assembles this for *logging*; I should leverage what he's already done.

### Announce game finished when no facedown cards in Tableau are left

This is DONE!

Which leaves a fun idea:

#### Have button animate moving cards to Foundation

Go wild with this one:

1. Animate movement of cards like artillery.
1. Animate explosions when cards land on their positions in the *Foundation*.
1. Have broken pieces of anything fly out from the *Foundation*.

### Tactics

Define tactics as intelligent key sequences to move cards around.  This is a primary strategy to have the game automate intelligent game playing.

An idea: users will eventually start using tactics.  When a user manually uses a particular tactic, ask the user whether he'd like the intelligent command for the tactic as a shortcut.


