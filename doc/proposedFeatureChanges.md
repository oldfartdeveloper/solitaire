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

Specifically, just sort the cards in the waste and display them across 6 rows of 4 items each.  When a card is moved out of the waste,
remove its waste entry.

Here's what needs to be done:

1. DONE: Remove `stock` and dependent variables.
1. DONE: Change waste from `Sp3` to `Splayed`: will have have 4 columns, one for each suit
1. DONE (Uses existing code): Be able to locate card in `waste` from mouse clickdown position.
1. DONE (Uses existing code): Be able to perform move testing and operation.
1. All of this should be similar to present waste operation, just only over 4 columns, one for each suit.
   This should be the easiest to implement.

Hence, the waste contains 4 piles, one for each suit.  Each pile is rendered as an `NS` vector growing down from the top.

#### Steps

Make name changes
| Module     | Var     | Type          | Old Name           | New Name   | Comments                                            |
|:-----------|:--------|:--------------|:-------------------|:-----------|:----------------------------------------------------|
| `CardType` |         | `DisplayMode` | `Sp3`              | *deleted*  | waste will use `Splayed` instead                    |
| `CardType` |         | `PileType`    | `StockP`, `WasteP` | *deleted*  | waste will use `TableP` instead                     |
| `CardType` |         | `Ext`         | `StockX`, `WasteX` | *deleted*  | (4 names required?) waste will use `TableX` instead |
| `Utils`    | `stock` |               |                    | *deleted*  | No longer dealing cards facedown                    |
| `Utils`    | `waste` |               |                    | *modified* | See "Utils waste" subsection below                  |

##### Utils waste

Basically need to configure `splitPlaces` call with 4 lengths reflecting number of clubs, diamonds, hearts, and spades respectively in 

### Other TODO

1.  Consider using keyboard modifier keys to add functionality (such as pressing 'shift' to show available targets for the current terminal position).
