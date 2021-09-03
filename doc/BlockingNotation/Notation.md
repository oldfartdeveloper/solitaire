# Blocking Notation

The following notation is used to describe the blocking configuration of a game.

## Card Move Status

During analysis, a card is determined to be moveable, blocked, or undetermined (don't know yet).  Hence:

| **Move Status** | **Description** |
| :-: | --- |
| + | moveable |
| ? | undetermined |
| - | blocked |

> I *think* I can pretend, for purposes for forecasting, that all cards are "faceup" even though they might not be.  I'm looking for strategies to determine whether a card is ultimately moveable, that is, moveable later in the game even if, at the present time, it isn't.  I may change my mind and add *faceup*/*facedown* status here later.

## Card ID

As we've seen thus far: the letter/number followed by its suit.  I.e.:

* <span style="color:red">5❤️</span>
* 6♠️

## Card Position

The card can be in the *waste*, *tableau*, or *foundation* as follows:

### Waste

The card ID followed by the letter "w".  Example: 6♠️w

### Tableau

The card ID followed by the column (index 1) and row (index 1).  Examples:

| **Notation** | **Meaning** |
| --- | --- |
| <span style="color:red">5❤️</span>3,5 | The <span style="color:red">5❤️</span> card is in the *tableau* in the column **3**, row **5**. |
| 8♣️t1,1 | The 8♣️ is in the *tableau*'s upper-left corner. |

### Foundatation

The card ID followed by the letter "f".  Example: A♠️f

## Card's Availability Notation

The above notations are combined to describe a card's moveable status by combination:

1. The card's ID followed by its position
1. Followed by a ':' followed by its first parent

Examples:

| **Notation** | **Meaning** |
| -- | -- |
| A♠️1,7:f+ | The face-up A♠️ at *tableau* position row **7**, column **1** is moveable to the *foundation*. |
| <span style="color:red">2♦️</span>1,6: The <span style="color:red">2♦️</span> at *tableau* column **1**, position row **6** is not moveable because it is **facedown**. |
