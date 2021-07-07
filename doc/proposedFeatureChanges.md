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

<img src="/doc/img/newFacedown.png" alt="better attempt at rendering facedown" width="30%" height="30%">

## Second generation of changes

### Display all the cards in the waste as wished for above

Specifically, I'd like to have a 4-column grid at the left:

* The 4 suites are displayed across.
* The 13 ranks are displayed vertically.
* Each card in the waste is listed in its place in the grid.
* A blank is left when the corresponding card is not in the waste.
* I'm considering an optimization where ranks that have no cards in them are collapsed.  Perhaps could be enabled by a flag.

### Other TODO
