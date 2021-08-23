# Locked Game Example 1

<img src="LockedGame1.png" alt="Simplest Blocked Case" width="70%" height="70%">

Here's an example of a typical unsolveable card deal:

1. Observe the leftmost tableau column containing two black **6** cards.
1. If they cannot be eventually exposed, then this stops any **<span style="color:red">5</span>** cards from being moved into the tableau. One of them is the top <span style="color:red">5♦️</span> in tableau column 2.
1. The <span style="color:red">5♦️</span> also can't be added to the *foundation* because the <span style="color:red">A♦️</span>, located above the <span style="color:red">5♦️</span> is blocked by the same <span style="color:red">5♦️</span> card.
1. The two black ***6*** cards can only move if the <span style="color:red">9♦️</span> below them in the same column can be moved.
1. This would require building down the <span style="color:red">K♦️</span> in the 7th column to provide the black **10** card for the <span style="color:red">9♦️</span> to be moved to.
1. Unfortunately that would require the two black **J** cards to be faceup.  This won't happen because they are in the same column as the stuck <span style="color:red">5♦️</span>.

Hence, this game is unsolveable.
