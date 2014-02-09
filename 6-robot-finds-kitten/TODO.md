# DONE

* The kitten has a random location.
* Show yet another object on the screen.
* New object acts exactly like the first non-kitten item.
* One NKI has a random location. &lt;--- Minimal viable product

# WIP

* New object has a different description.

# TODO

* Both NKIs have random locations.
* One NKI has a random description.
* There are three NKIs.
* All NKIs have random descriptions.
* There are a random number of NKIs. &lt;--- Release goal
* Robot has a random starting location.
* Ensure no accidentally overlapping objects.
* You can't move past the top of the screeb.
* You can't move past the left side of the screeb.
* You can't move past the right side of the screeb.
* You can't move past the bottom of the screeb.
* Status message appears at bottom of screen, not just line 25.
* Objects can appear anywhere on the screen, not just 80x25.

# Notes

This round moves us to having gameplay that's actually like Leonard's original version of Robot Finds Kitten, which means that tomorrow I move on to a *different game*. Exciting! It's going to be a challenge, but I also have more time today than I have in a while.

# Plan for this iteration, after last retro

* Narrow changes.
* Decouple your types.
* Omit the inessential from tests.
* Let the tests be your safety net.

# A Design Sketch

## Current design

* User Input -> Commands -> States
* parseInput translates stream of input to stream of commands
* playGame and advance translate commands to new states
* Lots of work maintaining state from one round to the next
* Some commands need to run through multiple states
* Some states don't have the same information as others, which causes pain
* Have to do a lot of manual updating of states

## New design

* User Input -> Commands &lt;- Game Behaviors
* Each game behavior watches the command stream
* Different game behaviors translate to the screen differently
* No more Big Monolithic Game State
* Map doesn't even need to be a behavior, since it's static
* Map *does* need to allow easy querying by position
