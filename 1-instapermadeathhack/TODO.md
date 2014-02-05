# DONE

* Run app and quit without errors.
* Print an at sign. &lt;--- welp, guess that was the release!

# Retrospective:

As I half expected, I stalled out here on researching curses libraries. I
think I found one to try out, though we'll see how it goes in practice in
the next round.

## What went well?

Tiny stories helped me make any progress at all getting something up and
running.

## What went badly?

I'm not gonna lie, this is a pretty shit game. Or rather, it's more of a
bad joke than a game. Also didn't have enough energy after work to get
anywhere. That's going to be a big risk today, since I'm on a plane to
Seattle and will probably get in pretty late. So, goal one for the next
couple days is going to be finding the time and energy to actually do
shit.

## So what's the game?

Allow me to present, the world's simplest possible roguelike:
**Instapermadeathhack!**

It implements all three of the core gameplay mechanics for a roguelike
game:

* Permadeath: Once your character dies (in the first move of the game)
  there is no way to continue.
* Turn-based: Unfortunately, since your character dies in the first turn,
  you will never arrive at the second.
* Procedural level generation: Fortunately, since the level is only 1x1,
  and your character obscures that square of the map, the level generation
  procedure is rather simple.
