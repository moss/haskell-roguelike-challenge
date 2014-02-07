# DONE

* Find a kitten and game ends.
* When you find the kitten it shows a happy message.

# WIP


# TODO

* Show another more different object.
* Can't walk through object.
* Says something when you interact with object (by walking at it).
* Show two objects.
* Random representation of objects.
* Random description of NKOs.
* Random positions of objects.
* Randomly generate a bunch of objects at the start.
* You can't move off any of the edged.
* You're a number sign, not an at sign.

# Notes

This round I'm building a Robot Finds Kitten clone.

I'm also toying with starting to do some testing.

# Retrospective:

**Goal for this release:** Robot Finds Kitten, at some parity with the original.

**Released:** Robot Finds A Kitten That Was Not Actually Hiding.

All in all, I consider it a pretty big success. I knew going in that
finding a kitten, and that managing the complexity therein, was going to
be the big challenge for today. I think I ended up with code that does an
okay job of managing that complexity, though I'm still not sure how well
it will scale further.

I finally added testing this round: I got to the point where feedback on
something happening at the code level was going to be _much more useful_
than feedback on observed program behavior, and where feedback from
program behavior was getting _much slower_ than unit test feedback. I'm
glad I put it off as long as I did, and I fully intend to TDD new stuff
now, at least as far as it makes sense to. (Randomness is coming soon, and
will add some challenges).

One thing I want to try to start doing tomorrow is aiming for smaller
stories and smaller releases. I've got two or three days of solid feedback
that I'm aiming for too-big releases, and most releases only end up with a
story or two done. I suspect I could split smaller, and I'd like to try.

One thing I'd like to keep doing is not thinking too far ahead. After
losing a bunch of time the first day researching ncurses libraries, I
ended up choosing something simpler that still met my needs. Today, I took
the hint, and avoided digging into FRP libraries when I felt tempted.

One thing I'd lie to stop doing is writing code without a test to prompt
it. This may prove to be a challenge in some places, but overall I think
it will encourage me to have a better sense of what I'm aiming to
accomplish in each step.
