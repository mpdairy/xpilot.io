# xpilot.io

a simple xpilot-like game that runs in the browser, written in Elm.

You can play it at [xpilot.io](http://xpilot.io)

It was fun trying to make this game in Elm, but I definitely think a
different programming language would be a better choice. I was trying
to make layers of abstractions that could be reused later for other
games, but every layer deeper it got harder to do things like get
random numbers or manage local state. Even sending back a message that
said a ship was dead was fairly difficult. Also, because all the game
objects are stored in a list that changes every frame, the garbage
collector has to do a lot of work and makes gameplay choppy sometimes.

So if I were to make it again, I'd probably use PureScript or GHCJS
and use some mutable state to save the game objects to prevent as much
garbage collection. Also I'd give the game objects and AIs much
greater Eff/IO access to random numbers and whatnot.


