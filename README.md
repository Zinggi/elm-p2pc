# Peer to Peer Chess

This is a project that was created on a "hackday".
The version here has been cleaned up a bit since then.

## Demo

https://zinggi.github.io/randomDemos/p2pc/

Copy the offer and send it (e.g. via email) to the one you want to challenge to a round of chess.  
They will have to paste that in and they will get an answer back. This answer has to be sent back to you.  
Paste the answer in and a peer to peer connection is established and the game begins.

To simulate this locally, you can open two tabs (this only worked with chrome for me)

## Credits

The chess game itself is implemented via [romstad/elm-chess](https://package.elm-lang.org/packages/romstad/elm-chess/latest/), thanks!
To make WebRTC easier, I've used [simple-peer](https://github.com/feross/simple-peer), thanks!
For an earlier version I've used [peerJS](https://peerjs.com/).
