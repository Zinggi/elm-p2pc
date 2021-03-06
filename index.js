import Peer from "simple-peer";
import { Elm } from "./dist/elm.js";

const app = Elm.Main.init({
  node: document.getElementById("main")
});

const peerInit = new Peer({ initiator: true });
const peerListen = new Peer({ initiator: false });
let peer = peerInit;

app.ports.output.subscribe(data => {
  if (data.type === "Connect") {
    try {
      const signals = JSON.parse(data.otherOffer);
      signals.forEach(signal => {
        if (signal.type === "offer") {
          peer = peerListen;
        } else if (signal.type === "answer") {
          peer = peerInit;
        }
        peer.signal(signal);
      });
    } catch (error) {}
  } else if (data.type === "Send") {
    peer.send(JSON.stringify(data.data));
  }
});

let offers = [];
let isOffers = true;
[peerListen, peerInit].forEach(p => {
  p.on("data", data => {
    const d = JSON.parse(data);
    console.log("on data", d);
    app.ports.input.send({ type: "GotMsg", data: d });
  });
  p.on("signal", data => {
    console.log("SIGNAL", data);
    if (data.type === "answer") {
      offers = [];
      isOffers = false;
    }
    offers.push(data);
    if (isOffers) {
      app.ports.input.send({ type: "GotOffer", offer: JSON.stringify(offers) });
    } else {
      app.ports.input.send({
        type: "GotAnswer",
        answer: JSON.stringify(offers)
      });
    }
  });
  p.on("connect", () => {
    console.log("connect");
    if (peer === peerInit) {
      peer.send(JSON.stringify({ type: "Ready" }));
    }
  });
  p.on("error", err => {
    console.log("error", err);
  });
});
