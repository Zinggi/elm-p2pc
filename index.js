import Peer from "peerjs";
import { Elm } from "./dist/elm.js";

const app = Elm.Main.init({
  node: document.getElementById("main")
});

const peer = new Peer({ debug: 2 });

peer.on("open", id => {
  console.log("open", id);
  app.ports.input.send({ type: "GotId", id });
});

peer.on("connection", conn => {
  console.log("connection", conn);
  window.conn = conn;
  window.conn.on("data", data => {
    console.log("Received", data);
    app.ports.input.send({ type: "GotMsg", data });
  });
  setTimeout(() => conn.send({ type: "Ready" }), 1000);
});

app.ports.output.subscribe(data => {
  console.log("elm data", data);
  if (data.type === "Connect") {
    window.conn = peer.connect(data.otherId);
    window.conn.on("data", data => {
      console.log("Received", data);
      app.ports.input.send({ type: "GotMsg", data });
    });
    // Send messages
    // setTimeout(() => conn.send("Hello!"), 1000);
  } else if (data.type === "Send") {
    window.conn.send(data.data);
  }
  // app.ports.input.send(42);
});

peer.on("disconnected", conn => {
  console.log("disconnected", conn);
});

peer.on("close", conn => {
  console.log("close", conn);
});

peer.on("error", error => {
  console.log("error", error);
});
