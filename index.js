import Peer from "peerjs";
import { Elm } from "./dist/elm.js";

// const base_id = "ZGe07Cx9SJinbtV5tw8JN++EBwHPC7Z8n0youC5v8pA=";
// const my_id = "" + Math.round(Math.random() * 10);

const app = Elm.Main.init({
  node: document.getElementById("main")
  // flags: my_id
});

// console.log("my id:", my_id);

const peer = new Peer({ debug: 2 });

// debugger;
// const conn = peer.connect(base_id + "3");
// conn.on("open", function() {
//   conn.send("hi!");
// });

peer.on("open", id => {
  app.ports.input.send({ type: "GotId", id });
  console.log("open", id);
});
peer.on("connection", conn => {
  console.log("connection", conn);
  window.conn = conn;
  window.conn.on("data", data => {
    console.log("Received", data);
    app.ports.input.send({ type: "GotMsg", data });
  });
  // Got paired
  setTimeout(() => conn.send({ type: "Ready" }), 1000);
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

// let conn = null;

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
