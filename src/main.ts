// @ts-ignore
import { Elm } from "./app/GameOfLife.elm";
import "./style.css";

Elm.GameOfLife.init({
  node: document.getElementById("app")
});
