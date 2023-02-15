module Nanospeak.App

open Browser.Dom
open Nanospeak
open Nanospeak.Core

let demo = Core.vis |> Core.sendMessage "visualize" [Core.sp]

let rec createHtml obj = 
  match unbox obj.Class.Value.Slots.["name"].Value.Value with
  | "String" ->       
      document.createTextNode(unbox obj.Value.Value) : Browser.Types.Node
  | "Html" ->
      let tag = obj.Slots.["tag"].Value.Value :?> string
      let res = document.createElement(tag)
      for ch in obj.Slots.["children"].Slots.Values do
        res.appendChild(createHtml ch) |> ignore
      res
  | className -> 
      failwithf "createHtml: Unexpected class '%s'" className

createHtml demo
|> document.getElementById("out").appendChild
|> ignore