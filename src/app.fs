module Nanospeak.App

open Browser.Dom
open Nanospeak
open Nanospeak.Core

let rec createHtml obj = 
  match unbox obj.Class.Value.Slots.["name"].Value.Value with
  | "String" ->       
      document.createTextNode(unbox obj.Value.Value) : Browser.Types.Node
  | "Html" ->
      let tag = obj.Slots.["tag"].Value.Value :?> string
      let res = document.createElement(tag)
      for attr in obj.Slots.["attributes"].Slots do
        let typ = attr.Value.Class.Value.Slots.["name"].Value.Value
        if typ = "Closure" then
          if attr.Key = "enter" then
            res.addEventListener("keyup", fun e -> 
              if (e :?> Browser.Types.KeyboardEvent).key = "Enter" then
                attr.Value |> sendMessage "value" [makeString (res :?> Browser.Types.HTMLInputElement).value] |> ignore
            )
          else
            res.addEventListener(attr.Key, fun _ -> 
              attr.Value |> sendMessage "value" [] |> ignore
            )
        else
          res.setAttribute(attr.Key, unbox attr.Value.Value.Value)
      for ch in obj.Slots.["children"].Slots.Values do
        res.appendChild(createHtml ch) |> ignore
      res
  | className -> 
      failwithf "createHtml: Unexpected class '%s'" className

let mutable objects = []

let update () = 
  let el = document.getElementById("out")
  while el.children.length > 0 do el.removeChild(el.children.[0]) |> ignore
  for o in objects do
    let vis = o |> sendMessage "visualizer" []
    vis |> sendMessage "visualize" [o] |> createHtml |> el.appendChild |> ignore

let openObject o = 
  objects <- objects @ [ o ]
  update () 

Visualizer |> addMethod "update" (newNativeMethod (fun _ _ -> 
  update ()
  makeObject Null []
)) |> ignore

Visualizer |> addMethod "open" (newNativeMethod (fun _ (L1(obj)) ->
  openObject obj
  makeObject Null []
)) |> ignore

//openObject visualize
//workspace |> sendMessage "set_code" [ (local("workspace")?print([str "Hello world!"])) ] |> ignore

//openObject sp
//openObject ov
//openObject (local("workspace")?print([str "Hello world!"]))
openObject sp
openObject workspace
openObject visualize