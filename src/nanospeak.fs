#if INTERACTIVE
#else
module Nanospeak.Core
#endif

let (|L0|) = function [] -> () | _ -> failwith "L0: Expected empty list"
let (|L1|) = function [v] -> v | _ -> failwith "L1: Expected list with 1 value"
let (|L2|) = function [v1;v2] -> v1,v2 | _ -> failwith "L2: Expected list with 2 values"


[<ReferenceEquality>]
type Objekt = 
  { Class : Objekt option
    Value : obj option
    mutable Slots : Map<string, Objekt> }

type NativeFunc = Objekt -> Objekt list -> Objekt

// TODO: This is probably wrong (depends on the language we are 
// modelling? but I'm pretty sure it is wrong for all of them...)
let Top = { Class = None; Value = None; Slots = Map.empty }
let Metaclass = { Class = None; Value = None; Slots = Map.empty }
let Class = { Class = Some Metaclass; Value = None; Slots = Map.empty }

let String = { Class = Some Class; Value = None; Slots = Map.empty }
let NativeObj = { Class = Some Class; Value = None; Slots = Map.empty }
let Array = { Class = Some Class; Value = None; Slots = Map.empty }
let Lookup = { Class = Some Class; Value = None; Slots = Map.empty }

let makeNativeObj o = 
  { Class = Some NativeObj; Value = Some o; Slots = Map.empty }
let makeString (s:string) = 
  { Class = Some String; Value = Some s; Slots = Map.empty }
let makeObject cls slots = 
  { Class = Some cls; Slots = Map.ofList slots; Value = None }

let makeClass name parent =
  makeObject Class [ 
    "parent", parent
    "name", makeString name
    "methods", makeObject Lookup [ ]
  ]

let Object = makeClass "Object" Top
let Null = makeClass "Null" Object
let Method = makeClass "Method" Object
let NativeMethod = makeClass "NativeMethod" Object

String.Slots <- (makeClass "String" Object).Slots
NativeObj.Slots <- (makeClass "NativeObj" Object).Slots
Array.Slots <- (makeClass "Array" Object).Slots
Lookup.Slots <- (makeClass "Lookup" Object).Slots
Metaclass.Slots <- (makeClass "Metaclass" Object).Slots
Class.Slots <- (makeClass "Class" Object).Slots

// let withSlot name value obj =
//   { obj with Slots = obj.Slots.Add(name, value) }

let arrayValues arr = 
  [ for kvp in arr.Slots -> int kvp.Key, kvp.Value ]
  |> List.sortBy fst
  |> List.map snd

let newMethod args code = 
  let args = makeObject Array [ for i, s in Seq.indexed args -> string i, makeString s ]
  makeObject Method [ "code", code; "args", args ]

let newNativeMethod (f:NativeFunc) = 
  makeObject NativeMethod [ "func", makeNativeObj f ]

let addMethod name m cls =
  if cls.Class <> Some Class && cls <> Class then 
    failwith "addMethod: Cannot add method to non-class"
  let methods = cls.Slots.["methods"]
  let newMethods = { methods with Slots = methods.Slots.Add(name, m) }
  cls.Slots <- cls.Slots.Add("methods", newMethods)
  cls

let addSlot name cls = 
  cls 
  |> addMethod ("set_" + name) (newNativeMethod (fun inst (L1(arg)) ->
    inst.Slots <- inst.Slots.Add(name, arg) 
    makeObject Null [] )) 
  |> addMethod ("get_" + name) (newNativeMethod (fun inst (L0()) ->
    inst.Slots.[name] )) 

//let Lookup = makeClass "Looup" Object
//let lookupExpr = makeObject 

Class
  |> addSlot "parent"
  |> addSlot "name"
  |> addSlot "methods"
  |> addMethod "new" (newNativeMethod (fun inst _ ->
    makeObject inst []
  ))
  |> ignore

let ObjectMirror = 
  makeClass "ObjectMirror" Object
  |> addSlot "object"
  |> addMethod "getClass" (newNativeMethod (fun inst (L0()) -> 
      inst.Slots["object"].Class.Value
  ))

let ClassMirror = 
  makeClass "ClassMirror" Object
  |> addSlot "class"
  |> addMethod "addSlot" (newNativeMethod (fun inst (L1(name)) -> 
      if name.Class <> Some String then failwith "ClassMirror.addSlot: 1st arg not a string"
      let cls = inst.Slots.["class"]
      cls |> addSlot (unbox name.Value.Value) |> ignore
      makeObject Null []
  ))
  |> addMethod "addMethod" (newNativeMethod (fun inst (L2(name, meth)) -> 
      if name.Class <> Some String then failwith "ClassMirror.addMethod: 1st arg not a string"
      if meth.Class <> Some Method && meth.Class <> Some NativeMethod then failwith "ClassMirror.addMethod: 2nd arg not a method or native method"
      let cls = inst.Slots.["class"]
      cls |> addMethod (unbox name.Value.Value) meth |> ignore
      makeObject Null []
  ))
  |> addMethod "getMethods" (newNativeMethod (fun inst (L0()) -> 
      let cls = inst.Slots.["class"]
      cls.Slots.["methods"]
  ))
  |> addMethod "getName" (newNativeMethod (fun inst (L0()) -> 
      let cls = inst.Slots.["class"]
      cls.Slots.["name"]
  ))

let Platform = 
  makeClass "Platform" Object
  |> addMethod "reflectClass" (newNativeMethod (fun _ (L1(cls)) -> 
      if cls.Class <> Some Class then failwith "Platform.reflectClass: 1st arg not a class"
      makeObject ClassMirror [ "class", cls ]
  ))
  |> addMethod "reflectObject" (newNativeMethod (fun _ (L1(obj)) -> 
      makeObject ObjectMirror [ "object", obj ]
  ))
  |> addMethod "newClass" (newNativeMethod (fun _ (L2(name, parent)) -> 
      if name.Class <> Some String then failwith "Platform.newClass: 1st arg not a string"
      if parent.Class <> Some Class then failwith "Platform.newClass: 2nd arg not a class"
      makeClass (unbox name.Value.Value) parent
  ))
  |> addMethod "getClass" (newNativeMethod (fun _ (L1(name)) -> 
      if name.Class <> Some String then failwith "Platform.getClass: 1st arg not a string"
      if name.Value.Value = "Object" then Object
      else failwithf "Platform.getClass: Unknown class: %A" name.Value.Value
  ))
  |> addMethod "print" (newNativeMethod (fun _ (L1(msg)) -> 
      if msg.Class <> Some String then failwith "Platform.print: 1st arg not a string"
      printfn "%s" (unbox msg.Value.Value)
      makeObject Null []
  ))
  |> addSlot "activation"

let Activation = 
  makeClass "Activation" Object
  |> addSlot "self"
  |> addSlot "locals"
  |> addSlot "prev"

let Locals = 
  makeClass "Locals" Object

let p = makeObject Platform [
  "activation", makeObject Activation [ 
    "self", makeObject Null []
    "locals", makeObject Locals [] 
  ]
]

let rec withActivation selfOpt locals f = 
  let prevAct = p |> sendMessage "get_activation" []
  let self = defaultArg selfOpt (prevAct.Slots.["self"])
  // TODO: Should we create anonymous class inherited from Activation with corresponding slots?
  let locals = makeObject Locals locals
  let act = makeObject Activation ["prev", prevAct; "self", self; "locals", locals]
  p |> sendMessage "set_activation" [act] |> ignore
  let res = f () 
  let currAct = p |> sendMessage "get_activation" []
  let prevAct = currAct |> sendMessage "get_prev" []
  p |> sendMessage "set_activation" [prevAct] |> ignore
  res 

and sendMessage msg args obj = 
  let rec loop cls = 
    match cls with 
    | None -> failwithf "sendMessage: sending '%A' to Top or Metaclass failed" msg
    | Some cls -> 
        if cls = Top then failwithf "doesNotUnderstand '%s'" msg 
        printfn "Looking for '%s' in %A" msg cls.Slots.["name"].Value.Value
        match cls.Slots.["methods"].Slots.TryFind msg with 
        | Some meth -> 
            if meth.Class = Some Method then
              let vars = arrayValues meth.Slots.["args"] |> List.map (fun v -> v.Value.Value :?> string)
              if vars.Length <> args.Length then failwith "sendMessage: Number of arguments did not match"
              let varArgs = List.zip vars args
              withActivation (Some obj) varArgs (fun () ->
                  meth.Slots.["code"] |> sendMessage "eval" [] )
            elif meth.Class = Some NativeMethod then
              (meth.Slots.["func"].Value.Value :?> NativeFunc) obj args
            else 
              failwith "sendMessage: Neither Method nor NativeMethod"
        | None -> loop (cls.Slots.TryFind "parent")
  loop obj.Class

Lookup  
  |> addMethod "map" (newNativeMethod (fun inst (L1(f)) ->
    makeObject Lookup [
      for kvp in inst.Slots ->
        kvp.Key, f |> sendMessage "value" [makeString kvp.Key; kvp.Value]
    ]
  ))
  |> ignore

let Closure = 
  makeClass "Closure" Object
  |> addSlot "arg"
  |> addSlot "body"
  |> addMethod "value" (newNativeMethod (fun inst args ->
      let vars = arrayValues inst.Slots.["args"] |> List.map (fun v -> v.Value.Value :?> string)
      let body = inst.Slots.["body"]
      if vars.Length <> args.Length then failwith "Closure.value: Number of arguments did not match"
      let varArgs = List.zip vars args
      withActivation None varArgs (fun () -> 
        body |> sendMessage "eval" [])
  ))

let Expr = 
  makeClass "Expr" Object 
  |> addMethod "eval" (newNativeMethod (fun _ (L0()) -> 
      failwithf "Expr.eval: Abstract method not implemented here"))
let SeqExpr = 
  makeClass "SeqExpr" Expr |> addSlot "e1" |> addSlot "e2"
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      inst.Slots.["e1"] |> sendMessage "eval" [] |> ignore
      inst.Slots.["e2"] |> sendMessage "eval" []
  ))
let ArrayExpr = 
  makeClass "ArrayExpr" Expr 
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      makeObject Lookup 
        [ for kv in inst.Slots -> 
            kv.Key, sendMessage "eval" [] kv.Value ]
  ))
let SelfExpr = 
  makeClass "SelfExpr" Expr
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      let act = p |> sendMessage "get_activation" []
      act |> sendMessage "get_self" []
  ))
let StringExpr = 
  makeClass "StringExpr" Expr |> addSlot "value"
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      inst.Slots.["value"]
  ))
let LocalExpr = 
  makeClass "LocalExpr" Expr |> addSlot "name"
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      let name = inst.Slots.["name"].Value.Value :?> string
      let rec loop act = 
        match act.Slots.["locals"].Slots.TryFind name with 
        | Some value -> value
        | None -> 
            match act.Slots.TryFind "prev" with
            | Some prevAct -> loop prevAct
            | _ -> failwithf "LocalExpr.eval: Did not find '%s' in current activation" name
      let act = p |> sendMessage "get_activation" []
      loop act
  ))
let SendExpr = 
  makeClass "SendExpr" Expr |> addSlot "receiver" |> addSlot "name" |> addSlot "args"
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      //printfn "EVALING SEND"
      let recv = inst.Slots.["receiver"] |> sendMessage "eval" []
      let name = inst.Slots.["name"] |> sendMessage "eval" []
      // TODO: This should send 'forEach' to the array?
      let args = arrayValues inst.Slots.["args"] |> List.map (sendMessage "eval" [] )  
      if name.Class <> Some String then failwith "SendExpr.eval: name is not string"
      //printfn "SENDING %s" (unbox name.Value.Value) 
      sendMessage (unbox name.Value.Value) args recv
  ))
let ClosureExpr = 
  makeClass "ClosureExpr" Expr |> addSlot "args" |> addSlot "body"
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      makeObject Closure ["args", inst.Slots.["args"]; "body", inst.Slots.["body"]] 
  ))

// --------------------------------------------------------------------------------------
// DSL for writing expressions & some samples
// --------------------------------------------------------------------------------------

let (?) r n args = 
  makeObject SendExpr [ 
    "receiver", r
    "name", makeObject StringExpr ["value", makeString n]
    "args", makeObject Array [
      for i, a in Seq.indexed args -> string i, a ]
  ]
let (<.>) e1 e2 = makeObject SeqExpr ["e1",e1; "e2",e2] 
let self = makeObject SelfExpr [] 
let local s = makeObject LocalExpr ["name", makeString s]
let str s = makeObject StringExpr ["value", makeString s]
let fnn ss body = 
  let args = [ for i, s in Seq.indexed ss -> string i, makeString s ]
  makeObject ClosureExpr ["args", makeObject Array args; "body", body]
let fn s body = fnn [s] body
let arr els = makeObject ArrayExpr [for i, el in Seq.indexed els -> string i, el ]

let hello = 
  (self?get_p [])?print [str "Hello world!!"]

let printSelf =
  let om = self?get_p([])?reflectObject [self]
  let cl = om?getClass []
  let cm = self?get_p([])?reflectClass [cl]
  cm?getMethods([])?map [fnn ["name";"method"] (
    self?get_p([])?print [local "name"]
  )]

  //(str "Hello world!")?``do`` [
  //]
  
  //
  //(fn "s" (
  //    (self?get_p [])?print [local "s"]
  //  ))?value [str "Test"]

// TODO: Classes should not come from the global namespace
// In reality, we should get them via some lookup, but I don't know how yet...
// TODO: getClass should be replaced with better class lookup mechanism

let Obj = p |> sendMessage "getClass" [makeString "Object"]
let SelfPrinting = p |> sendMessage "newClass" [makeString "SelfPrinting"; Obj]
let spMirror = p |> sendMessage "reflectClass" [SelfPrinting]
spMirror |> sendMessage "addMethod" [makeString "printSelf"; newMethod [] printSelf] |> ignore
spMirror |> sendMessage "addMethod" [makeString "helloWorld"; newMethod [] hello] |> ignore
spMirror |> sendMessage "addSlot" [makeString "p"] |> ignore
let sp = SelfPrinting |> sendMessage "new" []
sp |> sendMessage "set_p" [p] |> ignore

let tests () =
  sp |> sendMessage "helloWorld" [] |> ignore
  sp |> sendMessage "printSelf" [] |> ignore

  let x = sp |> sendMessage "printSelf" [] 

  x.Slots.Keys
  |> ignore

  x.Class.Value.Slots.["name"].Value.Value
  |> ignore
  
  x.Slots.["name"].Value.Value
  |> ignore

// --------------------------------------------------------------------------------------
// Visualizers
// --------------------------------------------------------------------------------------

Object
  |> addMethod "do" (newMethod ["f"] (
    local("f")?value([self])
  ))
  |> ignore

let Html = 
  makeClass "Html" Object
  |> addSlot "tag"
  |> addSlot "attributes"
  |> addSlot "children"

// TODO: Hack to make Html class accessible
p.Slots.["activation"].Slots.["locals"].Slots <-
  p.Slots.["activation"].Slots.["locals"].Slots.Add("Html", Html)


let visualize =
  self?get_p([])?``do``([fn "p" (
    let om = local("p")?reflectObject [local "obj"]
    let cl = om?getClass []
    let cm = local("p")?reflectClass [cl]
    let cn = cm?getName([])
    let attrs = cm?getMethods([])?map [fnn ["name";"method"] (
      local("Html")?``new``([])?``do``([fn "h" (
            local("h")?set_tag([str "li"]) 
        <.> local("h")?set_children([arr [local "name"]]) 
        <.> local("h")
      )])
    )]
    let ul = 
      local("Html")?``new``([])?``do``([fn "h" (
            local("h")?set_tag([str "ul"]) 
        <.> local("h")?set_children([attrs]) 
        <.> local("h")
      )])
    let h = 
      local("Html")?``new``([])?``do``([fn "h" (
            local("h")?set_tag([str "h2"]) 
        <.> local("h")?set_children([arr [cn]]) 
        <.> local("h")
      )])
    let div = 
      local("Html")?``new``([])?``do``([fn "h" (
            local("h")?set_tag([str "div"]) 
        <.> local("h")?set_children([arr [h; ul]]) 
        <.> local("h")
      )])
    div
  )])
  // cm?getName([])

(*
let om = self?get_p([])?reflectObject [self]
  cm?getMethods([])?map [fnn ["name";"method"] (
    self?get_p([])?print [local "name"]
  )]
*)

let Visualizer = p |> sendMessage "newClass" [makeString "Visualizer"; Obj]
let visMirror = p |> sendMessage "reflectClass" [Visualizer]
visMirror |> sendMessage "addMethod" [makeString "visualize"; newMethod ["obj"] visualize] |> ignore
visMirror |> sendMessage "addSlot" [makeString "p"] |> ignore
let vis = Visualizer |> sendMessage "new" []
vis |> sendMessage "set_p" [p] |> ignore

let demo = vis |> sendMessage "visualize" [sp]

let tests2() = 
  vis |> sendMessage "visualize" [sp]
  |> ignore

  let x = vis |> sendMessage "visualize" [sp]

  x.Value.Value
  |> ignore

  x.Slots.["name"].Value.Value
  |> ignore

  x.Class.Value.Slots.["name"].Value.Value
  |> ignore

  x.Slots.Keys
  |> ignore

(*
class SelfPrinting usingPlatform: p <Platform> = (
|
private ObjectMirror <ObjectMirror class> = p mirrors ObjectMirror.
|
) (
printClass ^ <String> = (
  ^(ObjectMirror reflecting: self) getClass mixin declaration source
)
) 
*)

// IDEA: Create a proxy over platform that replaces all 'Password'
// objects in all objects the platform can ever return with an 
// implementation that hides the value of the password.