#if INTERACTIVE
#else
module Nanospeak.Core
#endif

// --------------------------------------------------------------------------------------
//
// --------------------------------------------------------------------------------------

let (|L0|) = function [] -> () | _ -> failwith "L0: Expected empty list"
let (|L1|) = function [v] -> v | _ -> failwith "L1: Expected list with 1 value"
let (|L2|) = function [v1;v2] -> v1,v2 | _ -> failwith "L2: Expected list with 2 values"


[<ReferenceEquality>]
type Objekt = 
  { mutable Class : Objekt option
    mutable Value : obj option
    mutable Slots : Map<string, Objekt> }

type NativeFunc = Objekt -> Objekt list -> Objekt

// TODO: This is probably wrong (depends on the language we are 
// modelling? but I'm pretty sure it is wrong for all of them...)
let Top = { Class = None; Value = None; Slots = Map.empty }
let Metaclass = { Class = None; Value = None; Slots = Map.empty }
let Class = { Class = Some Metaclass; Value = None; Slots = Map.empty }

let Str = { Class = Some Class; Value = None; Slots = Map.empty }
let NativeObj = { Class = Some Class; Value = None; Slots = Map.empty }
let Lookup = { Class = Some Class; Value = None; Slots = Map.empty }

let makeNativeObj o = 
  { Class = Some NativeObj; Value = Some o; Slots = Map.empty }
let makeString (s:string) = 
  { Class = Some Str; Value = Some s; Slots = Map.empty }
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

Str.Slots <- (makeClass "String" Object).Slots
NativeObj.Slots <- (makeClass "NativeObj" Object).Slots
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
  let args = makeObject Lookup [ for i, s in Seq.indexed args -> string i, makeString s ]
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

Object
  |> addMethod "clone" (newNativeMethod (fun inst (L0()) ->
    let rec clone o = 
      { Slots = o.Slots |> Map.map (fun _ -> clone); Value = o.Value; Class = o.Class }
    clone inst
  ))
  |> addMethod "become" (newNativeMethod (fun inst (L1(target)) ->
    //printfn "Becoming: %A" target.Class.Value.Slots.["name"].Value.Value
    //printfn ".. %A" target.Slots.["value"].Value
    inst.Slots <- target.Slots
    inst.Value <- target.Value
    inst.Class <- target.Class
    makeObject Null []
  ))
  |> ignore

Class
  |> addSlot "parent"
  |> addSlot "name"
  |> addSlot "methods"
  |> addMethod "new" (newNativeMethod (fun inst _ ->
    makeObject inst []
  ))
  |> ignore

Lookup 
  |> addMethod "add" (newNativeMethod (fun inst (L1(arg)) ->
    let tryInt n = match System.Int32.TryParse(n) with true, n -> Some n | _ -> None
    let i = inst.Slots.Keys |> Seq.choose tryInt |> Seq.append [-1] |> Seq.max
    inst.Slots <- inst.Slots.Add(string (i + 1), arg)
    inst
  ))
  |> ignore

let ObjectMirror = 
  makeClass "ObjectMirror" Object
  |> addSlot "object"
  |> addMethod "getClass" (newNativeMethod (fun inst (L0()) -> 
      inst.Slots.["object"].Class.Value
  ))
  |> addMethod "getSlots" (newNativeMethod (fun inst (L0()) -> 
      makeObject Lookup [ for kv in inst.Slots.["object"].Slots -> kv.Key, kv.Value ]
  ))

let ClassMirror = 
  makeClass "ClassMirror" Object
  |> addSlot "class"
  |> addMethod "addSlot" (newNativeMethod (fun inst (L1(name)) -> 
      if name.Class <> Some Str then failwith "ClassMirror.addSlot: 1st arg not a string"
      let cls = inst.Slots.["class"]
      cls |> addSlot (unbox name.Value.Value) |> ignore
      makeObject Null []
  ))
  |> addMethod "addMethod" (newNativeMethod (fun inst (L2(name, meth)) -> 
      if name.Class <> Some Str then failwith "ClassMirror.addMethod: 1st arg not a string"
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
      if cls.Class <> Some Class && cls.Class <> Some Metaclass && cls.Class <> None then failwith "Platform.reflectClass: 1st arg not a class"
      makeObject ClassMirror [ "class", cls ]
  ))
  |> addMethod "reflectObject" (newNativeMethod (fun _ (L1(obj)) -> 
      makeObject ObjectMirror [ "object", obj ]
  ))
  |> addMethod "newClass" (newNativeMethod (fun _ (L2(name, parent)) -> 
      if name.Class <> Some Str then failwith "Platform.newClass: 1st arg not a string"
      if parent.Class <> Some Class then failwith "Platform.newClass: 2nd arg not a class"
      makeClass (unbox name.Value.Value) parent
  ))
  |> addMethod "getClass" (newNativeMethod (fun _ (L1(name)) -> 
      if name.Class <> Some Str then failwith "Platform.getClass: 1st arg not a string"
      if name.Value.Value = "Object" then Object
      else failwithf "Platform.getClass: Unknown class: %A" name.Value.Value
  ))
  |> addMethod "print" (newNativeMethod (fun _ (L1(msg)) -> 
      if msg.Class <> Some Str then failwith "Platform.print: 1st arg not a string"
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

let rec withActivation selfOpt ctxOpt locals f = 
  let prevAct = p |> sendMessage "get_activation" []
  let self = defaultArg selfOpt (prevAct.Slots.["self"])
  // TODO: Should we create anonymous class inherited from Activation with corresponding slots?
  let locals = makeObject Locals locals
  let act = makeObject Activation [
    yield! [ "prev", prevAct; "self", self; "locals", locals ]
    match ctxOpt with Some ctx -> yield "context", ctx | _ -> ()
  ]
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
        //printfn "Looking for '%s' in %A" msg cls.Slots.["name"].Value.Value
        match cls.Slots.["methods"].Slots.TryFind msg with 
        | Some meth -> 
            if meth.Class = Some Method then
              let vars = arrayValues meth.Slots.["args"] |> List.map (fun v -> v.Value.Value :?> string)
              if vars.Length <> args.Length then failwith "sendMessage: Number of arguments did not match"
              let varArgs = List.zip vars args
              withActivation (Some obj) None varArgs (fun () ->
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

Str
  |> addMethod "append" (newNativeMethod (fun inst (L1(s)) ->
    let gets o = unbox<string> o.Value.Value 
    inst.Value <- Some(box(gets inst + gets s))
    makeObject Null []
  ))
  |> ignore


let Closure = 
  makeClass "Closure" Object
  |> addSlot "args"
  |> addSlot "activation"
  |> addSlot "body"
  |> addMethod "value" (newNativeMethod (fun inst args ->
      let vars = arrayValues inst.Slots.["args"] |> List.map (fun v -> v.Value.Value :?> string)
      let body = inst.Slots.["body"]
      let declActivation = inst.Slots.["activation"]
      if vars.Length <> args.Length then failwith "Closure.value: Number of arguments did not match"
      let varArgs = List.zip vars args
      withActivation None (Some declActivation) varArgs (fun () -> 
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
let LookupExpr = 
  makeClass "LookupExpr" Expr |> addSlot "values"
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      makeObject Lookup 
        [ for kv in inst.Slots.["values"].Slots -> 
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
        //printfn "Looking for local '%s' in: %A" name (String.concat ", " (act.Slots.["locals"].Slots.Keys))
        match act.Slots.["locals"].Slots.TryFind name with 
        | Some value -> Some value
        | None -> 
            ( match act.Slots.TryFind "context" with
              | Some ctxAct -> loop ctxAct
              | _ -> None ) |> Option.orElseWith (fun () -> 
              match act.Slots.TryFind "prev" with
              | Some prevAct -> loop prevAct
              | _ -> None )
      let act = p |> sendMessage "get_activation" []
      match loop act with 
      | Some res -> res
      | _ -> failwithf "LocalExpr.eval: Did not find '%s' in current activation" name
  ))
let SendExpr = 
  makeClass "SendExpr" Expr |> addSlot "receiver" |> addSlot "name" |> addSlot "args"
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      //printfn "EVALING SEND"
      let recv = inst.Slots.["receiver"] |> sendMessage "eval" []
      let name = inst.Slots.["name"].Value.Value :?> string
      // TODO: This should send 'forEach' to the array?
      let args = arrayValues inst.Slots.["args"] |> List.map (sendMessage "eval" [] )  
      //printfn "SENDING %s" (unbox name.Value.Value) 
      sendMessage name args recv
  ))
let ClosureExpr = 
  makeClass "ClosureExpr" Expr |> addSlot "args" |> addSlot "body"
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      makeObject Closure [
        "args", inst.Slots.["args"]
        // TODO: How should closure capture local variables?
        "activation", p |> sendMessage "get_activation" [] 
        "body", inst.Slots.["body"]] 
  ))
let HoleExpr = 
  makeClass "HoleExpr" Expr 
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      failwith "HoleExpr.eval: Cannot evaluate a hole"
  ))

// --------------------------------------------------------------------------------------
// DSL for writing expressions & some samples
// --------------------------------------------------------------------------------------

let (<.>) e1 e2 = makeObject SeqExpr ["e1",e1; "e2",e2] 
let self = makeObject SelfExpr [] 
let fnn ss body = 
  let args = [ for i, s in Seq.indexed ss -> string i, makeString s ]
  makeObject ClosureExpr ["args", makeObject Lookup args; "body", body]
let fn s body = fnn [s] body
let arr els = makeObject LookupExpr ["values", makeObject Lookup [for i, el in Seq.indexed els -> string i, el ]]
let map els = makeObject LookupExpr ["values", makeObject Lookup els]

let local' s = makeObject LocalExpr ["name", makeString s ] // Missing 'opened' and so editor will break

let Boolean = makeClass "Boolean" Object
let rec True = 
  makeClass "True" Boolean
  |> addMethod "not" (newNativeMethod (fun _ _ -> False |> sendMessage "new" []))
and False = 
  makeClass "False" Boolean
  |> addMethod "not" (newNativeMethod (fun _ _ -> True |> sendMessage "new" []))

let (?) r n args = 
  makeObject SendExpr [ 
    "receiver", r
    "opened", makeObject False []
    "name", makeString n
    "args", makeObject Lookup [
      for i, a in Seq.indexed args -> string i, a ]
  ]
let local s = makeObject LocalExpr ["name", makeString s; "opened", makeObject False []]
let str s = makeObject StringExpr ["value", makeString s; "opened", makeObject False []]

True |> addMethod "if" (newMethod ["true"; "false"] (local'("true")?value [])) |> ignore
False |> addMethod "if" (newMethod ["true"; "false"] (local'("false")?value [])) |> ignore
Str
  |> addMethod "eq" (newNativeMethod (fun inst (L1(other)) ->
      if other.Class <> Some Str then failwith "String.eq: 1st arg not a string"
      if inst.Value = other.Value then makeObject True []
      else makeObject False []))      
  |> ignore


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

let html = 
  self?get_p([])?``do``([fn "p" (
    local("Html")?``new``([])?``do``([fn "h" (
          local("h")?set_tag([local("tag")]) 
      <.> local("h")?set_attributes([local("attributes")]) 
      <.> local("h")?set_children([local("children")]) 
      <.> local("h")
    )])
  )])

let link = 
  self?``do``([fn "v" (
    let click = fnn [] (local("v")?``open`` [local "obj"])
    self?html [ str "a"; map ["href", str "javascript:;"; "click", click ]; arr [str "open"] ] 
  )])

let visualize =
  // TODO: Store 'self' in a local variable first, because 'self' does not
  // work inside code blocks (not sure how this should be done right...)
  self?``do``([fn "v" (
    let om = local("v")?get_p([])?reflectObject [local "obj"]
    let cl = om?getClass []
    let cm = local("v")?get_p([])?reflectClass [cl]
    let cn = cm?getName([])
    let meths = cm?getMethods([])?map [fnn ["name";"method"] (
      local("v")?html [
        str "li"
        map []
        arr [local "name"]
      ]
    )]
    let slots = om?getSlots([])?map [fnn ["name";"value"] (
      local("v")?html [
        str "li"
        map []
        arr [
          local "name"
          str " ("
          local("v")?link [ str "open"; local "value" ]
          str ")"
        ]
      ]
    )]
    local("v")?html [
      str "div"
      map [ "class", str "obj" ]
      arr [
        local("v")?html [ 
          str "h2"; map []; 
          arr [ 
            str "Class: "; local("v")?html [ str "strong"; map []; arr [ cn ] ] 
            str " ("
            local("v")?link [ str "open"; cl ]
            str ")"
          ] 
        ]
        cn?eq([str "String"])?``if``([
          fnn[] (local("v")?html [ str "p"; map []; arr [ str "Value: "; local("obj") ]])
          fnn[] (str "")
        ])
        local("v")?html [ str "h3"; map []; arr [ str "Slots" ] ]
        local("v")?html [ str "ul"; map []; slots ]
        //local("v")?html [ str "h3"; map []; arr [ str "Methods" ] ]
        //local("v")?html [ str "ul"; map []; meths ]
      ]
    ]
  )])
  // cm?getName([])

(*
let om = self?get_p([])?reflectObject [self]
  cm?getMethods([])?map [fnn ["name";"method"] (
    self?get_p([])?print [local "name"]
  )]
*)

let Visualizer = 
  makeClass "Visualizer" Obj
  |> addMethod "html" (newMethod ["tag"; "attributes"; "children"] html)
  |> addMethod "link" (newMethod ["lbl"; "obj"] link)
  |> addSlot "p"

let ObjectVisualizer = 
  makeClass "ObjectVisualizer" Visualizer
  |> addMethod "visualize" (newMethod ["obj"] visualize)

let StringVisualizer = 
  makeClass "StringVisualizer" Visualizer
  |> addMethod "visualize" (newMethod ["obj"] (  
    self?``do``([fn "v" (
      local("v")?html [
        str "div"
        map [ "class", str "obj" ]
        arr [ local("obj") ]
      ]
    )])
  ))

let QuoteExpr = 
  makeClass "QuoteExpr" Expr |> addSlot "e" 
  |> addMethod "eval" (newNativeMethod (fun inst (L0()) -> 
      inst.Slots.["o"] 
  ))
let quote o = 
  makeObject QuoteExpr ["o", o]
let vz o = o?visualizer([])?visualize([o])
let ssv = self?visualizer([])

HoleExpr |> addSlot "opened" |> ignore
let hole () = makeObject HoleExpr [ "opened", makeObject False [] ]



let becomef l replacement = fnn [] (
  local(l)?become [quote(replacement)?clone([])] <.>
  local(l)?``visualizer``([])?``update`` []
)

let ExprVisualizer = 
  makeClass "ExprVisualizer" Visualizer
  |> addMethod "visualize" (newMethod ["obj"] (  
    self?``do``([fn "v" (
      local("v")?html [
        str "div"
        map [ "class", str "obj" ]
        (local("obj")?tokens [])?add([
          local("v")?html [
            str "a"
            map [ "href", str "javascript:;"; "click", becomef "obj" (hole ()) ]
            arr [ str "x" ]
          ]
        ])
      ]
    )])
  ))

SeqExpr |> addMethod "tokens" (newMethod [] (arr [
    self?get_e1 [] |> vz
    str "."
    self?get_e2 [] |> vz
  ])) |> ignore
LookupExpr |> addMethod "tokens" (newMethod [] (arr [
    str "["
    ssv?html [ str "span"; map []; self?get_values([])?map [fnn ["k"; "v"] (
      ssv?html [ str "span"; map []; arr [ local("k"); str "="; local("v") |> vz ]]
    )] ]
    str "]"
  ])) |> ignore
SelfExpr |> addMethod "tokens" (newMethod [] (arr [
    str "self"
  ])) |> ignore

let stringEdit isStr = 
    self?``do``([fn "v" (
      let toggle = fnn [] (
        local("v")?set_opened [local("v")?``get_opened``([])?not([]) ] <.>
        local("v")?``visualizer``([])?``update`` []
      )
      local("v")?get_opened([])?not([])?``if`` [ 
        fnn [] (ssv?html [ str "span"; map [ "click", toggle]; arr [
          if isStr then 
            yield str "\""
            yield local("v")?get_value([]) 
            yield str "\""
          else 
            yield local("v")?get_name([]) 
        ] ])
        let enter = fn "nval" ( 
          local("v")?set_opened([local "false"]) <.>
          ( if isStr then local("v")?get_value([])
            else local("v")?get_name([]) )?become([local "nval"]) <.>
          local("v")?``visualizer``([])?``update`` []
        )
        fnn [] (ssv?html [ str "input"; map [ "value", (if isStr then local("v")?get_value([]) else local("v")?get_name([])); 
          "type", str "text"; "enter", enter]; arr []])
      ]
    )])

StringExpr |> addSlot "opened" |> addMethod "tokens" (newMethod [] (arr [ stringEdit true ])) |> ignore
LocalExpr |> addSlot "opened" |> addMethod "tokens" (newMethod [] (arr [ stringEdit false ])) |> ignore

SendExpr |> addSlot "opened" |> addMethod "tokens" (newMethod [] (
  self?``do``([fn "v" (arr [
    self?get_receiver [] |> vz
    str " "
    stringEdit false
    str " "
    ssv?html [ str "span"; map []; self?get_args([])?map [fnn ["k"; "v"] (
      ssv?html [ str "span"; map []; arr [ local("v") |> vz ]]
    )] ] 
    let add = fnn [] (
      local("v")?get_args([])?add([quote(hole ())]) <.>
      local("v")?visualizer([])?update([])
    )
    ssv?html [ str "a"; map [ "href", str "javascript:;"; "click", add]; arr [ str "+" ] ]
  ])]))) |> ignore

ClosureExpr |> addMethod "tokens" (newMethod [] (arr [
    str "[ "
    ssv?html [ str "span"; map []; self?get_args([])?map [fnn ["k"; "v"] (
      ssv?html [ str "span"; map []; arr [ local("v"); str " " ]]
    )] ]
    str "|"
    self?get_body([]) |> vz
    str "]"
  ])) |> ignore
HoleExpr |> addMethod "tokens" (newMethod [] (arr [
    let toggle = fnn [] (
      local("v")?set_opened [local("v")?``get_opened``([])?not([]) ] <.>
      local("v")?``visualizer``([])?``update`` []
    )
    let become lbl replacement = 
      ssv?html [ str "li"; map []; arr [ 
        ssv?html [ str "a"; map [ "href", str "javascript:;"; "click", becomef "v" replacement ]; arr [ str lbl ] ]
      ] ]

    self?``do``([fn "v" (
      ssv?html [
        str "span";
        map [];
        arr [
          ssv?html [ str "a"; map ["href", str "javascript:;"; "click", toggle ]; arr [str "... "] ] 
          local("v")?get_opened([])?``if`` [ 
            fnn [] (ssv?html [
              str "ul";
              map [];
              arr [ 
                become "Seq" (hole() <.> hole())
                become "Local" (local "local")
                become "Self" (self)
                become "Send" (hole()?message [])
                become "String" (str "?")
                become "Array" (arr [ ])
                become "Map" (map [ ])
                become "Closure" (fnn [] (hole()))
              ]
            ]); 
            fnn [] (str "") ]
        ]
      ]
    )])
  ])) |> ignore

let WorkspaceVisualizer = 
  makeClass "WorkspaceVisualizer" Visualizer
  |> addMethod "visualize" (newMethod ["workspace"] (  
    self?``do``([fn "v" (
      local("v")?html [
        str "div"
        map [ "class", str "obj" ]
        arr [ 
          local("v")?html [ str "h2"; map []; arr [str "Workspace"]]
          local("v")?html [ str "h3"; map []; arr [str "Code"]]
          ( let c = local("workspace")?get_code([]) in c?visualizer([])?visualize([c]) )
          local("v")?html [ str "h3"; map []; arr [str "Output"]]
          local("workspace")?get_output([]) 
          local("v")?html [ str "h3"; map []; arr [str "Commands"]]
          ( let doit = fnn [] (
              local("workspace")?get_code([])?eval([]) <.> 
              local("v")?update([])
            )
            local("v")?html [ str "a"; map [ "href", str "javascript:;"; "click", doit ]; arr [ str "Run!" ] ] )
        ]
      ]
    )])
  ))
   

let Workspace = 
  makeClass "Workspace" Object
  |> addSlot "output"
  |> addSlot "code"
  |> addMethod "visualizer" (newMethod [] (local("workspaceVisualizer")))
  |> addMethod "print" (newMethod ["s"] (
    self?get_output([])?append([local("s")])
  ))
let workspace = makeObject Workspace ["output", makeString ""; "code", hole()]


// TODO: Hack to make Html & Visualizer class accessible
let wv = makeObject WorkspaceVisualizer []
wv |> sendMessage "set_p" [p] |> ignore
let ov = ObjectVisualizer |> sendMessage "new" [] 
ov |> sendMessage "set_p" [p] |> ignore
let sv = StringVisualizer |> sendMessage "new" [] 
sv |> sendMessage "set_p" [p] |> ignore
let ev = ExprVisualizer |> sendMessage "new" [] 
ev |> sendMessage "set_p" [p] |> ignore

p.Slots.["activation"].Slots.["locals"].Slots <-
  p.Slots.["activation"].Slots.["locals"].Slots
    .Add("Html", Html).Add("objectVisualizer", ov)
      .Add("stringVisualizer", sv).Add("exprVisualizer", ev).Add("workspaceVisualizer", wv)
        .Add("StringExpr", StringExpr).Add("LocalExpr", LocalExpr).Add("false", makeObject False [])
          .Add("workspace", workspace)

Object |> addMethod "visualizer" (newMethod [] (local("objectVisualizer"))) |> ignore
Str |> addMethod "visualizer" (newMethod [] (local("stringVisualizer"))) |> ignore
Expr |> addMethod "visualizer" (newMethod [] (local("exprVisualizer"))) |> ignore

let tests2() = 
  let vis = ObjectVisualizer |> sendMessage "new" []
  vis |> sendMessage "set_p" [p] |> ignore
  let demo = vis |> sendMessage "visualize" [sp]

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


