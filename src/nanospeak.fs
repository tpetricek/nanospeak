#if INTERACTIVE
fsi.PrintDepth <- 10
#else
module Nanospeak.Core
#endif

// --------------------------------------------------------------------------------------
//
// --------------------------------------------------------------------------------------

let (|L0|) = function [] -> () | _ -> failwith "L0: Expected empty list"
let (|L1|) = function [v] -> v | _ -> failwith "L1: Expected list with 1 value"
let (|L2|) = function [v1;v2] -> v1,v2 | _ -> failwith "L2: Expected list with 2 values"
let (|L3|) = function [v1;v2;v3] -> v1,v2,v3 | _ -> failwith "L3: Expected list with 3 values"


[<ReferenceEquality>]
type Objekt = 
  { mutable Class : Objekt option
    mutable Value : obj option
    mutable Slots : Map<string, Objekt> }

type NativeFunc = Objekt -> Objekt list -> Objekt

// TODO: This is probably wrong (depends on the language we are 
// modelling? but I'm pretty sure it is wrong for all of them...)
let Top = { Class = None; Value = None; Slots = Map.ofSeq ["name", { Class = None; Value = Some "Top"; Slots = Map.empty } ] }
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

let makeClass name parent enclosing =
  makeObject Class [ 
    "parent", parent
    "enclosing", enclosing
    "name", makeString name
    "methods", makeObject Lookup [ ]
  ]

let TopModule = makeClass "TopMOdule" Top (makeObject Top [])
let topModule = makeObject TopModule []

let Object = makeClass "Object" Top topModule
let Null = makeClass "Null" Object topModule
let Method = makeClass "Method" Object topModule
let NativeMethod = makeClass "NativeMethod" Object topModule

Str.Slots <- (makeClass "String" Object topModule).Slots
NativeObj.Slots <- (makeClass "NativeObj" Object topModule).Slots
Lookup.Slots <- (makeClass "Lookup" Object topModule).Slots
Metaclass.Slots <- (makeClass "Metaclass" Object topModule).Slots
Class.Slots <- (makeClass "Class" Object topModule).Slots

(*
(makeClass "Class" Object topModule).Slots.["parent"]

Object.Slots.["name"].Value.Value

Class.Slots.["name"].Value.Value
Class.Slots.["parent"].Class.Value.Slots.["name"].Value.Value
*)
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
  |> addMethod "addMap" (newNativeMethod (fun inst (L2(k, arg)) ->
    if k.Class <> Some Str then failwith "Lookup.addMap: 1st arg not a string"
    inst.Slots <- inst.Slots.Add(unbox k.Value.Value, arg)
    inst
  ))
  |> addMethod "add" (newNativeMethod (fun inst (L1(arg)) ->
    let tryInt (n:string) = match System.Int32.TryParse(n) with true, n -> Some n | _ -> None
    let i = inst.Slots.Keys |> Seq.choose tryInt |> Seq.append [-1] |> Seq.max
    inst.Slots <- inst.Slots.Add(string (i + 1), arg)
    inst
  ))
  |> addMethod "remove" (newNativeMethod (fun inst (L1(key)) ->
    if key.Class <> Some Str then failwith "Lookup.remove: 1st arg not a string"
    inst.Slots <- inst.Slots.Remove(unbox key.Value.Value)
    inst
  ))
  |> ignore

let ObjectMirror = 
  makeClass "ObjectMirror" Object topModule
  |> addSlot "object"
  |> addMethod "getClass" (newNativeMethod (fun inst (L0()) -> 
      inst.Slots.["object"].Class.Value
  ))
  |> addMethod "getSlots" (newNativeMethod (fun inst (L0()) -> 
      makeObject Lookup [ for kv in inst.Slots.["object"].Slots -> kv.Key, kv.Value ]
  ))

let ClassMirror = 
  makeClass "ClassMirror" Object topModule
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
  makeClass "Platform" Object topModule
  |> addMethod "reflectClass" (newNativeMethod (fun _ (L1(cls)) -> 
      if cls.Class <> Some Class && cls.Class <> Some Metaclass && cls.Class <> None then failwith "Platform.reflectClass: 1st arg not a class"
      makeObject ClassMirror [ "class", cls ]
  ))
  |> addMethod "reflectObject" (newNativeMethod (fun _ (L1(obj)) -> 
      makeObject ObjectMirror [ "object", obj ]
  ))
  |> addMethod "newClass" (newNativeMethod (fun _ (L3(name, parent, enclosing)) -> 
      if name.Class <> Some Str then failwith "Platform.newClass: 1st arg not a string"
      if parent.Class <> Some Class then failwith "Platform.newClass: 2nd arg not a class"
      makeClass (unbox name.Value.Value) parent enclosing
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
  makeClass "Activation" Object topModule
  |> addSlot "self"
  |> addSlot "locals"
  |> addSlot "prev"

let Locals = 
  makeClass "Locals" Object topModule

let topLocals = 
  makeObject Locals [] 

let p = makeObject Platform [
  "activation", makeObject Activation [ 
    "self", makeObject Null []
    "locals", makeObject Locals [] 
  ]
]

let withActivation self enclosingLocalsOpt locals f = 
  let prevAct = p.Slots.["activation"]
  
  let enclosingLocals = defaultArg enclosingLocalsOpt topLocals

  let AnonLocals = 
    locals |> Seq.fold (fun al (k, _) -> addSlot k al)
      (makeClass "#AnonLocals" Locals enclosingLocals)
  let locals = makeObject AnonLocals locals

  let act = 
    makeObject Activation 
      [ "prev", prevAct; "self", self; "locals", locals ]

  //printfn "Set activation! Class of new self: %s" (unbox act.Slots.["self"].Class.Value.Slots.["name"].Value.Value)
  p.Slots <- p.Slots.Add("activation", act)
  let res = f act
  let currAct = p.Slots.["activation"]
  let prevAct = currAct.Slots.["prev"]
  //printfn "Revert activation! Class of new self: %s" (unbox prevAct.Slots.["self"].Class.Value.Slots.["name"].Value.Value)
  p.Slots <- p.Slots.Add("activation", prevAct) 
  res 

let rec implicitReceiver self receiver msg =
  // c.f. https://bracha.org/newspeak-modules.pdf, fig 6, except that 
  // the following does not do 'enclosingDeclaration' in while loop
  //printfn "Looking for implicit receiver in '%A'" receiver.Class.Value.Slots.["name"].Value.Value
  if receiver.Class.Value = Top then self
  elif receiver.Class.Value.Slots.["methods"].Slots.ContainsKey msg then receiver
  else implicitReceiver self receiver.Class.Value.Slots.["enclosing"] msg
    
let mutable logDepth = ""

let rec evalMethod meth (args:_ list) obj =
  if meth.Class = Some Method then
    let vars = arrayValues meth.Slots.["args"] |> List.map (fun v -> v.Value.Value :?> string)
    if vars.Length <> args.Length then failwith "sendMessage: Number of arguments did not match"
    let varArgs = List.zip vars args
    withActivation obj None varArgs (fun act ->
        meth.Slots.["code"] |> sendMessage "eval" [act] )
  elif meth.Class = Some NativeMethod then
    withActivation obj None [] (fun act ->
      (meth.Slots.["func"].Value.Value :?> NativeFunc) obj args 
    )
  else 
    failwith "sendMessage: Neither Method nor NativeMethod"

and sendMessage msg args receiver = 
  let rec loop cls = 
    if cls = Top then 
      failwithf "sendMessage: doesNotUnderstand '%s' (receiver: %s)" msg 
        (unbox receiver.Class.Value.Slots.["name"].Value.Value)
    match cls.Slots.["methods"].Slots.TryFind msg with 
    | Some meth -> 
        //printfn "%sEval method: %s.%s" logDepth (unbox receiver.Class.Value.Slots.["name"].Value.Value) msg
        //logDepth <- logDepth + "  "
        let res = evalMethod meth args receiver
        //logDepth <- logDepth.Substring(0, logDepth.Length - 2)
        res
    | None ->
        //printfn $"""Current: {cls.Slots.["name"].Value.Value}"""
        //printfn $"""Parent: {cls.Slots.["parent"].Class.Value.Slots.["name"].Value.Value}"""
        loop (cls.Slots.["parent"])
  loop receiver.Class.Value

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
  makeClass "Closure" Object topModule
  |> addSlot "args"
  |> addSlot "activation"
  |> addSlot "body"
  |> addMethod "value" (newNativeMethod (fun inst args ->
      let vars = arrayValues inst.Slots.["args"] |> List.map (fun v -> v.Value.Value :?> string)
      let body = inst.Slots.["body"]
      let declActivation = inst.Slots.["activation"]
      if vars.Length <> args.Length then failwith "Closure.value: Number of arguments did not match"
      let varArgs = List.zip vars args
      withActivation declActivation.Slots.["self"] (Some declActivation.Slots.["locals"]) varArgs (fun act -> 
        body |> sendMessage "eval" [act])
  ))

let Expr = 
  makeClass "Expr" Object topModule
  |> addMethod "eval" (newNativeMethod (fun _ (L1(act)) -> 
      failwithf "Expr.eval: Abstract method not implemented here"))
let SeqExpr = 
  makeClass "SeqExpr" Expr topModule |> addSlot "e1" |> addSlot "e2"
  |> addMethod "eval" (newNativeMethod (fun inst (L1(act)) -> 
      inst.Slots.["e1"] |> sendMessage "eval" [act] |> ignore
      inst.Slots.["e2"] |> sendMessage "eval" [act]
  ))
let LookupExpr = 
  makeClass "LookupExpr" Expr topModule |> addSlot "values"
  |> addMethod "eval" (newNativeMethod (fun inst (L1(act)) -> 
      makeObject Lookup 
        [ for kv in inst.Slots.["values"].Slots -> 
            kv.Key, sendMessage "eval" [act] kv.Value ]
  ))
let SelfExpr = 
  makeClass "SelfExpr" Expr topModule 
  |> addMethod "eval" (newNativeMethod (fun inst (L1(act)) -> 
      let s = act |> sendMessage "get_self" []
      //printfn "Class of 'self': %s" (unbox s.Class.Value.Slots.["name"].Value.Value)
      s
  ))
let ImplicitExpr = 
  makeClass "ImplicitExpr" Expr topModule 
  |> addMethod "eval" (newNativeMethod (fun inst (L1(act)) -> 
      failwith "Implicitexpr.eval: Not supposed to be evaluated!"
  ))
let StringExpr = 
  makeClass "StringExpr" Expr topModule |> addSlot "value"
  |> addMethod "eval" (newNativeMethod (fun inst (L1(act)) -> 
      inst.Slots.["value"]
  ))
let LocalExpr = 
  makeClass "LocalExpr" Expr topModule |> addSlot "name"
  |> addMethod "eval" (newNativeMethod (fun inst (L1(act)) -> 
      //printfn "Lookup local: %s" (inst.Slots.["name"].Value.Value :?> string)
      //printfn "  in: %A" [ for kvp in act.Slots.["locals"].Slots -> kvp.Key ]
      //printfn "  enclosing: %A" act.Slots.["locals"].Class.Value.Slots.["enclosing"].Class.Value.Slots.["name"].Value.Value

      let msg = "get_" + (inst.Slots.["name"].Value.Value :?> string)
      let loc = act.Slots.["locals"] 
      implicitReceiver loc loc msg |> sendMessage msg []
  ))
let SendExpr = 
  makeClass "SendExpr" Expr topModule |> addSlot "receiver" |> addSlot "name" |> addSlot "args"
  |> addMethod "eval" (newNativeMethod (fun inst (L1(act)) -> 
      //printfn "EVALING SEND"
      let msg = inst.Slots.["name"].Value.Value :?> string
      let receiver = 
        if inst.Slots.["receiver"].Class.Value = ImplicitExpr then
          let self = act.Slots.["self"]
          implicitReceiver self self msg
        else 
          inst.Slots.["receiver"] |> sendMessage "eval" [act]

      // TODO: This should send 'forEach' to the array?
      let args = arrayValues inst.Slots.["args"] |> List.map (sendMessage "eval" [act] )  
      //printfn "SENDING %s" (unbox name.Value.Value) 
      sendMessage msg args receiver
  ))
let ClosureExpr = 
  makeClass "ClosureExpr" Expr topModule |> addSlot "args" |> addSlot "body"
  |> addMethod "eval" (newNativeMethod (fun inst (L1(act)) -> 
      makeObject Closure [
        "args", inst.Slots.["args"]
        "activation", act
        "body", inst.Slots.["body"]] 
  ))
let HoleExpr = 
  makeClass "HoleExpr" Expr topModule |> addSlot "wrapped"
  |> addMethod "eval" (newNativeMethod (fun inst (L1(act)) -> 
      if inst.Slots.["wrapped"].Class.Value = Null then failwith "HoleExpr.eval: Cannot evaluate an empty hole"
      else inst.Slots.["wrapped"] |> sendMessage "eval" [act]
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

let Boolean = makeClass "Boolean" Object topModule 
let rec True = 
  makeClass "True" Boolean topModule 
  |> addMethod "not" (newNativeMethod (fun _ _ -> False |> sendMessage "new" []))
and False = 
  makeClass "False" Boolean topModule 
  |> addMethod "not" (newNativeMethod (fun _ _ -> True |> sendMessage "new" []))

let (?) r n args = 
  makeObject SendExpr [ 
    "receiver", r
    "opened", makeObject False []
    "name", makeString n
    "args", makeObject Lookup [
      for i, a in Seq.indexed args -> string i, a ]
  ]

let implicit = makeObject ImplicitExpr []
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
let SelfPrinting = p |> sendMessage "newClass" [makeString "SelfPrinting"; Obj; topModule]
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
  makeClass "Html" Object topModule 
  |> addSlot "tag"
  |> addSlot "attributes"
  |> addSlot "children"

let html = 
  implicit?get_Html([])?``new``([])?``do``([fn "h" (
        local("h")?set_tag([local("tag")]) 
    <.> local("h")?set_attributes([local("attributes")]) 
    <.> local("h")?set_children([local("children")]) 
    <.> local("h")
  )])

let link = 
  let click = fnn [] (implicit?``open`` [local("target")])
  self?html [ 
    str "a"; map ["href", str "javascript:;"; "click", click ]; arr [str "open"] 
  ] 

let visualize =
  // TODO: Store 'self' in a local variable first, because 'self' does not
  // work inside code blocks (not sure how this should be done right...)
  let om = implicit?get_p([])?reflectObject [local("obj")]
  let cl = om?getClass []
  let cm = implicit?get_p([])?reflectClass [cl]
  let cn = cm?getName([])
  let meths = cm?getMethods([])?map [fnn ["name";"method"] (
    implicit?html [
      str "li"
      map []
      arr [local "name"]
    ]
  )]
  let slots = om?getSlots([])?map [fnn ["name";"value"] (
    implicit?html [
      str "li"
      map []
      arr [
        local "name"
        str " ("
        implicit?link [ str "open"; local "value" ]
        str ")"
      ]
    ]
  )]
  implicit?html [
    str "div"
    map [ "class", str "obj" ]
    arr [
      implicit?html [ 
        str "h2"; map []; 
        arr [ 
          str "Class: "; implicit?html [ str "strong"; map []; arr [ cn ] ] 
          str " ("
          implicit?link [ str "open"; cl ]
          str ")"
        ] 
      ]
      implicit?html [ str "h3"; map []; arr [ str "Slots" ] ]
      implicit?html [ str "ul"; map []; slots ]
      //implicit?html [ str "h3"; map []; arr [ str "Methods" ] ]
      //implicit?html [ str "ul"; map []; meths ]
    ]
  ]

let Visualizer = 
  makeClass "Visualizer" Obj topModule 
  |> addMethod "html" (newMethod ["tag"; "attributes"; "children"] html)
  |> addMethod "link" (newMethod ["lbl"; "target"] link)
  |> addSlot "p"

let ObjectVisualizer = 
  makeClass "ObjectVisualizer" Visualizer topModule 
  |> addMethod "visualize" (newMethod ["obj"] visualize)

let StringVisualizer = 
  makeClass "StringVisualizer" Visualizer topModule 
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
  makeClass "QuoteExpr" Expr topModule  |> addSlot "e" 
  |> addMethod "eval" (newNativeMethod (fun inst (L1(act)) -> 
      inst.Slots.["o"] 
  ))
let quote o = 
  makeObject QuoteExpr ["o", o]
let vz o = o?visualizer([])?visualize([o])
let ssv = self?visualizer([])

HoleExpr |> addSlot "opened" |> ignore
let hole () = makeObject HoleExpr [ "opened", makeObject False []; "wrapped", makeObject Null [] ]
let holeWrap o = makeObject HoleExpr [ "opened", makeObject False []; "wrapped", o ]



let becomef l replacement = fnn [] (
  local(l)?become [quote(replacement)?clone([])] <.>
  local(l)?``visualizer``([])?``update`` []
)

let becomehw l = fnn [] (
  quote(hole())?clone([])?``do``([ fn "h" (
    local("h")?set_wrapped([local(l)?clone([])]) <.>
    //implicit?``open``([local(l)]) <.>
    //implicit?``open``([local("h")])  <.>
    local(l)?become [local("h")] <.>
    local(l)?``visualizer``([])?``update`` []
  )])
)

  //local(l)?become [quote(replacement)?clone([])] <.>
//)

let ExprVisualizer = 
  makeClass "ExprVisualizer" Visualizer topModule 
  |> addMethod "visualize" (newMethod ["obj"] (  
    self?``do``([fn "v" (
      local("v")?html [
        str "div"
        map [ "class", str "obj" ]
        arr [
          local("v")?html [
            str "a"
            map [ 
              "class", str "close"; "href", str "javascript:;"; 
              "click", becomehw "obj" ]
            arr [ str "x" ]
          ]
          local("v")?html [str "span"; map []; (local("obj")?tokens []) ]
        ]        
      ]
    )])
  ))

SeqExpr |> addMethod "tokens" (newMethod [] (arr [
    self?get_e1 [] |> vz
    str "."
    self?get_e2 [] |> vz
  ])) |> ignore
LookupExpr |> addMethod "tokens" (newMethod [] (arr [
    ssv?html [ 
      str "ul"; map []; 
      self?get_values([])?map([fnn ["k"; "v"] (
        ssv?html [ str "li"; map []; arr [ 
          let rmthis = fnn [] (
            self?clone([])?``do``([fn "rpl" (
              local("rpl")?get_values([])?remove([local("k")]) <.> 
              self?become [local("rpl")] <.>
              self?``visualizer``([])?``update`` []
            )]) 
          )
          ssv?html [ str "a"; map [ "class", str "remove"; 
            "href", str "javascript:;"; "click", rmthis ]; arr [ str "x" ] ]
          local("k"); 
          str "="; 
          local("v") |> vz 
        ]]
      )])?add([
        ssv?html [ str "li"; map []; arr [ 
          ssv?html [ 
            str "a"; map [ "href", str "javascript:;"; "class", str "add"]; 
            arr [str "+"] ] 
        ]]
      ])
    ]
  ])) |> ignore
SelfExpr |> addMethod "tokens" (newMethod [] (arr [
    str "self"
  ])) |> ignore
ImplicitExpr |> addMethod "tokens" (newMethod [] (arr [
    str "?"
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
    ssv?html [ str "a"; map [ "class", str "add"; "href", str "javascript:;"; "click", add]; arr [ str "+" ] ]
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
  makeClass "WorkspaceVisualizer" Visualizer topModule 
  |> addMethod "visualize" (newMethod ["workspace"] (  
    implicit?html [
      str "div"
      map [ "class", str "obj" ]
      arr [ 
        implicit?html [ str "h2"; map []; arr [str "Workspace"]]
        implicit?html [ str "h3"; map []; arr [str "Code"]]
        ( let c = local("workspace")?get_code([]) in c?visualizer([])?visualize([c]) )
        implicit?html [ str "h3"; map []; arr [str "Output"]]
        local("workspace")?get_output([]) 
        implicit?html [ str "h3"; map []; arr [str "Commands"]]
        ( let doit = fnn [] (
            let act = implicit?get_p([])?get_activation([])
            local("workspace")?get_code([])?eval([act]) <.> 
            implicit?update([])
          )
          implicit?html [ str "a"; map [ "href", str "javascript:;"; "click", doit ]; arr [ str "Run!" ] ] )
      ]
    ]
  ))
   

let Workspace = 
  makeClass "Workspace" Object topModule 
  |> addSlot "output"
  |> addSlot "code"
  |> addMethod "visualizer" (newMethod [] (implicit?get_workspaceVisualizer([])))
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

let tops = 
  [ "Html", Html
    "objectVisualizer", ov
    "stringVisualizer", sv
    "exprVisualizer", ev
    "workspaceVisualizer", wv
    "StringExpr", StringExpr
    "LocalExpr", LocalExpr
    "false", makeObject False []
    "workspace", workspace ]

tops |> Seq.fold (fun tm (k, v) -> addSlot k tm) TopModule |> ignore
topModule.Slots <- 
  tops |> Seq.fold (fun slots (k, v) -> Map.add k v slots) topModule.Slots

Object |> addMethod "visualizer" (newMethod [] (implicit?get_objectVisualizer([]))) |> ignore
Str |> addMethod "visualizer" (newMethod [] (implicit?get_stringVisualizer([]))) |> ignore
Expr |> addMethod "visualizer" (newMethod [] (implicit?get_exprVisualizer([]))) |> ignore

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




// --------------------------------------------------------------------------------------
// Nested classes
// --------------------------------------------------------------------------------------

let test () =
  let A =
    makeClass "A" Object topModule
    |> addSlot "demo"
  let A_B =
    makeClass "B" Object (makeObject A ["demo", makeString "Hi there"])
    |> addMethod "getDemo1" (newMethod [] (implicit?get_demo([])))
    |> addMethod "getDemo2" (newMethod [] (self?getDemo1([])))
  let res = 
    makeObject A_B []
    |> sendMessage "getDemo2" []
  unbox res.Value.Value = "Hi there"

let test2 () = 
  let vis = sp |> sendMessage "visualizer" []
  vis |> sendMessage "visualize" [sp]
