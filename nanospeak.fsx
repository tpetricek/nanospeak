[<ReferenceEquality>]
type Object = 
  { Class : Object option
    Value : obj option
    mutable Slots : Map<string, Object> }

type NativeFunc = Object -> Object list -> Object

let Top = { Class = None; Value = None; Slots = Map.empty }
let Metaclass = { Class = None; Value = None; Slots = Map.empty }
let Class = { Class = Some Metaclass; Value = None; Slots = Map.empty }

let String = { Class = Some Class; Value = None; Slots = Map.empty }
let NativeObj = { Class = Some Class; Value = None; Slots = Map.empty }
let Collection = { Class = Some Class; Value = None; Slots = Map.empty }

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
    "methods", makeObject Collection [ ]
  ]

let Object = makeClass "Object" Top
let Null = makeClass "Null" Object
let Method = makeClass "Method" Object
let NativeMethod = makeClass "NativeMethod" Object

String.Slots <- (makeClass "String" Object).Slots
NativeObj.Slots <- (makeClass "NativeObj" Object).Slots
Collection.Slots <- (makeClass "List" Object).Slots
Metaclass.Slots <- (makeClass "Metaclass" Object).Slots
Class.Slots <- (makeClass "Class" Object).Slots

// let withSlot name value obj =
//   { obj with Slots = obj.Slots.Add(name, value) }

let newMethod code = 
  makeObject Method [ "code", code ]

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
  |> addMethod ("set_" + name) (newNativeMethod (fun inst [arg] ->
    inst.Slots <- inst.Slots.Add(name, arg) 
    makeObject Null [] )) 
  |> addMethod ("get_" + name) (newNativeMethod (fun inst [] ->
    inst.Slots.[name] )) 

//let Lookup = makeClass "Looup" Object
//let lookupExpr = makeObject 

Class
  |> addMethod "new" (newNativeMethod (fun inst _ ->
    makeObject inst []
  ))
  |> ignore

let Platform = 
  makeClass "Platform" Object
  |> addMethod "newClass" (newNativeMethod (fun _ [name; parent] -> 
      if name.Class <> Some String then failwith "Platform.newClass: 1st arg not a string"
      if parent.Class <> Some Class then failwith "Platform.newClass: 2nd arg not a class"
      makeClass (unbox name.Value.Value) parent
  ))
  |> addMethod "getClass" (newNativeMethod (fun _ [name] -> 
      if name.Class <> Some String then failwith "Platform.getClass: 1st arg not a string"
      if name.Value.Value = "Object" then Object
      else failwithf "Platform.getClass: Unknown class: %A" name.Value.Value
  ))
  |> addMethod "addSlot" (newNativeMethod (fun _ [cls; name] -> 
      if cls.Class <> Some Class then failwith "Platform.addSlot: 1st arg not a class"
      if name.Class <> Some String then failwith "Platform.addSlot: 2nd arg not a string"
      cls |> addSlot (unbox name.Value.Value) |> ignore
      makeObject Null []
  ))
  |> addMethod "addMethod" (newNativeMethod (fun _ [cls; name; meth] -> 
      if cls.Class <> Some Class then failwith "Platform.addMethod: 1st arg not a class"
      if name.Class <> Some String then failwith "Platform.addMethod: 2nd arg not a string"
      if meth.Class <> Some Method && meth.Class <> Some NativeMethod then failwith "Platform.addMethod: 3rd arg not a method or native method"
      cls |> addMethod (unbox name.Value.Value) meth |> ignore
      makeObject Null []
  ))
  |> addMethod "print" (newNativeMethod (fun _ [msg] -> 
      if msg.Class <> Some String then failwith "Platform.print: 1st arg not a string"
      printfn "%s" (unbox msg.Value.Value)
      makeObject Null []
  ))
  |> addSlot "activation"

let p = makeObject Platform []


let Activation = 
  makeClass "Activation" Object
  |> addSlot "self"

let rec sendMessage msg args obj = 
  let rec loop cls = 
    match cls with 
    | None -> failwithf "sendMessage: sending '%A' to Top or Metaclass failed" msg
    | Some cls -> 
        //printfn "Looking for '%s' in %A" msg cls.Slots.["name"].Value.Value
        match cls.Slots.["methods"].Slots.TryFind msg with 
        | Some meth -> 
            if meth.Class = Some Method then
              let act = makeObject Activation ["self", obj]
              p |> sendMessage "set_activation" [act] |> ignore
              let res = meth.Slots.["code"] |> sendMessage "eval" []
              p |> sendMessage "set_activation" [makeObject Null []] |> ignore
              res 
            elif meth.Class = Some NativeMethod then
              (meth.Slots.["func"].Value.Value :?> NativeFunc) obj args
            else 
              failwith "sendMessage: Neither Method nor NativeMethod"
        | None -> loop (cls.Slots.TryFind "parent")
  loop obj.Class


let Expr = 
  makeClass "Expr" Object 
  |> addMethod "eval" (newNativeMethod (fun _ [] -> 
      failwithf "Expr.eval: Abstract method not implemented here"))
let SelfExpr = 
  makeClass "SelfExpr" Expr
  |> addMethod "eval" (newNativeMethod (fun inst [] -> 
      let act = p |> sendMessage "get_activation" []
      act |> sendMessage "get_self" []
  ))
let StringExpr = 
  makeClass "SelfExpr" Expr |> addSlot "value"
  |> addMethod "eval" (newNativeMethod (fun inst [] -> 
      inst.Slots.["value"]
  ))
let SendExpr = 
  makeClass "SendExpr" Expr |> addSlot "receiver" |> addSlot "name" |> addSlot "args"
  |> addMethod "eval" (newNativeMethod (fun inst [] -> 
      //printfn "EVALING SEND"
      let recv = inst.Slots.["receiver"] |> sendMessage "eval" []
      let name = inst.Slots.["name"] |> sendMessage "eval" []
      let args = 
        [ for kvp in inst.Slots.["args"].Slots ->
            int kvp.Key, sendMessage "eval" [] kvp.Value ]
        |> List.sortBy fst |> List.map snd
      if name.Class <> Some String then failwith "SendExpr.eval: name is not string"
      //printfn "SENDING %s" (unbox name.Value.Value) 
      sendMessage (unbox name.Value.Value) args recv
  ))

let hello = 
  makeObject SendExpr [
    "receiver", makeObject SendExpr [ 
      "receiver", makeObject SelfExpr [] 
      "name", makeObject StringExpr ["value", makeString "get_p"] 
      "args", makeObject Collection []
    ]
    "name", makeObject StringExpr ["value", makeString "print"]
    "args", makeObject Collection [
      "0", makeObject StringExpr ["value", makeString "Hello world!"]
    ]
  ]

let Obj = p |> sendMessage "getClass" [makeString "Object"]
let SelfPrinting = p |> sendMessage "newClass" [makeString "SelfPrinting"; Obj]
p |> sendMessage "addMethod" [SelfPrinting; makeString "printClass"; newMethod hello ]
p |> sendMessage "addSlot" [SelfPrinting; makeString "p"]
let sp = SelfPrinting |> sendMessage "new" []
sp |> sendMessage "set_p" [p]
// (sp |> sendMessage "get_p" []).Slots.["name"].Value.Value
sp |> sendMessage "printClass" [] |> ignore



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