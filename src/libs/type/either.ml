type 'a t = Right of 'a | Left of string


let fail msg = Left msg
let return x = Right x 
let (>>=) m f = 
  match m with
  | Right x -> f x
  | Left err -> Left err

let map f = function
  | Right x -> Right (f x)
  | Left err -> Left err

let fold right left = function
  | Right x -> right x
  | Left err -> left err
  