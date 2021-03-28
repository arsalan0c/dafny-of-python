type 'a t = 'a option

let fail = None

let map f = function
  | None -> None
  | Some x -> Some (f x)

let return x = Some x 

let (>>=) m f = 
  match m with
  | None -> None
  | Some x -> f x

let fold some none = function
  | None -> none
  | Some x -> some x
  