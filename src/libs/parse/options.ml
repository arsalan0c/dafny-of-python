type 'a t = 'a option

let fail = None
let mzero = None

let map f = function
  | None -> None
  | Some x -> Some (f x)

let return x = Some x 

let (>>=) m f = 
  match m with
  | Some x -> f x
  | None -> None

let fold some none = function
  | None -> none ()
  | Some x -> some x
  
let mplus m1 m2 =
  match m1, m2 with
  | Some x, _ -> Some x
  | _, Some x -> Some x
  | None, None -> None