module Option = struct
  type 'a t = 'a option

  let map f = function
    | None -> None
    | Some x -> Some (f x)


  let return x = Some x 

  let fail = None

  let (>>=) m f = 
    match m with
    | None -> None
    | Some x -> f x

   let fold some none = function
     | None -> none
     | Some x -> some x  
end