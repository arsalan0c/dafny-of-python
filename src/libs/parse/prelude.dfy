// // function filterF<T>(pred: ((T) -> bool), a: seq<T>): seq<T>
// //   ensures |filterF(pred, a)| <= |a|
// //   ensures forall k :: 0 <= k < |filterF(pred, a)| ==> filterF(pred, a)[k] in a
// //   ensures forall k :: 0 <= k < |filterF(pred, a)| ==> pred(filterF(pred, a)[k])
// // {
// //   if |a| == 0 then [] else 
// //     (if pred(a[0]) then [a[0]] else []) + filterF(pred, a[1..])
// // }

method filterF<T>(pred: ((T) -> bool), a: seq<T>) returns (res: seq<T>)
    ensures |res| <= |a|
    ensures forall k :: 0 <= k < |res| ==> res[k] in a // no extraneous elements in res
    ensures forall k :: 0 <= k < |res| ==> pred(res[k])
    ensures forall k :: 0 <= k < |a| ==> (pred(a[k]) <==> a[k] in res)
    ensures forall k :: 0 <= k < |a| ==> pred(a[k]) ==> a[k] in res
    
{
    if |a| == 0 {
        return []; 
    } 
  
    var rem := filterF(pred, a[1..]);
    if pred(a[0])  {
      rem := rem + [a[0]];
    } 
    
    return rem;
}

function method mapF<T, S>(f: ((T) -> S), a: seq<T>): seq<S> 
  ensures |mapF(f, a)| == |a|
  ensures forall k :: 0 <= k < |a| ==> mapF(f, a)[k] == f(a[k])
{
  if |a| == 0 then [] else [f(a[0])] + mapF(f,a[1..])
}
