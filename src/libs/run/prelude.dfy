method filterF<T>(pred: ((T) -> bool), a: List<T>) returns (res: List<T>)
  ensures fresh(res)
  ensures res.len() <= a.len()
  ensures forall k :: 0 <= k < res.len() ==> a.contains(res.atIndex(k)) // no extraneous elements in res
  ensures forall k :: 0 <= k < a.len() ==> (pred(a.atIndex(k)) <==> res.contains(a.atIndex(k))) // for any element in a, it must be in res iff the predicate holds
{
  var lst := new List<T>([]);
  var i := 0;
  assert lst.len() == i;
  var l := a.len();
  assert forall k :: 0 <= k < lst.len() ==> (pred(a.atIndex(k)) <==> lst.contains(a.atIndex(k)));
  while (i < l) 
    invariant 0 <= i <= l
    invariant 0 <= lst.len() <= i
    invariant forall k :: lst.contains(k) ==> pred(k)
    invariant forall k :: 0 <= k < lst.len() ==> a.contains(lst.atIndex(k))
    invariant forall k :: 0 <= k < i ==> (pred(a.atIndex(k)) <==> lst.contains(a.atIndex(k)))
  { 
    var e : T := a.atIndex(i);
    var lstO := lst.copy();
    assert forall k :: 0 <= k < lstO.len() ==> lst.atIndex(k) == lstO.atIndex(k);
    if pred(e) {
        lst.append(e);
        assert forall k :: 0 <= k < lst.len() - 1 ==> lst.atIndex(k) == lstO.atIndex(k);
    } else {
        assert !lst.contains(e);
    }
    
    i := i + 1;
  }

  return lst; 
}

method mapF<T, S(==)>(f: ((T) -> S), a: List<T>) returns (res: List<S>) 
  decreases a.len()
  ensures fresh(res)
  ensures res.len() == a.len()
  ensures forall k :: 0 <= k < a.len() ==> res.atIndex(k) == f(a.atIndex(k))
{  
  if a.len() == 0 {
    res := new List<S>([]);
  } else if a.len() == 1 {
    var mapped := f(a.atIndex(0));
    res := new List<S>([mapped]);
  } else {
    var mapped := f(a.atIndex(0));
    var rest := a.rangeLower(1);
    res := mapF(f, rest);
    assert forall k :: 1 <= k < a.len() ==> res.atIndex(k - 1) == f(a.atIndex(k));
    res.insert(0, mapped);
  }
}

method floor(n: real) returns (res: int) {
  return n.Floor;
}

// method Main()

// {
//   var a := (2.0 / 1.0);
// }
