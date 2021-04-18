/* imperative implementation, based on Python's list: https://docs.python.org/3/tutorial/datastructures.html */

method newList<T>(s: seq<T>) returns (l: list<T>) 
    ensures fresh(l)
    ensures l.lst == s
{
    return new list<T>(s);
}

class list<T(==)> {
    var lst: seq<T>

    constructor(l: seq<T>)
        ensures fresh(this)
        ensures lst == l
        ensures forall k :: 0 <= k < |lst| ==> lst[k] == l[k]
    {
        lst := l;
    }

    method append(e: T) 
        modifies this
        ensures lst == old(lst) + [e]
    {
        lst := lst + [e];
    }

    method insert(idx: int, e: T) 
        requires 0 <= idx
        modifies this
        ensures |lst| == |old(lst)| + 1
        ensures multiset(lst)[e] == multiset(old(lst))[e] + 1 // count of e is increased by 1
        ensures forall k :: 0 <= k < |old(lst)| && lst[k] != e ==> count(old(lst)[k]) == (old(this)).count(old(lst)[k]) // count of all other elements is unchanged
        ensures idx < |lst| ==> lst == old(lst)[0..idx] + [e] + old(lst)[idx..]
        ensures idx >= |lst| ==> lst == old(lst) + [e]
        ensures idx < |lst| ==> lst[idx] == e
        ensures forall k :: 0 <= k < |old(lst)| ==> old(lst)[k] in lst 
        ensures forall k :: 0 <= k < |lst| && lst[k] != e ==> lst[k] in old(lst) 
    {
        if idx >= |lst| {
            var n := count(e);
            lst := lst + [e];
            return;
        }

        assert lst == lst[0..idx] + lst[idx..];
        var i := 0;
        var res := lst[0..idx];
        res := res + [e];
        res := res + lst[idx..];        
        lst := res;
    }

    function method removeIndex(e: T, lst: seq<T>): int
        requires e in lst
        ensures 0 <= removeIndex(e, lst) < |lst|
        ensures lst[removeIndex(e, lst)] == e
        ensures forall i :: 0 <= i < removeIndex(e, lst) ==> lst[i] != e
    {
        if lst[0] == e then 0 else 1 + removeIndex(e, lst[1..])
    }

    method remove(e: T)
        requires e in lst
        modifies this
        ensures |lst| == |old(lst)| - 1
        ensures forall i :: 0 <= i < removeIndex(e, old(lst)) ==> lst[i] == old(lst)[i]
        ensures forall i :: removeIndex(e, old(lst)) < i < |lst| ==> lst[i] == old(lst)[i+1]
    {
        var i := 0;
        while i < |lst| 
            invariant 0 <= i <= |lst|
            invariant forall k :: 0 <= k < i ==> lst[k] != e
        {
            if lst[i] == e {
                break;
            } 

            i := i + 1;
        }

        if i >= |lst| - 1 {
            lst := lst[0..i];
        } else {
            lst := lst[0..i] + lst[i + 1..];
        }
    }

    method pop() returns (popped: T)
        requires |lst| > 0
        modifies this
        ensures lst == old(lst)[0..|old(lst)| - 1]
        ensures popped == old(lst)[|old(lst)| - 1]
    {
        popped := lst[|lst| - 1];
        lst := lst[0..|lst| - 1];
    }

    method clear() 
        modifies this
        ensures |lst| == 0
    {
        lst := [];
    }

    method index(e: T) returns (idx: int)
        requires e in lst
        ensures 0 <= idx < |lst|
        ensures lst[idx] == e
        ensures forall k :: 0 <= k < idx ==> lst[k] != e
    {
        var i := 0;
        while i < |lst| 
            invariant 0 <= i <= |lst|
            invariant forall k :: 0 <= k < i ==> lst[k] != e
        {
            if lst[i] == e {
                return i;
            }

            i := i + 1;
        }
    }

    function method atIndex(idx: int): (e: T)
        reads this
        requires 0 <= idx < |lst|
        ensures e == lst[idx]
        ensures this.contains(e)
    {
        lst[idx]
    }

    method range(low: int, high: int) returns (l: list<T>)
        requires 0 <= low <= high <= |lst|
        ensures fresh(l)
        ensures l.lst == lst[low..high]
    {
        return new list(lst[low..high]);
    }

    method rangeLower(low: int) returns (l: list<T>)
        requires 0 <= low < |lst|
        ensures fresh(l)
        ensures l.lst == lst[low..]
    {
        return new list(lst[low..]);
    }

    method rangeUpper(upper: int) returns (l: list<T>)
        requires 0 <= upper <= |lst|
        ensures fresh(l)
        ensures l.lst == lst[..upper]
    {
        return new list(lst[..upper]);
    }

    method rangeNone() returns (l: list<T>)
        ensures fresh(l)
        ensures l.lst == lst
    {
        return new list(lst);
    }

    function method len(): (l: int) 
        reads this
        ensures l == |lst|
    {
        |lst|
    }

    function method count(e: T): (res: int)
        reads this
        ensures res == multiset(lst)[e]
    {
        multiset(lst)[e]
    }

    method reverse() 
        modifies this
        ensures |lst| == |old(lst)|
        ensures forall i :: 0 <= i < |lst| ==> lst[i] == old(lst)[|lst| - i - 1]
    {
        var newLst := [];
        while |newLst| < |lst| 
            invariant 0 <= |newLst| <= |lst|
            invariant forall j :: 0 <= j < |newLst| ==> newLst[j] == lst[|lst| - j - 1]
        {
            var i := |newLst|;
            var ej := lst[|lst| - i - 1];
            newLst := newLst + [ej];
        }

        lst := newLst;
    }

    function method contains(e: T): (res: bool)
        reads this
        ensures e in lst <==> res
    {
        e in lst
    }

    method equals(l2: list<T>) returns (res: bool)
        ensures res <==> lst == l2.lst
    {
        return lst == l2.lst;
    }

    method concat(l2: list<T>) returns (res: list<T>) 
        ensures fresh(res)
        ensures res.lst == lst + l2.lst
    {
        return new list<T>(lst + l2.lst);
    }

    // predicate sorted(s: seq<T>)
    // {
    //     forall i,j :: 0 <= i < j < |s| ==> s[i] <= s[j]
    // }

    // method sort()
    //     modifies this
    //     ensures sorted(lst)
    //     ensures |lst| == |old(lst)|
    //     ensures forall i :: 0 <= i < |lst| ==> count(lst[i]) == old(this).count(lst[i])
    // {

    // }
}

// method filterF<T>(pred: ((T) -> bool), a: list<T>) returns (res: list<T>)
//     decreases a.len()
//     ensures fresh(res)
//     ensures res.len() <= a.len()
//     ensures forall k :: 0 <= k < res.len() ==> pred(res.atIndex(k)) // predicate holds for every element in res 
//     ensures forall k :: 0 <= k < a.len() ==> (pred(a.atIndex(k)) <==> res.contains(a.atIndex(k))) // for any element in a, it must be in res iff the predicate holds
//     ensures forall k :: 0 <= k < res.len() ==> a.contains(res.atIndex(k)) // no extraneous elements in res
//     //ensures forall k :: 0 <= k < res.len() ==> res.count(res.atIndex(k)) <= a.count(res.atIndex(k)) // count of each element in res should be lte than that in a
// {
//     if a.len() == 0 { 
//         res := new list<T>([]); 
//     } else if a.len() == 1 {
//         res := new list<T>([]);
//         var e := a.atIndex(0);
//         if pred(e)  {
//             res.insert(0, e);
//         }
//     } else {
//         var rest := a.rangeLower(1);
//         assert forall k :: 1 <= k < a.len() ==> rest.contains(a.atIndex(k));
//         assert forall k :: 1 <= k < a.len() ==> a.atIndex(k) == rest.atIndex(k - 1);
//         res := filterF(pred, rest);
//         assert forall k :: 0 <= k < res.len() ==> a.contains(res.atIndex(k));
//         assert forall k :: 0 <= k < res.len() ==> pred(res.atIndex(k));
//         assert res.len() <= rest.len();
//         assert forall k :: 0 <= k < rest.len() ==> (pred(rest.atIndex(k)) <==> res.contains(rest.atIndex(k)));
//         assert forall k :: 0 <= k < rest.len() ==> a.atIndex(k + 1) == rest.atIndex(k);
//         assert forall k :: 1 <= k < a.len() ==> (pred(a.atIndex(k)) <==> res.contains(a.atIndex(k)));
//         //assert forall k :: 0 <= k < res.len() ==> res.count(res.atIndex(k)) <= a.count(res.atIndex(k));

//         var e := a.atIndex(0);
//         assert a.contains(e);
//         var n := res.len();
//         var resO; 
//         if (res.len() == 0) {
//             resO := new list<T>([]);
//         } else {
//             resO := res.rangeLower(0);
//         }

//         assert forall k :: 0 <= k < resO.len() ==> resO.atIndex(k) == res.atIndex(k);
//         if pred(e) {
//             res.insert(0, e);
//             assert forall k :: 1 <= k < res.len() ==> res.atIndex(k) == resO.atIndex(k - 1);
//             assert forall k :: 0 <= k < a.len() ==> (pred(a.atIndex(k)) <==> res.contains(a.atIndex(k)));
//             //assert forall k :: 0 <= k < res.len() ==> res.count(res.atIndex(k)) <= a.count(res.atIndex(k));
//         } else {
//             assert !pred(e);
//             assert forall k :: 0 <= k < res.len() ==> pred(res.atIndex(k)) && res.contains(res.atIndex(k));
//             assert exists h :: !res.contains(h) || pred(h);
//             assert !res.contains(e) || pred(e);
//         }
//         assert forall k :: 0 <= k < a.len() ==> (pred(a.atIndex(k)) <==> res.contains(a.atIndex(k)));
//     }

//     assert forall k :: 0 <= k < res.len() ==> a.contains(res.atIndex(k));
//     assert forall k :: 0 <= k < res.len() ==> pred(res.atIndex(k));
//     assert forall k :: 0 <= k < a.len() ==> (pred(a.atIndex(k)) <==> res.contains(a.atIndex(k)));
//     //assert forall k :: 0 <= k < res.len() ==> res.count(res.atIndex(k)) <= a.count(res.atIndex(k));
// }

// method mapF<T, S(==)>(f: ((T) -> S), a: list<T>) returns (res: list<S>) 
//   decreases a.len()
//   ensures fresh(res)
//   ensures res.len() == a.len()
//   ensures forall k :: 0 <= k < a.len() ==> res.atIndex(k) == f(a.atIndex(k))
// {  
//     if a.len() == 0 {
//         res := new list<S>([]);
//     } else if a.len() == 1 {
//         var mapped := f(a.atIndex(0));
//         res := new list<S>([mapped]);
//     } else {
//         var mapped := f(a.atIndex(0));
//         var rest := a.rangeLower(1);
//         res := mapF(f, rest);
//         assert forall k :: 1 <= k < a.len() ==> res.atIndex(k - 1) == f(a.atIndex(k));
//         res.insert(0, mapped);
//     }
// }


// method range(start: int, stop: int, step: int) returns (res: seq<int>)
//     requires start == 0
//     requires step == 1
//     requires stop > start
//     ensures |res| == stop - start
//     ensures forall k :: start <= k < stop ==> res[k - start] == k
// {
//     var i := start;
//     var a := [];
//     while i < stop 
//         invariant start <= i <= stop + step - 1
//         invariant |a| == (i - start) / step
//         invariant forall k :: start <= k < i ==> a[k - start] == k
//     {
//         a := a + [i];
//         i := i + step
//     }

//     return a;
// }

// method maxInts<T>(l: seq<int>) returns (res: int)
//     requires |l| > 0
//     ensures forall k :: 0 <= k < |l| ==> res >= l[k] 
// {
//     var i := 0;
//     var soFar := l[i];
//     while i < |l| 
//         invariant 0 <= i <= |l|
//         invariant forall k :: 0 <= k < i ==> soFar >= l[k] 
//     {
//         if l[i] > soFar {
//             soFar := l[i];
//         }
//         i := i + 1;
//     }

//     return soFar;
// }

// method maxReals<T>(l: seq<real>) returns (res: real)
//     requires |l| > 0
//     ensures forall k :: 0 <= k < |l| ==> res >= l[k] 
// {
//     var i := 0;
//     var soFar := l[i];
//     while i < |l| 
//         invariant 0 <= i <= |l|
//         invariant forall k :: 0 <= k < i ==> soFar >= l[k] 
//     {
//         if l[i] > soFar {
//             soFar := l[i];
//         }
//         i := i + 1;
//     }

//     return soFar;
// }


method maxListInt(l: list<int>) returns (res: int)
    requires l.len() > 0
    ensures forall k :: 0 <= k < l.len() ==> res >= l.atIndex(k)
{
    var i := 0;
    var soFar := l.atIndex(i);
    while i < l.len()
        invariant 0 <= i <= l.len()
        invariant forall k :: 0 <= k < i ==> soFar >= l.atIndex(k)
    {
        if l.atIndex(i) > soFar {
            soFar := l.atIndex(i);
        }
        i := i + 1;
    }

    return soFar;
}

// method maxListReal<T>(l: list<real>) returns (res: real)
//     requires l.len() > 0
//     ensures forall k :: 0 <= k < l.len() ==> res >= l.atIndex(k)
// {
//     var i := 0;
//     var soFar := l.atIndex(i);
//     while i < l.len()
//         invariant 0 <= i <= l.len()
//         invariant forall k :: 0 <= k < i ==> soFar >= l.atIndex(k)
//     {
//         if l.atIndex(i) > soFar {
//             soFar := l.atIndex(i);
//         }
//         i := i + 1;
//     }

//     return soFar;
// }




// method search(x: int, s: list<int>) returns (res1: int)
//   requires (s.len() > 0)
//   requires forall k, j :: ((((0 <= k) && (k < j)) && (j < s.len())) ==> (s.atIndex(k) <= s.atIndex(j)))
//   ensures ((res1 >= 0 && res1 < s.len()) ==> (x <= s.atIndex(res1)))
// {
//   var x: int := x;
//   var s: list<int> := s;
//   var output: int := 0;
//   var tempcall_1 := s.len();
//   var l: int := tempcall_1;
//   while (output < l)
//     invariant ((0 <= output) && (output <= l))
//     invariant forall k :: (((0 <= k) && (k < output)) ==> (x > s.atIndex(k)))
//   {
//     var tempcall_2 := s.atIndex(output);
//     if (x > tempcall_2) {
//       output := (output + 1);
//     } else {
//       break;    
//     }
//   }
//   return output;
// }

