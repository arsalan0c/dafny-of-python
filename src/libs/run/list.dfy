/* imperative implementation, based on Python's list: https://docs.python.org/3/tutorial/datastructures.html */

method list<T>(s: seq<T>) returns (l: List<T>) 
    ensures fresh(l)
    ensures l.lst == s
{
    return new List<T>(s);
}

class List<T(==)> {
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
    {
        lst[idx]
    }

    method copy() returns (l: List<T>)
        ensures fresh(l)
        ensures l.lst == lst
    {
        return new List(lst);
    }

    method range(low: int, high: int) returns (l: List<T>)
        requires 0 <= low <= high <= |lst|
        ensures fresh(l)
        ensures l.lst == lst[low..high]
    {
        return new List(lst[low..high]);
    }

    method rangeLower(low: int) returns (l: List<T>)
        requires 0 <= low < |lst|
        ensures fresh(l)
        ensures l.lst == lst[low..]
    {
        return new List(lst[low..]);
    }

    method rangeUpper(upper: int) returns (l: List<T>)
        requires 0 <= upper <= |lst|
        ensures fresh(l)
        ensures l.lst == lst[..upper]
    {
        return new List(lst[..upper]);
    }

    method rangeNone() returns (l: List<T>)
        ensures fresh(l)
        ensures l.lst == lst
    {
        return new List(lst);
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

    method equals(l2: List<T>) returns (res: bool)
        ensures res <==> lst == l2.lst
    {
        return lst == l2.lst;
    }

    method concat(l2: List<T>) returns (res: List<T>) 
        ensures fresh(res)
        ensures res.lst == lst + l2.lst
    {
        return new List<T>(lst + l2.lst);
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


method maxListInt(l: List<int>) returns (res: int)
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
