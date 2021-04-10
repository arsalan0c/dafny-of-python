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
        ensures lst == l
        ensures fresh(this)
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
        ensures e in lst
        ensures idx < |lst| ==> lst == old(lst)[0..idx] + [e] + old(lst)[idx..]
        ensures idx >= |lst| ==> lst == old(lst) + [e]
       
    {
        if idx >= |lst| {
            lst := lst + [e];
            return;
        }

        lst := lst[0..idx] + [e] + lst[idx..];
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

    method contains(e: T) returns (res: bool) 
        ensures e in lst <==> res
    {
        return e in lst;
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
