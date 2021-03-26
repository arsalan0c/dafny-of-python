/* imperative implementation, based on Python's list: https://docs.python.org/3/tutorial/datastructures.html */

method test() returns (res: list<int>) {
    // var d := [1, 2, 3];
    // var a1 := new List(d);
    // var a := new List([a1]);
    // // assert 2 in d;
    // // var r := l.index(2);
    // // assert r == 1;
    // take(a1);
    // var b := [new List([1])];
    // return new List([1]);
    var a := newList([1, 2, 3]);
    return a;
}

method newList<T>(s: seq<T>) returns (l: list<T>) {
    return new list<T>(s);
}

class list<T(==)> {
    var lst: seq<T>

    // lst[|lst| - 1]
    // lst[1..]
    // lst[0 := 2]

    // a[0] => a.index(0)
    // a[0..10] => a.range(0, 10)
    // a = [1, 2, 3] => a = new list<int>([1, 2, 3]);
    constructor(l: seq<T>) {
        lst := l;
    }

    method append(e: T) 
        modifies this
        // ensures |lst| == |old(lst)| + 1
        // ensures forall i :: 0 <= i < |lst| - 1 ==> lst[i] == old(lst)[i]
        // ensures lst[|lst| - 1] == e
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
        // ensures idx < |lst| ==> lst[idx] == e
        // ensures idx < |lst| ==> forall i :: 0 <= i < idx ==> lst[i] == old(lst)[i]
        // ensures idx < |lst| ==> forall i :: idx < i < |lst| ==> lst[i] == old(lst)[i-1]
        ensures idx >= |lst| ==> lst == old(lst) + [e]
        // ensures idx >= |lst| ==> lst[|lst| - 1] == e
        // ensures idx >= |lst| ==> forall i :: 0 <= i < |lst| - 1 ==> lst[i] == old(lst)[i]
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
        ensures |lst| == |old(lst)| - 1
        ensures popped == old(lst)[|old(lst)| - 1]
        ensures forall i :: 0 <= i < |lst| ==> lst[i] == old(lst)[i]
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

    method atIndex(idx: int) returns (e: T)
        requires 0 <= idx < |lst|
        ensures e == lst[idx]
    {
        return lst[idx];
    }

    method count(e: T) returns (res: int)
        ensures res == multiset(lst)[e]
    {
        return multiset(lst)[e];
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

    // predicate sorted(s: seq<T>)
    // {
    //     forall i,j :: 0 <= i < j < |s| ==> s[i] <= s[j]
    // }

    // method sort()
    // {

    // }
}

