# dafny-of-python

![tool overview](tool_overview.png)
*dafny-of-python* enables formal verification of Python programs. It does so by translating a program written in a [subset of typed Python](https://github.com/arsalanc-v2/dafny-of-python/wiki/Language) along with its specification to the [Dafny](https://github.com/dafny-lang/dafny) verification language. 

Given the following program annotated with types and a specification:
```Python
# pre True
# post res == x * x * x
def cube(x: int) -> int:
  return x * x * x
```

The following Dafny program is generated:
```
function method cube(x: int): (res: int)
  requires true
  ensures (res == ((x * x) * x))
{
  ((x * x) * x)
}
```

Along with the outcome of verification:
```
verifier finished with 1 verified, 0 error(s)
```

As the specifications are written in comments, Python programs can remain executable without modification. Assuming the translation is correct, successful verification of the translated Dafny program implies that the same properties hold for the original Python program. While the aim is to prevent knowledge of Dafny from being essential, it would certainly help. You can see additional examples [below](#examples).

## Requirements
- mypy
- dafny
- niceparser
- sexp jane street
- obelisk
- re2
- menhir, ocamllex

## Usage
```
sudo dune exec src/bin/main.exe < [file].py
```
## Examples

### Finding the index of an element in a list
```Python
# post 0 <= res ==> res < len(a) and a[res] == key
# post res == -1 ==> forall k :: 0 <= k and k < len(a) ==> a[k] != key
def find(a: list[int], key: int) -> int:
  index = 0
  # invariant 0 <= index and index <= len(a)
  # invariant forall k :: 0 <= k and k < index ==> a[k] != key
  while index < len(a):
    if a[index] == key:
      return index
    
    index += 1
  
  return -1
```

### Contributing
If you find a bug or have any comments, feel free to open an [issue](https://github.com/arsalanc-v2/dafny-of-python/issues/new/choose) or pull request.

## Credits
*dafny-of-python* was developed as part of my final year project at the National University of Singapore, with the valuable guidance of [Professor Chin Wei Ngan](https://www.comp.nus.edu.sg/cs/bio/chinwn/).

- [Nice Parser](https://github.com/smolkaj/nice-parser) is used to provide beautiful parser error messages.
- [Obelisk](https://github.com/Lelio-Brun/Obelisk) is used to pretty-print the source language grammar.
## Related Work
- [Nagini](https://github.com/marcoeilers/nagini), a static verification tool for Python using [Viper](http://viper.ethz.ch/)
- [CrossHair](https://github.com/pschanely/CrossHair), a static verification tool for Python using symbolic execution
- [H2D](http://www.doc.ic.ac.uk/~dcw/h2d.cgi), a compiler from Haskell to Dafny
- [coq-of-ocaml](https://github.com/clarus/coq-of-ocaml), a compiler from OCaml to Coq
- [goose](https://github.com/tchajed/goose), a compiler from Go to Coq

