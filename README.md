# dafny-of-python
![tool overview](tool_overview.png)
*dafny-of-python* enables formal verification Python programs by translating a program written in a subset of Python along with its specification to the [Dafny](https://github.com/dafny-lang/dafny) verification language. Assuming the translation is correct, successful verification of the translated Dafny program implies that the same properties hold for the original Python program.

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

## Acknowledgements
This tool was developed as part of my final year project, with the valuable guidance of [Professor Chin Wei Ngan](https://www.comp.nus.edu.sg/cs/bio/chinwn/).

[Nice Parser](https://github.com/smolkaj/nice-parser) is used to provide beautiful parser error messages.
