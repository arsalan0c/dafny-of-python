# dafny-of-python
A tool to statically verify a program written in a subset of Python. 


![tool overview](tool_overview.png)
## Architecture
*dafny-of-python* translates a Python program as well as a specification annotated in comments to the [Dafny](https://github.com/dafny-lang/dafny) verification language.

## Requirements
-mypy
-niceparser
-sexp jane street

## Overview

## Language
- mypy
- python subset + types
- specifications: res


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
