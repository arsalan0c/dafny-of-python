# pyny

## Usage
```
sudo dune exec src/bin/main.exe
```

## Example
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
