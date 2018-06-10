# Overview

DataBlock is a library to record & process a native JSon-like structure in scala.
The idea behind DataBlock is to experiment a memory structure that is much more compact than regular scala memory storage.

## Data structure

The JSon-like data structure is built around the following scheme :

```
DataNode ::= Key -> Value

Key      ::= String

Value    ::=   DataNode
             | List[RawValue]
             | RawValue
          
RawValue ::=   Boolean
             | Byte
             | Short
             | Int
             | Long
             | Double
             | String
```

## Memory consideration

All data handled through a DataNode is memorized through "pointers" to paginated arrays; even for the DataNode structure.
