type 'a tree =
    | Leaf
    | Node of 'a node

and 'a node = {
    name: 'a;
    value: int;
    children:  'a tree list;
    mutable subtree_weight: int option;
    mutable root_weight: int option
}
