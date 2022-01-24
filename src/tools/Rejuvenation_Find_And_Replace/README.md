# Rejuvenation Find And Replace

Find the given find pattern, a.k.a. code snippet, 
and replace with the given replace pattern 
in your project.

Find matches code based on their AST.
So it is independent of layout and presence of comment.

## wild cards

There are two types of wildcards, those starting with "$S_" and those starting with "$M_".
Any alphanumeric string is allowed to follow after those prefixes.

"$S_" wildcards allow one to match a single thing, be that a single expression, a single statement or anything else.
As long as Ada parses it to a single AST node, the "$S_" wildcard can match it.

"$M_" wildcards allow one to match a list of items, i.e., zero or more items. 

## back references

A back reference in a find pattern is a wild card that is used multiple times.
All occurance of a back reference must be identical.
Of course, multiple different back references can be used in a single find pattern.

A back reference in a replace pattern is a wild card that is also used in the associated find pattern.
The value associated with the wild card in the match, will be replaced for that wild card in the replace pattern.


See [predefined_rewriters.ads](https://github.com/TNO/Renaissance-Ada/blob/main/src/libraries/Rewriters_Lib/src/predefined_rewriters.ads)
for many examples.
