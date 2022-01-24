# Rejuvenation Find

Find the given pattern, a.k.a. code snippet, in your project.

Find matches code based on their AST.
So it is independent of layout and presence of comment.

## wild cards

There are two types of wildcards, those starting with "$S_" and those starting with "$M_".
Any alphanumeric string is allowed to follow after those prefixes.

"$S_" wildcards allow one to match a single thing, be that a single expression, a single statement or anything else.
As long as Ada parses it to a single AST node, the "$S_" wildcard can match it.

"$M_" wildcards allow one to match a list of items, i.e., zero or more items. 

## back references

A back reference is a wild card that is used multiple times in a search pattern.
All occurance of a back reference must be identical.
Of course, multiple different back references can be used in a single search pattern.

See [predefined_rewriters.ads](https://github.com/TNO/Renaissance-Ada/blob/main/src/libraries/Rewriters_Lib/src/predefined_rewriters.ads)
for many examples.