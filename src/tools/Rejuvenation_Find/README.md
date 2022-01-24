# Rejuvenation Find

Find matches of the given code snippet in your project.

We will refer to the given code snippet as the find pattern.

Find matches code based on their [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
So it is independent of layout and presence of comment.


## wild cards

There are two types of wildcards, those starting with "$S_" and those starting with "$M_".
Any alphanumeric string is allowed to follow after those prefixes.

"$S_" wildcards allow one to match a single AST node, be that a single expression, a single statement, a single argument, or anything else.
As long as Ada parses it to a single AST node, the "$S_" wildcard can match it.

"$M_" wildcards allow one to match a list of AST nodes, i.e., zero or more nodes. 

## wild cards in find patterns

A find pattern might contain multiple different wild cards.

A find pattern might contain the same wild card multiple times.
This add a constraint to the find process:
A match will only be found when all occurrence of the same wild card are identical.
Note that universities are still researching what the best definition of identical is.
In analogy with [Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression), 
the term backreference is used to denote a wild card that reoccurs.

## examples

See [predefined_rewriters.ads](https://github.com/TNO/Renaissance-Ada/blob/main/src/libraries/Rewriters_Lib/src/predefined_rewriters.ads)
for many examples.