# Rejuvenation Find And Replace

Find matches of the given code snippet 
and replace with the other given code snippet
in your project.

We will refer to the given code snippet as the find pattern
and the other given code snippet as the replace pattern.

Both find and replace are based on the [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
So the process is independent of layout and presence of comment.

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

## wild cards in replace patterns

A wild card in a replace pattern is always refers to that wild card in the find pattern.
The wild card in the replace pattern will be replaced by the value of that the wild card in the match of the find pattern.
In analogy with [Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression), 
all wild cards in a replace patterns can be called backreferences.

## examples

See [predefined_rewriters.ads](https://github.com/TNO/Renaissance-Ada/blob/main/src/libraries/Rewriters_Lib/src/predefined_rewriters.ads)
for many examples.
