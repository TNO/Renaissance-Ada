# Rejuvenation Library

The Rejuvenation Library enables analysis and manipulation of Ada code based on concrete patterns.
Both find- and replace-functionality are based on the [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
of the concrete patterns.
Wild cards are allowed in these concrete patterns.

## wild cards

There are two types of wildcards, those starting with "$S_" and those starting with "$M_".
Any alphanumeric string is allowed to follow after those prefixes.

"$S_" wildcards allow one to match a single AST node, be that a single expression, a single statement, a single argument, or anything else.
As long as Ada parses it to a single AST node, the "$S_" wildcard can match it.

"$M_" wildcards allow one to match a list of AST nodes, i.e., zero or more nodes. 

### wild cards in find patterns

A find pattern might contain multiple different wild cards.

A find pattern might contain the same wild card multiple times.
This add a constraint to the find process:
A match will only be found when all occurrence of the same wild card are identical.
Note that universities are still researching what the best definition of identical is.
In analogy with [Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression), 
the term backreference is used to denote a wild card that reoccurs.

### wild cards in replace patterns

A wild card in a replace pattern always refers to that wild card in the find pattern.
A wild card in a replace pattern that does not occur in the find pattern is thus an error.
The wild card in the replace pattern will be replaced by the value of that wild card in the match of the find pattern.
In analogy with [Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression), 
all wild cards in a replace pattern can be called backreferences.

# Examples
See the [test project](test_driver.gpr), the [examples](../../examples/Rejuvenation_Examples), 
and the [workshop](../../examples/Rejuvenation_Workshop) for inspiration to use the Rejuvenation Library.

# Tools made using Rejuvenation Library
* [Rejuvenation Find](../../tools/Rejuvenation_Find) finds a pattern in a given project
* [Rejuvenation Find And Replace](../../tools/Rejuvenation_Find_And_Replace) finds a pattern and replaces it with another in a given project