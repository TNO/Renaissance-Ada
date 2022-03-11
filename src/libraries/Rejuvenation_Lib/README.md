# Rejuvenation Library

The Rejuvenation Library enables analysis and manipulation of Ada code based on concrete patterns.
Both find- and replace-functionality are based on the [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
of the concrete patterns.
Placeholders are allowed in these concrete patterns.

## placeholders

There are two types of placeholders, those starting with "$S_" and those starting with "$M_".
Any alphanumeric string is allowed to follow after those prefixes.

"$S_" placeholders allow one to match a single AST node, be that a single expression, a single statement, a single argument, or anything else.
As long as Ada parses it to a single AST node, the "$S_" placeholders can match it.

"$M_" placeholders allow one to match a list of AST nodes, i.e., zero or more nodes.

Note that the current implementation is greedy with respect to placeholders.
Whenever one could proceed to the next placeholder this will happen.
So all matches in the current implementation of `$M_X; $S_T;` will always have an empty list for `$M_X`.

### placeholders in find patterns

A find pattern might contain multiple different placeholders.

A find pattern might contain the same placeholders multiple times.
This add a constraint to the find process:
A match will only be found when all occurrence of the same placeholders are identical.
Note that universities are still researching what the best definition of identical is.
In analogy with [Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression), 
the term backreference is used to denote a placeholders that reoccurs.

### placeholders in replace patterns

A placeholders in a replace pattern always refers to that placeholders in the find pattern.
A placeholders in a replace pattern that does not occur in the find pattern is thus an error.
The placeholders in the replace pattern will be replaced by the value of that placeholders in the match of the find pattern.
In analogy with [Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression), 
all placeholders in a replace pattern can be called backreferences.

# Examples
See the [examples](../../examples/Rejuvenation_Examples), 
the [workshop](../../examples/Rejuvenation_Workshop), and the test cases in the [test project](test_driver.gpr)
for inspiration to use the Rejuvenation Library.

# Tools made using Rejuvenation Library
* [Rejuvenation Find](../../tools/Rejuvenation_Find) finds a pattern in a given project
* [Rejuvenation Find And Replace](../../tools/Rejuvenation_Find_And_Replace) finds a pattern and replaces it with another in a given project
