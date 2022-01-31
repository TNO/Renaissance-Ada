# Renaissance-Ada
Tooling for [ESI](https://esi.nl)'s Renaissance approach to [legacy software](https://en.wikipedia.org/wiki/Legacy_code) written in [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)).

The Renaissance-Ada project builds on top of [LibAdalang](https://adaco.re/libadalang) and includes the following functionality
* [Dependency Graph Extractor](/src/tools/Dependency_Graph_Extractor) that produces a [graphml](http://graphml.graphdrawing.org) file for visualization and querying 
with e.g. [yEd](https://www.yworks.com/products/yed) and [Neo4J](https://neo4j.com/).
* [Rejuvenation Library](/src/libraries/Rejuvenation_Lib) that 
allow analysis and manipulation of [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language))  code based on concrete patterns.
* [Rewriters Library](/src/libraries/Rewriters_Lib) that
enables automatic rewriting of [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)) code based on concrete patterns.
* [Code Reviewer](/src/tools/Code_Reviewer) that automatically reviews [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)) code 
based on a large list of rewrite rules.

The image below shows the depencies of Find-related subprograms of 
the [Rejuvenation Library](/src/libraries/Rejuvenation_Lib)
as extracted by the [Dependency Graph Extractor](/src/tools/Dependency_Graph_Extractor) and 
queried using [Neo4J](https://neo4j.com/).
![Dependencies of Find-related subprograms of the Rejuvenation Library](/doc/Relations_Of_Find.jpg)

## Used by Industry
[Nexperia](https://nexperia.com) described during [the AdaCore Tech Days](https://events.adacore.com/eutechday2021) 
[how they benefit from the Renaissance-Ada tooling](https://www.youtube.com/watch?v=EHrd-9wgALM).

## History
The Renaissance approach was initially developed by [ESI](https://esi.nl)
in public-private research projects together with [Thermo Fisher](https://thermofisher.com) and [Philips](http://philips.com).
For more info, see e.g. this 
[Bits & Chips article](https://bits-chips.nl/artikel/esi-helps-thermo-fisher-and-philips-grease-their-software-machines)
and [Model-based software transformation](https://esi.nl/research/output/methods/model-based-software-transformation).
The Renaissance tooling to target [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)) software
was developed in Bright, 
a public-private research project together with [ITEC](https://itecequipment.com), an independent subsidiary of [Nexperia](https://nexperia.com).

## Related technologies
* [HayStack-Ada](https://github.com/BurritoZz/Haystack-Ada) is a GNATStudio plug-in for AST-based Find and Replace. 
[HayStack-Ada](https://github.com/BurritoZz/Haystack-Ada) uses a re-implementation of the rejuvenation library in python.
