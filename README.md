# Renaissance-Ada
The Renaissance-Ada project develops tooling for analysis and manipulation of [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)) software.
The Renaissance-Ada project builds on top of [LibAdalang](https://adaco.re/libadalang) and includes the following functionality
* [Dependency Graph Extractor](https://github.com/TNO/Dependency_Graph_Extractor-Ada) 
that produces a [graphml](http://graphml.graphdrawing.org) file for visualization and querying 
with e.g. [yEd](https://www.yworks.com/products/yed) and [Neo4J](https://neo4j.com/).
* [Rejuvenation Library](https://github.com/TNO/Rejuvenation-Ada) that 
allow analysis and manipulation of [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language))  code based on concrete syntax.
* [Rewriters Library](/src/libraries/Rewriters) that
enables automatic rewriting of [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)) code based on concrete syntax.
* [Code Reviewer](/src/tools/Code_Reviewer) that automatically reviews [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)) code 
based on a large list of rewrite rules.

## Examples

The image below shows the dependencies of Find-related subprograms of 
the [Rejuvenation Library](https://github.com/TNO/Rejuvenation-Ada)
as extracted by the [Dependency Graph Extractor](https://github.com/TNO/Dependency_Graph_Extractor-Ada) and 
queried using [Neo4J](https://neo4j.com/).
![Dependencies of Find-related subprograms of the Rejuvenation Library](/doc/Relations_Of_Find.jpg)

Snippets from diff made with [Code Reviewer](/src/tools/Code_Reviewer)
```diff
   function Release_Only (Mode : Operation_Mode) return Boolean is
-     (case Mode is when Release_Size_Mode | Release_Speed_Mode => True, when others => False);
+     (Mode in Release_Size_Mode | Release_Speed_Mode);
```

```diff
- if Valid then
-    Add (Value, 0, 0, 0);
- else
-    Add ("", 0, 0, 0);
- end if;
+ Add ((if Valid then Value else ""), 0, 0, 0);
```

```diff
- for Acf of Acfs loop
-    if Acf = null then
-       return False;
-    end if;
- end loop;
- return True;
+ return (for all Acf of Acfs => Acf /= null);
```

Example based on [aws](https://github.com/AdaCore/aws/blob/7488c0f6f4c593b51e8b61b94d245e2ff4896e33/config/ssl/aws-net-ssl__openssl.adb#L215) code
```diff
-   Max_Overhead : Stream_Element_Count range 0 .. 2**15 := 81 with Atomic;
-   for Max_Overhead'Size use 16;
+   Max_Overhead : Stream_Element_Count range 0 .. 2**15 := 81 with
+      Atomic,
+      Size => 16;
```

## Used by Industry
[Nexperia](https://nexperia.com) described during [the AdaCore Tech Days](https://events.adacore.com/eutechday2021) 
how [they benefit from the Renaissance-Ada tooling](https://www.youtube.com/watch?v=EHrd-9wgALM).

## Renaissance History

The Renaissance approach to [legacy software](https://en.wikipedia.org/wiki/Legacy_code) was initially developed by [ESI](https://esi.nl)
in public-private research projects together with [Thermo Fisher](https://thermofisher.com) and [Philips](http://philips.com).

![Enhance insight and reduce complexity](/doc/enhance-insight-reduce-complexity.jpg)

The Renaissance approach is an interative process of
two steps that strengthen each other
* enhance insight by analysis and 
* reduce complexity by manipulation.

For more info, see e.g. this 
[Bits & Chips article](https://bits-chips.nl/artikel/esi-helps-thermo-fisher-and-philips-grease-their-software-machines)
and [ESI's research on model-based software transformation](https://esi.nl/research/output/methods/model-based-software-transformation).

The development of Renaissance tooling to target [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)) software
started in Bright, a public-private research project together with [ITEC](https://itecequipment.com), 
an independent subsidiary of [Nexperia](https://nexperia.com).

## Related technologies
* [HayStack-Ada](https://github.com/BurritoZz/Haystack-Ada) is a GNATStudio plug-in for AST-based Find and Replace. 
[HayStack-Ada](https://github.com/BurritoZz/Haystack-Ada) uses a re-implementation of the rejuvenation library in python.
* [OpenRewrite](https://docs.openrewrite.org) is a semantic code search and transformation ecosystem for Java and other source code.
* [Rascal MPL](https://www.rascal-mpl.org) is a metaprogramming language that 
integrates source code analysis, transformation, and generation primitives on the language level.
Unfortunately, [Rascal MPL](https://www.rascal-mpl.org) still lacks support for [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)).
* [Spoofax](https://spoofax.dev) is a language designer's workbench. [Spoofax](https://spoofax.dev) supports [concrete syntax](https://www.spoofax.dev/howtos/stratego/concrete-syntax) to specify code transformations.
Unfortunately, [Spoofax](https://spoofax.dev) still lacks support for [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)).
