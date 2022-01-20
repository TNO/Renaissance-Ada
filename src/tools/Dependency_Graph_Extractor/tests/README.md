## How to test Dependency_Graph_Extractor

TODO: use old instructions, yet GraphStatistics and GraphDiff are not (yet) opensource!
use Renaissance-Ada instead of local projects (adat, itec).



=====================================================================================================================
set PATH=D:\bright\libadalang-install\bin;%PATH%
set GPR_PROJECT_PATH=D:\bright\libadalang-install\share\gpr;%GPR_PROJECT_PATH%
gprbuild -XLIBRARY_TYPE=static -Pdependency_graph_extractor.gpr

obj\dependency_graph_extractor.exe -o syntax_examples.graphml --no-project-recurse -p d:\bright\nexplore\ada-experiments\Syntax_Examples d:\bright\nexplore\ada-experiments\Syntax_Examples\syntax_examples.gpr > syntax_examples.txt
obj\dependency_graph_extractor.exe -o adat.graphml -p d:\bright\itecembed_source d:\bright\itecembed_source\Source\adat.gpr > adat.txt
obj\dependency_graph_extractor.exe -o itec.graphml -p d:\bright\itecembed_source d:\bright\itecembed_source\Source\itec.gpr > itec.txt

d:\DT-Renaissance\RenaissanceShared\Library\GraphTools\GraphDiff\bin\x64\Debug\GraphDiff.exe syntax_examples_gold.graphml syntax_examples.graphml
d:\DT-Renaissance\RenaissanceShared\Library\GraphTools\GraphDiff\bin\x64\Debug\GraphDiff.exe adat_gold.graphml adat.graphml
d:\DT-Renaissance\RenaissanceShared\Library\GraphTools\GraphDiff\bin\x64\Debug\GraphDiff.exe itec_gold.graphml itec.graphml
=====================================================================================================================
Variant

* use *.log instead of *.txt to ensure git ignores them

=== create golden standard

obj\dependency_graph_extractor.exe -o syntax_examples_gold.graphml --no-project-recurse -p c:\bright\git\ada-experiments\Syntax_Examples c:\bright\git\ada-experiments\Syntax_Examples\syntax_examples.gpr > syntax_examples_gold.log
obj\dependency_graph_extractor.exe -o adat_gold.graphml -p c:\bright\IFR c:\bright\IFR\Source\adat.gpr > adat_gold.log
obj\dependency_graph_extractor.exe -o itec_gold.graphml -p c:\bright\IFR c:\bright\IFR\Source\itec.gpr > itec_gold.log

=== create new graph

obj\dependency_graph_extractor.exe -o syntax_examples.graphml --no-project-recurse -p c:\bright\git\ada-experiments\Syntax_Examples c:\bright\git\ada-experiments\Syntax_Examples\syntax_examples.gpr > syntax_examples.log
obj\dependency_graph_extractor.exe -o adat.graphml -p c:\bright\IFR c:\bright\IFR\Source\adat.gpr > adat.log
obj\dependency_graph_extractor.exe -o itec.graphml -p c:\bright\IFR c:\bright\IFR\Source\itec.gpr > itec.log

== inspect

C:\WHaRS\rejuvenation\Library\GraphTools\GraphStatistics\bin\Debug\GraphStatistics.exe syntax_examples.graphml
C:\WHaRS\rejuvenation\Library\GraphTools\GraphStatistics\bin\Debug\GraphStatistics.exe adat.graphml
C:\WHaRS\rejuvenation\Library\GraphTools\GraphStatistics\bin\Debug\GraphStatistics.exe itec.graphml

=== compare

C:\WHaRS\rejuvenation\Library\GraphTools\GraphDiff\bin\Debug\GraphDiff.exe syntax_examples_gold.graphml syntax_examples.graphml > graphdiff.syntax_examples.log
C:\WHaRS\rejuvenation\Library\GraphTools\GraphDiff\bin\Debug\GraphDiff.exe adat_gold.graphml adat.graphml > graphdiff.adat.log
C:\WHaRS\rejuvenation\Library\GraphTools\GraphDiff\bin\Debug\GraphDiff.exe itec_gold.graphml itec.graphml > graphdiff.itec.log
