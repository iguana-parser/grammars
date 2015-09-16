This repository contains grammars of proramming languages for Iguana. The grammars are extracted from their corresponding language specification manual. For some grammars, we also provide a version of the grammar that has a natural expression grammar (Natural Grammar) which is declaratively disambiguated.

At the moment, we have the following grammars:

- Java 7: [Specification Grammar](https://github.com/iguana-parser/grammars/blob/master/src/java/JavaSpecification.rsc), [Natural grammar](https://github.com/iguana-parser/grammars/blob/master/src/java/JavaNatural.rsc)
- C# 5 [Specification Grammar](https://github.com/iguana-parser/grammars/blob/master/src/csharp/specification/CSharp.rsc)
- C [Specification Grammar](https://github.com/iguana-parser/grammars/blob/master/src/c/specification/C.rsc)
- OCaml 4.002 [Specification Grammar](https://github.com/iguana-parser/grammars/blob/master/src/ocaml/specification/OCaml.rsc)
- Haskell [Specification Grammar](https://github.com/iguana-parser/grammars/blob/master/src/haskell/specification/Haskell.rsc), [Indentation-sensitive Grammar](https://github.com/iguana-parser/grammars/blob/master/src/haskell/datadependent/HaskellDD.rsc)
- XML [Specification Grammar] (https://github.com/iguana-parser/grammars/blob/master/src/xml/XML.rsc)
- Python [Specification Grammar](https://github.com/iguana-parser/grammars/blob/master/src/python/specification/Python.rsc) Work in progress.
- Scala [Specification Grammar](https://github.com/iguana-parser/grammars/blob/master/src/scala/specification/Scala.rsc) Work in progress.

The grammars are written in [Rascal](http://www.rascal-mpl.org/). The integration of Iguana into Rascal is a work in progress. To use the Iguana backend for Rascal, you need the [Iguana](https://github.com/cwi-swat/rascal/tree/iguana) branch of Rascal.
We will gradually update this page on how to use Iguana grammars.
