# Template processor
PP use [HStringTemplate](https://hackage.haskell.org/package/HStringTemplate) for the template compilation, thus the template language is the one used by [StringTemplate](https://github.com/antlr/stringtemplate4/blob/master/doc/cheatsheet.md).

The accessible context into the template files are instances of the `PP.Template` type class (defined in modules `PP.Templates.*`). Take a look at their documentation on Hackage for more details.
