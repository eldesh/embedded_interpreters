
Embedded Interpreters
================================================================

A sample implementation of the "Embedded Interpreters"[benton05]_ techniques on SML/NJ.
That paper describe many interesting DSL techniques, but that not includes whole source code for working on real SML implementations. e.g. fragment parser named *%*.

Also the contents of this repository is written for coterie book `解説 Embedded Interpreters`_ . This book is scheduled for publishing in `技術書典3`_ (techbookfesta3).


Environment
----------------------------------------------------------------

The core technique which is introduced by the paper depends on proprietary extension *Quote/Antiquote*.
These 2 implementations provide Quote/Antiquote extension.

- SML/NJ
- MoscowML



.. [benton05] BENTON, N. (2005). Embedded interpreters. Journal of Functional Programming, 15(4), 503-542. doi:10.1017/S0956796804005398

.. _`技術書典3`: https://techbookfest.org/event/tbf03
.. _`解説 Embedded Interpreters`: contact to @eldesh (http://twitter.com/eldesh)

