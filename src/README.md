Sources Overview
================

* [`SH`](sh.ads): Base of SH package
* [`SH.Annotated_Strings`](sh-annotated_strings.ads): String structure used during substitution
* [`SH.Buffers`](sh-buffers.ads): API to access script buffers 
* [`SH.Commands`](sh-commands.ads): Simple commands launcher
* [`SH.Functions`](sh-functions.ads): Shell functions execution
* [`SH.Lexer`](sh-lexer.ads): Shell lexer
* [`SH.List_Pools`](sh-list_pools.ads): List structure used in SH.Tree
* [`SH.Opts`](sh-opts.ads): GSH command line arguments parsing
* [`SH.Parser`](sh-parser.ads): Shell parser
* [`SH.Rm`](sh-rm.ads): Delete file function (should be replaced by OS.FS)
* [`SH.States`](sh-states.ads): Shell state
* [`SH.States.IO`](sh-states-io.ads): Shell I/O
* [`SH.String_Utils`](sh-string_utils.ads): Some string manipulation functions
* [`SH.Subst`](sh-subst.ads): Shell command expansion, parameter expansion, file expansion, field splitting
* [`SH.Subst.Arith`](sh-subst-arith.ads): Arithmetic expansion
* [`SH.Traces`](sh-traces.ads): GSH Traces mechanism
* [`SH.Tree`](sh-tree.ads): AST of shell scripts
* [`SH.Tree.Dumps`](sh-tree-dumps.ads): Dump script AST as yaml
* [`SH.Tree.Evals`](sh-tree-evals.ads): Script evaluation
* [`SH.Utils`](sh-utils.ads): Wrapper to Readline and conversion functions
