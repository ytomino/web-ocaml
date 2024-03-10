very very simple CGI library for Objective-Caml
===============================================

What's this?
------------

This library is a port of https://github.com/ytomino/web-ada/ to OCaml, and
contains the simple functions for CGI.

Prerequisites
-------------

OCaml >= 4.11
 https://ocaml.org/

How to make
-----------

Install
+++++++

::

 make install PREFIX=/usr/local

Specify your preferred directory to ``PREFIX``.
The libraries would be installed into ``$PREFIX/lib/ocaml`` (default is
``ocamlc -where``).

Uninstall
+++++++++

::

 make uninstall PREFIX=/usr/local

Build examples
++++++++++++++

::

 make -C examples
