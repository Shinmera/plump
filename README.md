plump
=====

Practically Lenient and Unimpressive Markup Parser for Common Lisp

This is an XML/HTML parser aiming to be simple and lenient. It should be able to eat up almost anything you throw at it. Plump also comes with a small DOM, though this can easily be delegated to another implementation by overwriting the specific ```MAKE-``` functions.

To parse a file, stream or string, simply invoke: ```PLUMP:PARSE```.
To see the results, you can invoke ```PLUMP:SERIALIZE``` on any node (as well as the ```ROOT``` returned by ```PARSE```).

See [about.html](http://shinmera.github.io/plump/) for more information and a complete symbol documentation index.
