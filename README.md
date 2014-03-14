plump
=====

Practically Lenient and Unimpressive Markup Parser for Common Lisp

This is an XML/HTML parser aiming to be simple and lenient. It should be able to eat up almost anything you throw at it. Plump also comes with a small DOM, though this can easily be delegated to another implementation by overwriting the specific ```MAKE-``` functions.

To parse a file, stream or string, simply invoke: ```(plump:parse stream-string-pathname)```.
To see the results, you can invoke ```plump:serialize``` on any node (as well as the ```root``` returned by ```parse```).
