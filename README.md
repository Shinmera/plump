## What is Plump?
Plump is a parser for HTML/XML like documents, focusing on being lenient towards invalid markup. It can handle things like invalid attributes, bad closing tag order, unencoded entities, inexistent tag types, self-closing tags and so on. It parses documents to a class representation and offers a small set of DOM functions to manipulate it. You are free to change it to parse to your own classes though.

## How To
Load Plump through Quicklisp or ASDF:

    (ql:quickload :plump)

Using the `PARSE` function, plump will transform a string, pathname or stream into a document:

    (plump:parse "<foo><bar this is=\"a thing\">baz</bar><span id=\"test\">oh my")

This returns a root node. If you want to append a document to a root node (or any other node that accepts children) that you've made, you can pass it into the parse function. To return the document into a readable form, you can call `SERIALIZE`:

    (plump:serialize *)
        
Using the DOM you can easily traverse the document and change it:

    (plump:remove-child (plump:get-element-by-id ** "test"))
    (plump:serialize ***)

By default plump includes a few special tag dispatchers to catch HTML oddities like self-closing tags and fulltext-nodes. Especially the self-closing tags can lead to problems in XML documents. In order to parse without any HTML "tricks", you can simply do:

    (let ((plump:*tag-dispatchers* plump:*xml-tags*)) (plump:parse "<link>foo</link>"))

This will also influence the serialization. By default self-closing tags will be printed in "HTML-fashion," but if you require full XML support, the above should be the way to go. This behaviour is new in Plump2, as previously everything was always serialized in XML mode.

## Extending Plump
If you want to handle a certain tag in a special way, you can write your own tag-dispatcher. For example comments, the doctype and self-closing tags are handled in this fashion. In order to properly hook in, you will have to learn to use Plump's lexer (see next section).

    (plump:define-tag-dispatcher (my-dispatcher *tag-dispatchers*) (name)
      (string-equal name "my-tag"))
    
    (plump:define-tag-parser my-dispatcher (name)
      (let ((attrs (plump:read-attributes)))
        (when (char= (plump:consume) #\/)
          (plump:consume)) ;; Consume closing
        (make-instance 'my-tag :parent plump:*root* :attributes attrs)))

If you don't want to disturb the standard Plump tag dispatchers list, you can define your own special variable to contain the dispatchers and bind `*tag-dispatchers*` to that during parsing, as shown for the XML example above. Shorthand macros exist to define self-closing or full-text tags:

    (plump:define-self-closing-element img *tag-dispatchers* *html-tags*)
    (plump:define-fulltext-element style *tag-dispatchers* *html-tags*)

XML allows for script tags (like `<?php ?>`). By default Plump does not specify any special reading for any script tag. If an unhandled script tag is encountered, a warning is emitted and Plump will try to just read anything until `?>` is encountered. For most script tags this probably will not suffice, as they might contain some form of escaped `?>`. If you do want to use Plump to process script tags properly as well, you will have to define your own reader with `define-processing-parser`. You can also use that macro to define a reader that outputs a more suitable format than a text tag.

During parsing, all elements are created through `MAKE-*` functions like `MAKE-ROOT`, `MAKE-ELEMENT`, `MAKE-TEXT-NODE`, and so on. By overriding these functions you can instead delegate the parsing to your own DOM.

If you subclass the DOM classes, you might want to define a method on `SERIALIZE-OBJECT` to produce the right output.

## Plump's Lexer
Since parser generators are good for strict grammars and Plump needed to be fast and lenient, it comes with its own primitive reading/lexing mechanisms. All the lexer primitives are defined in `lexer.lisp` and you can leverage them for your own projects as well, if you so desire.

In order to allow the lexing to work, you'll have to wrap your processing code in `with-lexer-environment`. You can then use functions like `consume`, `advance`, `unread`, `peek` and `consume-until` to process the input.

`make-matcher` allows you to use a very simple language to define matching operations. This will evaluate to a function with no arguments that should return `T` if it matches and `NIL` otherwise. Combining matchers with `consume-until` allows you to easily make sequence readers:

    (plump:with-lexer-environment ("<foo>")
      (when (char= #\< (plump:consume))
        (plump:consume-until (plump:make-matcher (is #\>)))))

Available matcher constructs are `not`, `and`, `or`, `is`, `in`, `next`, `prev`, `any`, and `find`. `define-matcher` allows you to associate keywords to matchers, which you can then use as a matcher rule in `make-matcher`. Regular symbols act as variables:

    (let ((find "baz"))
      (plump:with-lexer-environment ("foo bar baz")
         (plump:consume-until (plump:make-matcher (is find)))))

## Speed
![benchmark](http://shinmera.tymoon.eu/public/plump-benchmark.png)

If you know of other native-lisp libraries that beat Plump, please do let me know, I would be very interested!

## See Also
* [lQuery](https://shinmera.github.io/lquery/) Dissect and manipulate the DOM with jQuery-like commands.
* [CLSS](https://shinmera.github.io/CLSS/) Traverse the DOM by CSS selectors.
* [plump-tex](https://github.com/Shinmera/plump-tex) Serialize between TeX and the Plump DOM.
* [plump-sexp](https://github.com/Shinmera/plump-sexp) Serialize between SEXPrs and the Plump DOM.
