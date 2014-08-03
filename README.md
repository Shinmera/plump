What is Plump?
--------------
Plump is a parser for HTML/XML like documents, focusing on being lenient towards invalid markup. It can handle things like invalid attributes, bad closing tag order, unencoded entities, inexistent tag types, self-closing tags and so on. It parses documents to a class representation and offers a small set of DOM functions to manipulate it. You are free to change it to parse to your own classes though.

How To
------
Load Plump through Quicklisp or ASDF:

```
(ql:quickload :plump)
```

Using the `PARSE` function, plump will transform a string, pathname or stream into a document:
         
```
(plump:parse "&lt;foo&gt;&lt;bar this is=\"a thing\"&gt;baz&lt;/bar&gt;&lt;span id=\"test\"&gt;oh my")
```

This returns a root node. If you want to append a document to a root node (or any other node that accepts children) that you've made, you can pass it into the parse function. To return the document into a readable form, you can call `SERIALIZE`:
         
```
(plump:serialize *)
```
        
Using the DOM you can easily traverse the document and change it:
         
```
(plump:remove-child (plump:get-element-by-id ** "test"))
(plump:serialize ***)
```

By default plump includes a few special tag dispatchers to catch HTML oddities like doctype, self-closing tags and comments. Especially the self-closing tags can lead to problems in XML documents. In order to parse without any HTML &quot;tricks&quot;, you can simply bind `*TAG-DISPATCHERS*` to NIL before parsing.
         
```
(let ((plump:*tag-dispatchers* ())) (plump:parse "&lt;link&gt;foo&lt;/link&gt;"))
```

Extending Plump
---------------
If you want to handle a certain tag in a special way, you can write your own tag-dispatcher. For example comments, the doctype and self-closing tags are handled in this fashion by default. 
         
```
(plump:define-tag-dispatcher my-dispatcher (name)
    (string-equal name "my-tag")
  (let ((attrs (plump:read-attributes)))
    (when (char= (plump:consume) #\/)
      (plump:consume)) ;; Consume closing
    (make-instance 'my-tag :parent plump:*root* :attributes attrs)))
```
      
During parsing, all elements are created through the functions `MAKE-ROOT`, `MAKE-ELEMENT`, `MAKE-TEXT-NODE`, and `MAKE-COMMENT`. By overriding these functions you can instead delegate the parsing to your own DOM.
         
Speed
-----
![benchmark](http://shinmera.tymoon.eu/public/plump-benchmark.png)

If you know of other native-lisp libraries that beat Plump, please do let me know, I would be very interested!
