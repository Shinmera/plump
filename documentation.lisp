#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.dom)

;; dom.lisp
(docs:define-docs
  (type node
    "Base DOM node class.")
  
  (type nesting-node
    "Node class that can contain child nodes.")

  (function children
    "Returns a vector of child-nodes that are contained within the node.")

  (type child-node
    "Node class that is a child and thus has a parent.")

  (function parent
    "Returns the node's parent that should contain this element as a child.")

  (type textual-node
    "Node class that represents a textual node and thus contains a TEXT field.")

  (function text
    "Returns the node's textual content.")

  (type root
    "Root DOM node, practically equivalent to a \"document\".")

  (type text-node
    "Text node that can only contain a single text string.")

  (type comment
    "Comment node that can only contain a single comment string.")

  (type element
    "Standard DOM element/block including attributes, tag-name, parent and children.")

  (function tag-name
    "Returns the element's tag name.")

  (function attributes
    "Returns an EQUALP hash-table of the element's attributes.")

  (type doctype
    "Special DOM node for the doctype declaration.")

  (function doctype
    "Returns the doctype node's actual doctype string.")

  (type fulltext-element
    "Special DOM element that contains full, un-entitied text like SCRIPT and STYLE.")

  (type xml-header
    "XML header element")

  (type cdata
    "XML CDATA section node.")

  (type processing-instruction
    "XML processing instruction node.")

  (function make-child-array
    "Creates an array to contain child elements")
  
  (function ensure-child-array
    "If the ARRAY is suitable as a child-array, it is returned.
Otherwise the array's elements are copied over into a proper
child-array.")

  (function make-attribute-map
    "Creates a map to contain attributes.")

  (function ensure-attribute-map
    "Ensures that the TABLE is suitable as an attribute-map.
If it is not, the table is copied into a proper attribute-map.")

  (function make-root
    "Creates a root node with the given children.
Children should be a vector with fill-pointer.")

  (function make-element
    "Creates a standard DOM element with the given tag name and parent.
Optionally a vector (with fill-pointer) containing children and an
attribute-map (a hash-table with equalp test) can be supplied.

Note that the element is automatically appended to the parent's child list.")

  (function make-text-node
    "Creates a new text node under the parent.

Note that the node is automatically appended to the parent's child list.")

  (function make-comment
    "Creates a new comment node under the parent.

Note that the node is automatically appended to the parent's child list.")

  (function make-doctype
    "Creates a new doctype node under the parent.

Note that the node is automatically appended to the parent's child list.")

  (function make-fulltext-element
    "Creates a fulltext element under the parent.
Optionally a text and an attribute-map (a hash-table with equalp test) 
can be supplied.

Note that the element is automatically appended to the parent's child list.")

  (function make-xml-header
    "Creates an XML header object under the parent.

Note that the element is automatically appended to the parent's child list.")

  (function make-cdata
    "Creates an XML CDATA section under the parent.

Note that the element is automatically appended to the parent's child list.")

  (function make-processing-instruction
    "Creates an XML processing instruction under the parent.

Note that the element is automatically appended to the parent's child list.")

  (function clear
    "Clears all children from the node.

Noe that the PARENT of all child elements is set to NIL.")

  (function siblings
    "Returns the array of siblings of the given child.
Note that this is a copy of the array, modifying it is safe.")

  (function family
    "Returns the direct array of children of the parent of the given child.
Note that modifying this array directly modifies that of the parent.")

  (function child-position
    "Returns the index of the child within its parent.")

  (function append-child
    "Appends the given child onto the parent's child list.
Returns the child.")

  (function prepend-child
    "Prepends the given child onto the parent's child list.
Returns the child.

Note that this operation is costly, see VECTOR-PUSH-EXTEND-FRONT")

  (function remove-child
    "Removes the child from its parent.
Returns the child.

Note that this operation is potentially very costly.
See VECTOR-POP-POSITION")

  (function replace-child
    "Replace the old child with a new one.
Returns the old child.")

  (function insert-before
    "Inserts the new-child before the given element in the parent's list.
Returns the new child.

Note that this operation is potentially very costly.
See VECTOR-PUSH-EXTEND-POSITION")

  (function insert-after
    "Inserts the new-child after the given element in the parent's list.
Returns the new child.

Note that this operation is potentially very costly.
See VECTOR-PUSH-EXTEND-POSITION")

  (function splice
    "Splices the contents of element into the position of the element in its parent.
Returns the parent.

Note that this operation is potentially very costly.
See ARRAY-SHIFT")

  (function clone-children
    "Clone the array of children.
If DEEP is non-NIL, each child is cloned as per (CLONE-NODE NODE T).
When copying deeply, you can also pass a NEW-PARENT to set on each child.")

  (function clone-attributes
    "Clone the attribute map.
Note that keys and values are NOT copied/cloned.")

  (function clone-node
    "Clone the given node, creating a new instance with the same contents.
If DEEP is non-NIL, the following applies:
The text of COMMENT and TEXT-NODEs is copied as per COPY-SEQ.
The children of NESTING-NODEs are copied as per (CLONE-CHILDREN CHILD T)")

  (function first-child
    "Returns the first child within the parent or NIL
if the parent is empty.")

  (function last-child
    "Returns the last child within the parent or NIL
if the parent is empty.")

  (function previous-sibling
    "Returns the sibling before this one or NIL if 
it is already the first.")

  (function next-sibling
    "Returns the sibling next to this one or NIL if
it is already the last.")

  (function child-elements
    "Returns a new vector of children of the given node, filtered to elements.")

  (function element-position
    "Returns the index of the child within its parent, counting only elements.
This excludes comments, text-nodes and the like.")

  (function sibling-elements
    "Returns the array of sibling elements of the given child.
Note that this is a copy of the array, modifying it is safe.
This excludes comments, text-nodes and the like.")

  (function family-elements
    "Returns the direct array of children elements of the parent of the given child.
Note that this is a copy of the array, modifying it is safe.
This excludes comments, text-nodes and the like.")

  (function first-element
    "Returns the first child element within the parent or NIL
if the parent is empty. This excludes comments, text-nodes and the like.")

  (function last-element
    "Returns the last child element within the parent or NIL
if the parent is empty. This excludes comments, text-nodes and the like.")

  (function previous-element
    "Returns the sibling element before this one or NIL if
it is already the first. This excludes comments, text-nodes and the like.")

  (function next-element
    "Returns the sibling element next to this one or NIL if
it is already last. This excludes comments, text-nodes and the like.")

  (function has-child-nodes
    "Returns T if the node can contain children and
the child array is not empty.")

  (function attribute
    "Returns the asked attribute from the element
or NIL. If the attribute could not be found, the
second return value is set to NIL.")

  (function (setf attribute)
    "Set an attribute on an element to the given value.")

  (function get-attribute
    "Synonymous to ATTRIBUTE.")

  (function set-attribute
    "Synonymous to (SETF (ATTIBUTE ..) ..)")

  (function remove-attribute
    "Remove the specified attribute if it exists.
Returns NIL.")

  (function has-attribute
    "Returns T if the provided attribute exists.")

  (function render-text
    "\"Renders\" the text of this element and its children.

In effect the text is gathered from the component and all of
its children, but transforming the text in such a way that:
- All ASCII white space (Space, Tab, CR, LF) is converted into spaces.
- There are no consecutive spaces.
- There are no spaces at the beginning or end.

This is somewhat analogous to how the text will be shown to
the user when rendered by a browser. Hence, render-text.")

  (function get-elements-by-tag-name
    "Searches the given node and returns an unordered
list of child nodes at arbitrary depth that match
the given tag.")

  (function get-element-by-id
    "Searches the given node and returns the first
node at arbitrary depth that matches the given ID
attribute.")

  (variable *stream*
    "The stream to serialize to during SERIALIZE-OBJECT.")

  (function serialize
    "Serializes NODE to STREAM.
STREAM can be a stream, T for *standard-output* or NIL to serialize to string.")

  (function serialize-object
    "Serialize the given node and print it to *stream*.")

  (function traverse
    "Traverse the NODE and all its children recursively,
calling FUNCTION on the current node if calling TEST on the current node
returns a non-NIL value. It is safe to modify the child array of the
parent of each node passed to FUNCTION.

NODE is returned.")

  (function trim
    "Trim all text-nodes within NODE (at any depth) of leading and trailing whitespace.")

  (function strip
    "Trim all text-nodes within NODE (at any depth) of leading and trailing whitespace. 
If their TEXT should be an empty string after trimming, remove them."))

;; entities.lisp
(docs:define-docs
  (variable *entity-map*
    "String hash-table containing the entity names and mapping them to their respective characters.")

  (function translate-entity
    "Translates the given entity identifier (a name, #Dec or #xHex) into their respective strings if possible.
Otherwise returns NIL.")

  (variable *alpha-chars*
    "All alphabetic characters and #")

  (function decode-entities
    "Translates all entities in the text into their character counterparts if possible.
If an entity does not match, it is left in place unless REMOVE-INVALID is non-NIL.")

  (function allowed-char-p
    "Returns T if the character is a permitted XML character.")

  (function discouraged-char-p
    "Returns T if the character is a discouraged XML character.")

  (type invalid-xml-character
    "Error signalled when an invalid XML character is encountered during WRITE-ENCODE-CHAR.")

  (type discouraged-xml-character
    "Warning signalled when a discouraged XML character is encountered during WRITE-ENCODE-CHAR.")

  (function write-encode-char
    "Write and possibly encode the CHAR to STREAM.
This also properly handles detection of invalid or discouraged XML characters.

The following restarts are available in the case of a faulty character:
  ABORT              Do not output the faulty character at all.
  USE-NEW-CHARACTER  Output a replacement character instead.
  CONTINUE           Continue and output the faulty character anyway.

See INVALID-XML-CHARACTER
See DISCOURAGED-XML-CHARACTER")

  (function encode-entities
    "Encodes the characters < > & \" with their XML entity equivalents.

If no STREAM is given, it encodes to a new string."))

(in-package #:org.shirakumo.plump.lexer)

;; lexer.lisp
(docs:define-docs
  (variable *string*
    "Contains the current string to lex.")

  (variable *length*
    "Set to the length of the string for bounds checking.")

  (variable *index*
    "Set to the current reading index.")

  (variable *matchers*
    "Hash table containing matching rules.")

  (function with-lexer-environment
    "Sets up the required lexing environment for the given string.")

  (function consume
    "Consumes a single character if possible and returns it.
Otherwise returns NIL.")
  
  (function advance
    "Skips a chracter if possible.
Returns the new index or NIL.")
  
  (function unread
    "Steps back a single character if possible.
Returns the new *INDEX*.")
  
  (function peek
    "Returns the next character, if any.")
  
  (function advance-n
    "Advances by N characters if possible.
Returns the new *INDEX*.")
  
  (function unread-n
    "Steps back by N characters if possible.
Returns the new *INDEX*.")
  
  (function consume-until
    "Consumes until the provided matcher function returns positively.
Returns the substring that was consumed.")
  
  (function matcher-character
    "Creates a matcher function that attempts to match the given character.")
  
  (function matcher-string
    "Creates a matcher function that attempts to match the given string.")
  
  (function matcher-range
    "Creates a matcher that checks a range according to the next character's CHAR-CODE.")
  
  (function matcher-find
    "Creates a matcher function that returns T if the character is found in the given list.")
  
  (function matcher-or
    "Creates a matcher function that returns successfully if any of the
sub-expressions return successfully. The first match is returned, if any.")
  
  (function matcher-and
    "Creates a matcher function that returns if all of the sub-expressions
return successfully. The last match is returned, if all.")
  
  (function matcher-not
    "Creates a matcher function that inverts the result of the sub-expression.")
  
  (function matcher-next
    "Creates a matcher environment that peeks ahead one farther.")
  
  (function matcher-prev
    "Creates a matcher environment that peeks behind.")

  (function matcher-any
    "Shorthand for (or (is a) (is b)..)")

  (function make-matcher
    "Macro to create a matcher chain.")

  (function define-matcher
    "Associates NAME as a keyword to the matcher form. You can then use the keyword in further matcher rules."))

(in-package #:org.shirakumo.plump.parser)

;; parser.lisp
(docs:define-docs
  (variable *whitespace*
    "List containing all whitespace characters.")

  (variable *root*
    "Object containing the current node to set as parent.")

  (variable *tagstack*
    "")

  (function skip-whitespace
    "")

  (function read-name
    "Reads and returns a tag name.")

  (function read-text
    "Reads and returns a text-node.")

  (function read-tag-contents
    "Reads and returns all tag contents. 
E.g. <foo bar baz> => bar baz")

  (function read-children
    "Read all children of the current *root* until the closing tag for *root* is encountered.")

  (function read-attribute-value
    "Reads an attribute value, either enclosed in quotation marks or until a space or tag end.")

  (function read-attribute-name
    "Reads an attribute name.")

  (function read-attribute
    "Reads an attribute and returns it as a key value cons.")

  (function read-attributes
    "Reads as many attributes as possible from a tag and returns them as an attribute map.")

  (function read-standard-tag
    "Reads an arbitrary tag and returns it.
This recurses with READ-CHILDREN.")

  (function read-tag
    "Attempts to read a tag and dispatches or defaults to READ-STANDARD-TAG.
Returns the completed node if one can be read.")

  (function read-root
    "Creates a root element and reads nodes into it.
Optionally uses the specified root to append child nodes to.
Returns the root.")

  (function slurp-stream
    "Quickly slurps the stream's contents into an array with fill pointer.")

  (function parse
    "Parses the given input into a DOM representation.
By default, methods for STRING, PATHNAME and STREAM are defined.
If supplied, the given root is used to append children to as per READ-ROOT.
Returns the root."))

;; processing.lisp
(docs:define-docs
  (function processing-parser
    "Return the processing-parser function for PROCESS-NAME. SETF-able.")

  (function (setf processing-parser)
    "Set the processing-parser function for PROCESS-NAME.")

  (function remove-processing-parser
    "Remove the processing-parser for PROCESS-NAME.")

  (function define-processing-parser
    "Defines a new processing-instruction parser. 
It is invoked if a processing-instruction (<?) with PROCESS-NAME is encountered.
The lexer will be at the point straight after reading in the PROCESS-NAME.
Expected return value is a string to use as the processing-instructions' TEXT.
The closing tag (?>) should NOT be consumed by a processing-parser."))

;; special-tags.lisp
(docs:define-docs
  (function starts-with
    "")

  (function ends-with
    "")

  (function define-self-closing-element
    "Defines an element that does not need to be closed with /> and cannot contain child nodes.")

  (function read-fulltext-element-content
    "Reads the contents of a fulltext element. This slurps everything until the matching closing tag is encountered.")

  (function define-fulltext-element
    "Defines an element to be read as a full-text element.
This means that it cannot contain any child-nodes and everything up until its closing
tag is used as its text."))

;; tag-dispatcher.lisp
(docs:define-docs
  (variable *all-tag-dispatchers*
    "All defined tag dispatchers")
  
  (variable *tag-dispatchers*
    "Active tag dispatcher functions")
  
  (variable *xml-tags*
    "List of XML tag dispatchers")
  
  (variable *html-tags*
    "List of HTML tag dispatchers")

  (type tag-dispatcher
    "Encapsulates the processing for a given tag.

See TAG-DISPATCHER-NAME
See TAG-DISPATCHER-TEST
See TAG-DISPATCHER-PARSER
See TAG-DISPATCHER-PRINTER")

  (function tag-dispatcher-name
    "Returns the name of the tag dispatcher.")

  (function tag-dispatcher-test
    "Returns the test function of the tag dispatcher.

The function takes one argument, a string, denoting the tag name.
It returns a boolean as to whether the tag name matches.")

  (function tag-dispatcher-parser
    "Parses the node for the given tag.

The function takes one argument, a string, denoting the tag name.
It returns a node, or NIL, if the matching was refused for some reason.")

  (function tag-dispatcher-printer
    "Prints  the node for the given tag.

The function takes one argument, a node, the element to print.
It returns the same node, or NIL, if the printing was refused for some reason.")
  
  (function tag-dispatcher
    "Accessor to the tag-dispatcher of the given name.")

  (function remove-tag-dispatcher
    "Removes the tag-dispatcher of the given name from the list.")

  (function define-tag-dispatcher
    "Defines a new tag dispatcher. It is invoked if TEST-FORM passes.

NAME      --- Name to discern the dispatcher with.
LISTS     --- Symbols of lists to which the dispatcher should be added.
TAGVAR    --- Symbol bound to the tag name.
BODY      --- Body forms describing the test to match the tag.")

  (function define-tag-parser
    "Defines the parser function for a tag dispatcher.

NAME    --- The name of the tag dispatcher. If one with this name 
            cannot be found, an error is signalled.
TAGVAR  --- Symbol bound to the tag name.
BODY    --- Body forms describing the tag parsing behaviour.")

  (function define-tag-printer
    "Defines the printer function for a tag dispatcher.

NAME    --- The name of the tag dispatcher. If one with this name 
            cannot be found, an error is signalled.
TAGVAR  --- Symbol bound to the tag name.
BODY    --- Body forms describing the printing behaviour. Write to
            the stream bound to PLUMP-DOM:*STREAM*.")

  (function do-tag-parsers
    "Iterates over the current *TAG-DISPATCHERS*, binding the TEST and PARSER functions.
Returns RESULT-FORM's evaluated value.")

  (function do-tag-printers
    "Iterates over the current *TAG-DISPATCHERS*, binding the TEST and PRINTER functions.
Returns RESULT-FORM's evaluated value."))

