#lang rhombus
// Tests passes if all results are #true

import:
  "string.rkt"  : no_prefix  // for static  s[range]
  "range.rkt"   : no_prefix  
  "map-ref.rkt" : no_prefix  // for dynamic s[range]
  rhombus/macro : no_prefix
  
// Used as annotation
def s : "foo" :: String

// Used as predicate
("foo" is_a String)===#true
(42    is_a String)===#false

// Reference
s[0]===#{#\f}
s[1--end]==="oo"
s[--2]==="fo"

// Dynamic reference
def t : "bar"
t[0]===#{#\b}

// Binding syntax
def middle(String(a,_,b)): [a,b]
middle("xyz")===[#{#\x},#{#\z}]

// Mutation
// s[1]=#{#\a}
// s==="fao"

// Dot
// s.length