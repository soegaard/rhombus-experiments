#lang rhombus
///
/// Spelling Corrector
///

// This is a Rhombus implementation of Norvig's spelling corrector.
//     https://norvig.com/spell-correct.html
// Norvig's spelling corrector is written in Python.

// Imports

import:
  "map-ref.rkt":       no_prefix  // todo: make "foo"[0] work
  "racket-for.rkt":    no_prefix
  rhombus/macro: no_prefix
               
  racket/base:         prefix rkt
  "racket-regexp.rkt": prefix re
  "racket-string.rkt": prefix string
  "racket-file.rkt":   prefix file
  "racket-port.rkt":   prefix port
  "racket-hash.rkt":   prefix hash
  "racket-list.rkt":   prefix list
  "racket-set.rkt":    prefix set

// Extra Forms

// Multiple definitions
defn.macro '(defs:
               $x: $y ...
               ...):
  '(:
      def $x: $y ...
      ...
  )


// Helpers

val apply:   rkt.apply
val map:     list.map
val foreach: list.for_each
fun add1(x): x+1           
fun sum(xs): rkt.foldl((fun(a,b):a+b),0,xs)
fun max(xs, ~key: f): // assumes that xs is non-empty
  def mutable x0: xs[0]
  def mutable k0: 0
  list.for_each((fun(x):
                   val fx: f(x)
                   if fx>k0        // Is there a `when`? Not yet.
                   | x0 = x        
                     k0 = fx
                   | #false),
                xs)
  x0

fun findall(pattern, string): re.match_all(pattern, string)                              
fun regexp(ss):   apply(re.pregexp, ss)
fun open(path):   file.open(path)        // Note: path can't be renamed to file here
fun read(a_port): port.to_string(a_port)
fun stringify(x): if string.is_string(x) | x | string.from_char(x)                          

// Problem: how to define the nary case?
operator (a ++ b):
  ~associativity: ~right  
  ~stronger_than: && ||                  
  string.append(stringify(a),stringify(b))


// Counters
                  
class Counter(keys, hash)

fun values(counter):          hash.values(counter.hash)
fun add_key_to_hash(key, ht): hash.update(ht, key, add1, 1)

fun add_keys_to_counter(keys, counter):
  def hash: counter.hash
  list.for_each((fun(key): add_key_to_hash(key,hash)),keys)

fun new_counter(keys) :
  def counter: Counter(keys,hash.make())
  add_keys_to_counter(keys,counter)
  counter

///
/// The Spelling Corrector
///
 
// def words(text): return re.findall(r'\w+', text.lower())
//   > words("foo bar baz")
//   ["foo", "bar", "baz"] 
def words(text): findall(@regexp{\w+}, string.downcase(text))

// def WORDS: words(read(open("big.txt")))
// def WORDS: new_counter(words(read(open("small.txt"))))
def WORDS: new_counter(words(read(open("big.txt"))))

// def P(word, N=sum(WORDS.values())): 
//    "Probability of `word`."
//    return WORDS[word] / N
def P(word, N=sum(values(WORDS))) :
  1.0*WORDS.hash[word]/N

// def correction(word): 
//    "Most probable spelling correction for word."
//    return max(candidates(word), key=P)
def correction(word):
  max(set.to_list(candidates(word)), ~key:P)

// def candidates(word): 
//    "Generate possible spelling corrections for word."
//    return (known([word]) or known(edits1(word)) or known(edits2(word)) or [word])
// Note: In Python an empty set is interpreted as `false` by `or`.
def candidates(word):
  known([word]) || known(edits1(word)) || known(edits2(word)) || [word]

// def known(words): 
//    "The subset of `words` that appear in the dictionary of WORDS."
//    return set(w for w in words if w in WORDS)
def known(words) :
  fun pred(word) : hash.ref(WORDS.hash,word,#false)
  def ks: for_list_when(word,words, pred(word), word)
  list.is_non_empty(ks) && set.to_list(ks)

// def edits1(word):
//    "All edits that are one edit away from `word`."
//    letters    = 'abcdefghijklmnopqrstuvwxyz'
//    splits     = [(word[:i], word[i:])    for i in range(len(word) + 1)]
//    deletes    = [L + R[1:]               for L, R in splits if R]
//    transposes = [L + R[1] + R[0] + R[2:] for L, R in splits if len(R)>1]
//    replaces   = [L + c + R[1:]           for L, R in splits if R for c in letters]
//    inserts    = [L + c + R               for L, R in splits for c in letters]
//    return set(deletes + transposes + replaces + inserts)

def edits1(word):
  def ok(s)     : string.is_non_empty(s)
  def clean(xs) : rkt.filter((fun(x): x),xs)
  defs:
    letters:    "abcdefghijklmnopqrstuvwxyz"
    splits:     for_list(i, list.range(string.length(word)+1), [word[~to:i], word[~from:i]])
    deletes:    for_list(s, splits,(fun(List(L,R)):                ok(R) && L++R[~from:1])(s))
    transposes: for_list(s, splits,(fun(List(L,R)):  string.length(R)>1  && L++R[1]++R[0]++R[~from:2])(s))
    replaces:   for_list(s, splits,(fun(List(L,R)):                ok(R) && for_list(c,letters,L++c++R[~from:1]))(s))
    inserts:    for_list(s, splits,(fun(List(L,R)):                         for_list(c,letters,L++c++R))(s))
  set.from_list(clean(list.flatten([deletes, transposes, replaces, inserts])))

// def edits2(word): 
//    "All edits that are two edits away from `word`."
//    return (e2 for e1 in edits1(word) for e2 in edits1(e1))
def edits2(word):
  set.from_list(list.flatten(for_list(e1,edits1(word),for_list(e2,edits1(e1),e2))))

//// Examples

// > known(edits2("somthing"))
// ["seething", "something", "smoothing", "scathing", "loathing", "sorting", "nothing", "soothing"]

// > correction("sumthing")
// "something"
