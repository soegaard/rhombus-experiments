#lang rhombus
///
/// Spelling Corrector
///

// This is a Rhombus implementation of Norvig's spelling corrector.
//     https://norvig.com/spell-correct.html
// Norvig's spelling corrector is written in Python.

// Imports

import:
  "map-ref.rkt":       no_prefix // for dynamic s[i]  
  "string.rkt":        no_prefix // for String and s[i]
  "range.rkt":         no_prefix // for -- and end
  "racket-for.rkt":    no_prefix
  "macro.rkt":         no_prefix // displayln on compile time
  rhombus/macro:       no_prefix // for defining dot provider
               
  racket/base:         prefix base
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

val apply:   base.apply
val map:     list.map
def length:  base.length
def append:  base.append
val foreach: list.for_each
fun sum(xs): base.foldl((fun(a,b):a+b),0,xs)

def is_string: string.is_string
fun findall(pattern, string): re.match_all(pattern, string)                              
fun regexp(ss):   apply(re.pregexp, ss)
fun stringify(x): if is_string(x) | x | string.from_char(x)

fun add1(x): x+1           

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

fun open(path):   file.open(path)        // Note: path can't be renamed to file here
fun read(a_port): port.to_string(a_port)

// Problem: how to define the nary case?
operator (a ++ b):
  ~associativity: ~right  
  ~stronger_than: && ||                  
  string.append(stringify(a),stringify(b))


// String Dot Provider
// If s is annotated with String, then we can use s.length etc
// The Rhombus standard library will at some point have
// a String annotation. This is temporary.

// Note: Don't use `x is_a String` in the predicate. That's an infinite loop.
annotation.macro 'String:
  annotation_ct.pack_predicate('is_string,
                               '(($(dot_ct.provider_key), string_dot_provider)))
dot.macro '(string_dot_provider $left $dot $right):
  match right
  // One argument functions
  | 'length:       '(string.length($left))
  | 'is_non_empty: '(string.is_non_empty($left))
  | 'is_empty:     '(string.is_empty($left))
  | 'downcase:     '(string.downcase($left))
  // More arguments
  | 'append:       '(fun (more): string.appends(cons($left,[more]))) // variadic? how?

// def s: "foo" :: String
// s.length
// s.append("bar")


/// Counters

// TODO: Use maps here.

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

// The original spelling corrector is in Python.
// I have kept the original in order to compare.

// def words(text): return re.findall(r'\w+', text.lower())
//   > words("foo bar baz")
//   ["foo", "bar", "baz"] 
def words(text::String): findall(@regexp{\w+}, text.downcase)

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
  def ks: for_list_when(word, words, pred(word), word)
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

def edits1(word :: String):
  def ok(s::String) : s.is_non_empty
  def clean(xs)     : base.filter((fun(x): x),xs)
  defs:
    letters:    "abcdefghijklmnopqrstuvwxyz"
    splits:     for_list(i, list.range(word.length+1), [word[--i], word[i--end]])
    deletes:    for_list(s, splits, (fun(List(L,R)):                ok(R) && L++R[1--end])(s))
    transposes: for_list(s, splits, (fun(List(L,R::String)):  R.length>1  && L++R[1]++R[0]++R[2--end])(s))
    replaces:   for_list(s, splits, (fun(List(L,R)):                ok(R) && for_list(c,letters, L++c++R[1--end]))(s))
    inserts:    for_list(s, splits, (fun(List(L,R)):                         for_list(c,letters, L++c++R))(s))
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
