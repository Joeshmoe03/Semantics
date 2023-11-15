(* CS0433 HW1 --------------------------------------------------------------- *)
(*
   hellocaml.ml

   This is a group project. You may work with another person in the class, but
   please submit individually, and put the name of who you worked with up here.
*)

(*
  The goal of this homework assignment it to give you some familiarity with
  writing OCaml code. There is much more to learning OCaml than what this
  homework covers, but this will give you a strong foundation for the future.

  This tutorial-style project refers to parts of the online book "OCaml
  Programming: Correct + Efficient + Beautiful" linked to from the main Canvas
  page. It provides a very good reference for learning OCaml. In the problems
  below when you see a note like "See Book 5" please refer to that section of
  the book.

  This project covers most of the content of the chapters 2-4 of the book, and
  has been covered in class. You might want to keep the book open while you do
  the homework; parts of the project below will point you to specific sections
  of the book.

  Please feel ask the instructor or drop by office hours.
*)


(*
   Files in this project:

   hellocaml.ml
     -- (this file) the homework file you will need to modify

   gradedtests.ml
     -- some tests that we will run to assess the correctness of your homework
     -- our test suite includes more tests that those given here

   studenttests.ml
     -- tests that you write to help with debugging and for a grade
        (as requested below)

*)

(******************************************************************************)
(*                                                                            *)
(* PART 1: OCaml Basics                                                       *)
(*                                                                            *)
(******************************************************************************)

(* OCaml comments are written using '(*' and '*)' delimiters.  They nest.     *)

(*
  OCaml is strongly typed and provides a standard suite of base types:
    int  -  31 bit integers 0, 1, (-1), etc.
    int64 - 64 bit integers 0l, 1l, (-1l), etc.
    bool -  'true' and 'false'
    unit -  ()   a 'trivial' type that has only one member, namely ()
    string - "a string" or "another"
    float, char, etc. will also crop up occasionally
  For more details, see Book Chapter 2, pay particular attention to 2.3.7 about
  typechecking.
*)

(*
   OCaml is an expression-oriented, functional language.  This mean you typically
   don't do imperative updates to variables as you do in languages like C, Java, etc.

   Instead, you 'name' computations using the keyword 'let':
*)

let an_int : int = 3
let another_int : int = 3 * 14

(*
   The  ': int' part of the 'let' bindings above is an type ascription -- these
   are optional in OCaml programs (type inference will figure them out), but it
   is good style to use type ascriptions on the top-level definitions you make
   because it will improve the error messages that the OCaml typechecker
   generates when you make a mistake.

   For example, it is an error to add a float and an int:
*)
(* Uncomment to get a type error:
   let an_error : int = 3 + 1.0
*)

(*
  'let' expressions can be nested.  The scope of a let-bound variable is
  delimited by the 'in' keyword.
  See also Book Chapter 2.3:
*)

(* bind z to the value 39 *)
let z : int =
  let x = 3 in
  (* x is in scope here *)
  let y = x + x in
  (* x and y are both in scope here *)
  (y * y) + x


(*
   Scoping is sometimes easier to see by writing (optional) 'begin'-'end'
   delimiters.  This is equivalent to the binding for z above.
*)
(* bind z to the value 39 *)
let z : int =
  let x = 3 in begin
    let y = x + x in begin
      (y * y) + x
    end
  end

(*
   Here and elsewhere 'begin'-'end' are treated exactly the same as parentheses:
   (but it is bad format to abuse that fact).
*)
(* bind z to the value 39 *)
let z : int =
  let x = 3 in
  let y = x + x in
  begin y * y end + x

(*
  Once bound by a 'let', binding between a variable (like 'z' above) and its
  value (like 39) never changes.  Variables bindings can be shadowed, though.
  each subsequent definition of 'z' above 'shadows' the previous one.
*)

(*
   The most important type of values in OCaml is the function type.
   Function types are written like 'int -> int', which is the type of a function
   that takes an int argument and produces an int result.
   See Book Chapter 2.4

   Functions values are introduced using the 'fun' keyword and the '->' sytnax:
    fun (x:int) -> x + x    (* a function that takes an int and doubles it *)

   Functions are first class -- they can passed around just like integers or
   other primitive data.
*)

(* bind the variable 'double' of type 'int -> int' to a function: *)
let double : int -> int = fun (x : int) -> x + x

(*
  Functions are called or 'applied' by juxtaposition -- the space ' '
  between a function name and its arguments is the function application site.
  Unlike Java or C, no parentheses are needed, exept for grouping and
  precedence:
*)
let doubled_z : int = double z (* call double on z  *)

let quadrupled_z : int = double (double z) (* parens needed for grouping *)

let sextupled_z : int = quadrupled_z + double z

(*
  Functions with more than one argument have types like:
  'int -> int -> int', which is the type of a function that takes an int
  and returns a function that itself takes an int and returns an int.
  i.e. 'int -> int -> int' is just 'int -> (int -> int)'
*)

let mult : int -> int -> int = fun (x : int) (y : int) -> x * y
let squared_z : int = mult z z (* multiply z times z *)

(*
  Because functions like 'mult' above return functions, they can be
  partially applied:
*)
let mult_by_3 : int -> int = mult 3 (* partially apply mult to 3 *)

let mult_by_4 : int -> int = mult 4 (* partially apply mult to 4 *)

let meaning_of_life : int = mult_by_3 14
(* call the partially applied function *)

let excellent_score : int = mult_by_4 25 (* compute 100 *)

(*
   The let-fun syntax above is a bit heavy, so OCaml provides syntactic sugar
   for abbreviating function definitions, avoiding the need for 'fun' and
   redundant-type annotations.

   For example, we can write double like this:
*)
let double (x : int) : int = x + x (* this definition shadows the earlier one *)

(* and mult like this: *)
let mult (x : int) (y : int) : int = x * y

(* We still call them in the same way as before: *)
let quadrupled_z : int = double (double z) (* parens needed for grouping *)

let mult_by_3 : int -> int = mult 3 (* partially apply mult to 3 *)

(*
   Note the use of type annotations
      let f (arg1:t1) (arg2:t2) ... (argN:tN) : retT = ...
   Defines f, a function of type t1 -> t2 -> ... -> tN -> retT
*)

(* Functions are first-class values, they can be passed to other functions: *)
let twice (f : int -> int) (x : int) : int =
  (* f is a function from ints to ints *)
  f (f x)

let quadrupled_z_again : int = twice double z (* pass double to twice *)

(* OCaml's interactive interpreter --------------------------------------------------- *)
(*

   OCaml comes with an interactive interpreter, also called a REPL (or
   Read-Eval-Print-Loop). This is an interactive OCaml session that accepts
   OCaml expressions on the command line, compiles them, runs the resulting
   bytecode, and then prints out the result. The REPL is hard to use
   once programs get big (and are distributed into many source files), but it is
   a great way to learn the OCaml basics, and for trying out one-file programs
   like 'hellocaml.ml'.

   If you are using VS Code and have installed the OCaml Platform extension,
   this REPL is available for you in "Terminal" section at the bottom of the
   screen. If you don't see it, you may need to open it from the View menu.

   This version of the OCaml REPL is called 'utop' -- a more featureful toplevel loop.

  In the terminal, you should see a welcome message and then a prompt:

  utop #

  You can type ocaml expressions at the prompt like this:

  # 3 + 4;;
  -: int = 7

  To quit, use the '#quit' directive:

  # #quit;;

  Note that, in the top-level-loop (unlike in a source file) top-level
  definitions and expressions to be evaluated must be terminated by ';;'. OCaml
  will compile and run the result (in this case computing the int 7).

  You can (re)load .ml files into the top-level loop via the '#use' directive:

  # #use "hellocaml.ml";;
  ...

  In this case, OCaml behaves as though you entered in all of the contents of
  the .ml file at the command prompt (except that ';;' is not needed).

  Try it now -- load this "hellocaml.ml" file into the OCaml top level.
  If you succeed, you should see a long list of definitions that this file
  makes.

  Note that utop, by default, will start in whatever directory you run it in; by
  default, that will probably be the root directory of your project. You can
  change utop's working directory using the #cd command.

  Once you '#use' a file, you can then interact with the functions and other
  values it defines at the top-level.  This can be very useful when playing
  around with your programs, debugging, or testing functions.

  Try it now: after using Hellocaml.ml, type  `twice ((+) 2) 4;;` a the
  prompt.  Utop should respond with:

  - : int = 8
*)

(*
Running the tests

As part of this homework assignment you have been given a set of automated tests
that will help tell you once you have a correct solution. In order to run them,
you will need to use the Terminal on your computer. You probably used the
Terminal to install OCaml on your machine. In the terminal, navigate to the
homework folder (on macOS and Linux, you will need to use the `cd` command).

Once in the homework folder, run utop. It will be launched with this hellocaml
  file, and related files already available. You can run the tests by typing

exec_tests ();;

Be sure to be exact: there is a space between exec_tests and the (). This should
run all the tests. In the beginning many of them will say "Failed". As you
fill in the problems in this file, more and more of them will pass.

*)


(* PART 1 Problems ---------------------------------------------------------- *)
(*
   Complete the following definitions as directed -- you can tell when you get
   them right because the corresponding tests will pass when you run them using
   the instructions above.

   Note that (fun _ -> failwith "unimplemented") is a function that takes any
   argument and raises a Failure exception -- you will have to replace these
   definitions with other function definitions to pass the tests.
*)

(* Problem 1-1 *)
(*
   The 'pieces' variable below is bound to the wrong value.  Bind it to one that
   makes the first case of part1_tests "Problem 1" succeed. See the
   gradedtests.ml file.
*)
let pieces : int = 8

(* Implement a function cube that takes an int value and produces its cube. *)
let cube : int -> int = fun (x : int) -> x * x * x

(* Problem 1-2 *)
(*
   Write a function "cents_of" that takes
    q - a number of quarters
    d - a number of dimes
    n - a number of nickels
    p - a number of pennies
   (all numbers non-negative)
   and computes the total value of the coins in cents:
*)
let cents_of : int -> int -> int -> int -> int = 
  fun (q: int)(d: int)(n: int)(p: int) -> 25 * q + 10 * d + 5 * n + p

(* Problem 1-3 *)
(*
   Open the file studenttests.ml. There are a number of tests using the function
  using assert_eqf, which asserts that its two arguments are equal. The first
  argument is a function that returns something, the second is a function call
  or a constant value.

  Edit the function argument of the "Student-Provided Problem 3" test so that
  "case1" passes, given the definition below. You will need to remove the
  function body starting with failwith and replace it with something else.
*)
let prob3_ans : int = 42

(*
  Similarly, edit the function argument of the "Student-Provided Problem 3" test
  in studenttests.ml so that "case2" passes, given the definition below:
*)
let prob3_case2 (x : int) : int = prob3_ans - x

(*
  Replace 0 with a literal integer argument in the "Student-Provided Problem 3"
  test in studenttest.ml so that "case3" passes, given the definition below:
*)
let prob3_case3 : int =
  let aux = prob3_case2 10 in
  double aux

(*
  In this and later projects, you can add your own test cases to the
  studenttests.ml file.  They can be run by calling exec_test ();; in utop.
*)

(******************************************************************************)
(*                                                                            *)
(* PART 2: Tuples, Generics, Pattern Matching                                 *)
(*                                                                            *)
(******************************************************************************)

(*
   NOTE: See Book Section 3.
   Tuples are a built-in aggregate datatype that generalizes
   pairs, triples, etc.   Tuples have types like:
       int * bool * string
   At the value level, tuples are written using ',':
*)

let triple : int * bool * string = (3, true, "some string")

(* NOTE: technically, the parentheses are _optional_, so we could have done: *)
let triple : int * bool * string = 3, true, "some string"

(* Tuples can nest *)
let pair_of_triples : (int * bool * string) * (int * bool * string) =
  (triple, triple)

(*
   IMPORTANT!!  Be sure to learn this!

   You can destruct tuples and most other kinds of data by "pattern-matching".

   Pattern matching is a fundamental concept in OCaml: most
   non-trivial OCaml types are usually "destructed" or "inspected" by
   pattern matching using the 'match-with' notation.  See Book Chapter 3.1.

   A "pattern" looks like a value, except that it can have 'holes'
   marked by _ that indicate irrelevant parts of the pattern, and
   binders, indicated by variables.

   Consider:

   begin match exp with
     | pat1 -> case1
     | pat2 -> case2
     ...
   end

   This evaluates exp until it reaches a value.  Then, that value is
   'matched' against the patterns pat1, pat2, etc. until a match is
   found.  When the first match is found, the variables appearing in
   the pattern are bound to the corresponding parts of the value and
   the case associated with the pattern is executed.

   If no match is found, OCaml will raise a Match_failure exception.
   If your patterns are not exhaustive -- i.e. they do not cover
   all of the possible cases, the compiler will issue a (usually very
   helpful) warning.
*)

(*
  Tuples are "generic" or "polymorphic" -- you can create tuples of any
  datatypes.

  The generic parts of types in OCaml are written using tick ' notation:
  as shown in the examples below.  What you might write in Java as
   List<A>  you would write in OCaml as 'a list -- type parameters
  are written in prefix.
  Similarly, Map<A,B> would be written as ('a,'b) map -- multiple
  type parameters use a 'tuple' notation.
*)

(*
   NOTE:
   'a is pronounced "tick a" or "alpha".
   'b is pronounced "tick b" or "beta".
   'c is pronounced "tick c" or "gamma".

   Using Greek letters -- this an ML tradition that dates back to its use
   for developing formal logics and proof systems.
*)

(*
  TIP: In VSCode you can get the type of an expression by hovering the
  mouse cursor over the program text.  This only works if the program
  successfully compiled.

  If you're using Emacs or Vim with the merlin plugin, you can get the
  type of a an expression at the cursor too: ^C ^T on Emacs
*)

(* Example pattern matching against tuples: *)
let first_of_three (t : 'a * 'b * 'c) : 'a =
  (* t is a generic triple *)
  match t with x, _, _ -> x

let t1 : int = first_of_three triple (* binds t1 to 3 *)

let second_of_three (t : 'a * 'b * 'c) : 'b = match t with _, x, _ -> x
let t2 : bool = second_of_three triple (* binds t2 to true *)

(*
   This generic function takes an arbitrary input, x, and
   returns a pair both of whose components are the given input:
*)
let pair_up (x : 'a) : 'a * 'a = (x, x)

(* Part 2 Problems ---------------------------------------------------------- *)

(*
   Problem 2-1

   Complete the definition of third_of_three; be sure to give it
   the correct type signature (we will grade that part manually):
*)
let third_of_three (t : 'a * 'b * 'c) : 'c = match t with _, _, x -> x
let t3 : string = third_of_three triple

(*
  Problem 2-2

  Implement a function compose_pair of the given type that takes
  a pair of functions and composes them in sequence.  Note that
  you must return a function.  See the test cases in gradedtests.ml
  for examples of its use.
*)

let compose_pair (p : ('b -> 'c) * ('a -> 'b)) : 'a -> 'c =
  fun x -> match p with b_to_c, a_to_b -> b_to_c (a_to_b x)

(******************************************************************************)
(*                                                                            *)
(* PART 3: Lists and Recursion                                                *)
(*                                                                            *)
(******************************************************************************)

(*
  OCaml has a build-in datatype of generic lists: See Book Chapter 3.1

  [] is the nil list
  if
    h   is a head-value of type t
    tl  is a list of elements of type t
  then
   h::tl  is a list with h as the head and tl as the tail

  `::`  is pronounced "cons", as it constructs a list.
*)

let list1 : int list = [ 3; 2; 1 ]

(* Lists can also be written using the [v1;v2;v3] notation: *)

let list1' = [ 3; 2; 1 ] (* this is equivalent to list1 *)

(* Lists are homogeneous -- they hold values of only one type: *)
(* Uncomment to get a type error; recomment to compile:
   let bad_list = [1;"hello";true]
*)

(*
  As usual in OCaml, we use pattern matching to destruct lists.
  For example, to determine whether a list has length 0 we need to
  do a case-analysis (via pattern matching) to see whether it is nil
  or is non-empty.  The following function takes a list l and
  determines whether l is empty:
*)
let is_empty (l : 'a list) : bool =
  match l with [] -> true (* nil case -- return true *) | _ -> false
(* non-nil case -- return false *)

let ans1 : bool = is_empty [] (* evaluates to true *)

let ans2 : bool = is_empty list1 (* evaluates to false *)

(*
  Lists are an example of a "disjoint union" data type -- they are either
  empty [] or have some head value consed on to a tail h::tl.
  OCaml provides programmers with mechanisms to define their own generic
  disjoint union and potentially recursive types using the 'type' keyword.
*)

(*
  A user-defined generic type ('a mylist) can be defined within OCaml by:
*)

type 'a mylist =
  | Nil
  (* my version of [] *)
  | Cons of 'a * 'a mylist
(* Cons(h,tl) is my version of h::tl *)

(*
   We build a mylist by using its 'constructors' (specified in the branches)
   For example, compare mylist1 below to list1 defined by built-in lists
   above:
*)
let mylist1 : int mylist = Cons (3, Cons (2, Cons (1, Nil)))

(*
   Pattern matching against a user-defined datatype works the same as for
   a built-in type.  The cases we need to consider in a pattern are given by
   the cases of the type definition.  For example, to write the is_empty
   function for mylist we do the following.  Compare it with is_empty:
*)
let is_mylist_empty (l : 'a mylist) : bool =
  match l with Nil -> true | Cons _ -> false

(*
   IMPORTANT!! Be sure to learn this!

   Recursion:  The built in list type and the mylist type we defined above
   are recursive -- they are defined in terms of themselves.  To implement
   useful functions over such datatypes, we often need to use recursion.

   Recursive functions in OCaml use the 'rec' keyword -- inside the body
   of a recursive function, you can call the function being defined.  As usual
   you must be careful not to introduce infinite loops.

   Recursion plus pattern matching is a very powerful combination.  Here is
   a recursive function that sums the elements of an integer list:
*)

let rec sum (l : int list) : int =
  (* note the 'rec' keyword! *)
  match l with [] -> 0 | x :: xs -> x + sum xs
(* note the recursive call to sum *)

let sum_ans1 : int = sum [ 1; 2; 3 ] (* evaluates to 6 *)

(*
  Here is a function that takes a list and determines whether it is
  sorted and contains no duplicates according to the built-in
  generic inequality test <

  Note that it uses nested pattern matching to name the
  first two elements of the list in the third case of the match:
*)
let rec is_sorted (l : 'a list) : bool =
  match l with
  | [] -> true
  | [ _ ] -> true
  | h1 :: h2 :: tl -> h1 < h2 && is_sorted (h2 :: tl)

let is_sorted_ans1 : bool = is_sorted [ 1; 2; 3 ] (* true *)

let is_sorted_ans2 : bool = is_sorted [ 1; 3; 2 ] (* false *)

(*
   The List library
   (see http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html)
   implements many useful functions for list maniuplation.  You will
   recreate some of them in the exercises below.

   Here is map, one of the most useful:
*)
let rec map (f : 'a -> 'b) (l : 'a list) : 'b list =
  match l with [] -> [] | h :: tl -> f h :: map f tl

let map_ans1 : int list = map double [ 1; 2; 3 ] (* evaluates to [2;4;6]  *)

let map_ans2 : (int * int) list = map pair_up [ 1; 2; 3 ]
(* evaluates to [(1,1);(2,2);(3,3)] *)

(*
   The mylist type is isomorphic to the built-in lists.
   The recursive function below converts a mylist to a built-in list.
*)

let rec mylist_to_list (l : 'a mylist) : 'a list =
  match l with Nil -> [] | Cons (h, tl) -> h :: mylist_to_list tl

(* Part 3 Problems ---------------------------------------------------------- *)

(*
  Problem 3-1

  Implement list_to_mylist with the type signature below; this is
  the inverse of the mylist_to_list function given above.
*)
let rec list_to_mylist (l : 'a list) : 'a mylist =
  match l with [] -> Nil | x :: xs -> Cons(x, list_to_mylist xs)

(*
  Problem 3-2

  Implement the library function append, which takes two lists (of the same
  times) and concatenates them.  Do not use the library function or the built-in
  short-hand '@'.

  (append [1;2;3] [4;5]) should evaluate to [1;2;3;4;5]
  (append [] []) should evaluate to []

  Note that OCaml provides the infix fuction @ as an alternate way of writing
  append.  So (List.append [1;2] [3]) is the same as  ([1;2] @ [3]).
*)
let rec append (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with
  | [] -> l2
  | x :: xs -> x :: append xs l2

(*
  Problem 3-3

  Implement the library function rev, which reverses a list. In this solution,
  you might want to call append.  Do not use the library function.
*)
let rec rev (l : 'a list) : 'a list =
  match l with 
  | x :: xs -> append (rev xs) [x]
  | [] -> []

(*
  Problem 3-4

  Read Book Chapter 3.1.9 about "tail recursion" and implement a tail recursive
  version of rev. Note that you will need a helper function that takes an extra
  parameter -- it should be defined using a local let definition. The rev_t
  function itself should not be recursive. Tail recursion is important to
  efficiency -- OCaml will compile a tail recursive function to a simple loop.
*)
let rev_t (l : 'a list) : 'a list =
  let rec rev_aux l aux_l =
    match l with 
    | [] -> aux_l
    | x :: xs -> rev_aux xs (x :: aux_l)
  in rev_aux l []

(*
  Problem 3-5

  Implement insert, a function that, given an element x and a sorted list l
  (i.e. one for which is_sorted returns true) returns the list obtained by
  inserting x into the list l at the proper location.  Note that if x is
  already in the list then insert should just return the original list.

  You will need to use the if-then-else expression (see Book 2.3.4). Remember that
  OCaml is expression-oriented; "if t then e1 else e2" evaluates to either the
  value computed by e1 or the value computed by e2 depending on whether t
  evaluates to true or false.
*)
let rec insert (x : 'a) (l : 'a list) : 'a list =
  match l with
  | h :: t ->
    if x < h then x::l else 
      if x == h then l else h :: insert x t
  | [] -> [x]

(*
  Problem 3-6

  Implement union, a function that takes two sorted lists and returns the
  sorted list containing all of the elements from both of the two input lists.
  Hint: you might want to use the insert function that you just defined.
*)
let rec union (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with
  | h :: t -> union t (insert h l2)
  | [] -> l2
