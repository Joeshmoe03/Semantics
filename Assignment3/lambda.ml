(* In this assignment, we will implement a simple evaluator for the Lambda
   Calculus. We will closely follow the operational semantics for both
   call-by-value and call-by-name evaluation. You will want to refer to the
   slides from the relevant classes posted on Canvas. This file will guide you
   to the proper solutions. I recommend you read the entire file to understand
   the assignment, and then work from the top down.


   Here is a general hint for OCaml that you may find useful in this assignment:
   If you want to write the same code for multiple cases in a match statement,
   you can avoid duplication as follows:

   If you have some type t = A | B | C | D and a variable v of type t,
   and you want to call some_code () for A, B and D, and other_code () for D,
   you may write:

   match v with
   | C -> other_code ()
   | A
   | B
   | C -> some_code ()

*)


(** Terms in the Lambda Calculus **)
(* First, let's start by defining a type to describe terms in the Lambda
   Calculus. Recall that a term in the Lambda Calculus is either a variable, an
   abstraction (a lambda function), or an application. An outline of the type
   constructors have been provided for you. You will have to fill in the
   associated types by replacing the ?? with the correct type. The type for
   variables has been provided for you.

   In addition to the core lambda calculus terms, let's also add constructors
   for integers and addition, so that we can write some useful programs. Note
   that for the addition case, you should make the types as general as possible
   so that you can write a larger set of interesting programs.

   Note that until you complete this part, utop will give you syntax errors.
*)
type term = Var of string
          | Lambda of string * term
          | App of term * term
          (* Let's add some constructor for integers and addition *)
          | Int of int
          | Add of term * term

(** Finding free variables of a term *)
(* Recall the definition of free variables from a class. A free variable in a
 * term is one that is not bound by a lambda variable. We will need to write a
 * function to find the free variables in a term, so that we can properly
 * define capture-avoiding substitution later. Refer to the slides to refresh
 * your memory of what these terms mean.

   This function should take a single term `t` as argument and return a list of
   strings representing all the free variables in this term.

   Hints:
   1. You may assume that an integer does not have any free variables.
   2. You may find some functions over lists useful. You can see the all the
   functions for lists here: https://v2.ocaml.org/api/List.html . In particular,
   you may want to use List.filter and append (@) in some places.
   3. You can test equality in OCaml with = and inequality with <>.

*)

let rec free (t:term) : string list =
   match t with
   | Int (_) -> []
   | Var (v) -> [v]
   | App (e1, e2) -> free e1 @ free e2
   | Add (e1, e2) -> free e1 @ free e2
   | Lambda (arg, e) -> List.filter (fun v -> v <> arg) (free e)

(** Capture-avoiding substitution *)
(* With our function to find free variables, we can now write a function that
   implements capture-avoiding substitution in lambda terms. Recall that
   capture-avoiding substitution is required to avoid capturing free variables,
   and substituting bound variables in inner functions. Look at the definition
   for capture-avoiding substitution in the slides and implement it in the
   following function.

   Specifically, the function subst should replace instances of the variable
   `var` in the term `body` with the term `term`, according to the rules defined
   in the slides.
*)
let rec subst (var:string) (body:term) (term:term) =
  match body with
  | Var (v) ->
      if var = v then term
      else body
  | Int i -> body
  | App (e1, e2) -> App (subst var e1 term, subst var e2 term)
  | Add (e1, e2) -> Add (subst var e1 term, subst var e2 term)
  | Lambda (x, body) ->
      if var = x || (List.mem x (free term)) then Lambda (x, body)
      else Lambda (x, subst var body term)

(** Implementing the semantics of the Lambda Calculus *)

(* With a proper substitution function, we are now ready to implement the
   semantics for the lambda calculus. We will implement two functions, one to
   evaluate lambda terms according to the call-by-value semantics, and one to
   evaluate them by the call-by-name semantics.
*)

(** Call by value **)
(* First let's implement the call-by-value semantics. Write a function that
 * takes a lambda calculus term and evaluates it by one step according to the
 * CBV semantics.

   Hints:
   1. You should be able to translate the semantics described in the slides to
   OCaml in a pretty straightforward way.
   2. A number of cases can be implemented with the same code. Think about what
   terms are already basic values in the lambda calculus.
   3. You will have to come up with reasonable semantics for integers and
   addition. Refer back to IMP and the Arithmetic language for ideas.
*)

let rec eval_cbv (term:term) = 
   match term with

   (* we always evaluate what we are applying first in CBV and then subst into Lambda *)
   | App (Lambda (x, e), v) -> eval_cbv (subst x e (eval_cbv v))

   (* Var and Int are simply irreducible terms *)
   | Var _ | Int _ -> term

   (*Try doing the left thing until there is nothing left to do, then do the right thing 
      -> Handles substitution already in other App case above *)
   | App (e1, e2) ->
      let e1' = eval_cbv e1 in 
      if e1' != e1 then App(e1', e2)
      else App (e1', eval_cbv e2)

   (* attempts to evaluate the expression in our Lambda once App(Lambda(...), eval x) w/ substitution case has happened *)
   | Lambda (arg, e) -> Lambda(arg, eval_cbv e)

   | Add(e1, e2) ->
      match e1, e2 with
      | Int a, Int b -> Int (a + b)
      | _, _ -> Add (eval_cbv e1, eval_cbv e2)
   
(** Call by name **)
(* Now we can implement call-by-name semantics for the lambda calculus.

   Hints:
   1. Again, refer to the slides and translate them to OCaml code.
   2. Much of the code for this function will be the same as the call-by-value
   function. Think carefully about which parts will be different.
*)
let rec eval_cbn term = (*failwith "call-by-name semantics unimplemented"*)
   match term with

   (* Substitute now, evaluate later -> no eval on v this time *)
   | App (Lambda (x, e), v) -> subst x e v

   (* Var and Int are simply irreducible terms *)
   | Var _ | Int _ -> term

   | App (e1, e2) ->
      let e1' = eval_cbv e1 in 
      if e1' != e1 then App(e1', e2)
      else App (e1', eval_cbv e2)

   | Lambda (arg, e) -> Lambda(arg, eval_cbv e)

   | Add(e1, e2) ->
      match e1, e2 with
      | Int a, Int b -> Int (a + b)
      | _, _ -> Add (eval_cbv e1, eval_cbv e2)
      
(** Testing **)
(* Here are example programs that should be usable once you have completed the
   definition of the type term above. You can uncomment them once you have
   written the type definition. These should be familiar from the material
   covered in class. *)

let identity = Lambda("x", Var("x"))
let half_loop = Lambda("y", App(Var("y"), Var("y")))
let loop = App(half_loop, half_loop)
let discard_1st = Lambda("x", identity)
let wrap_loop = App(discard_1st, loop)

(* Here are some functions that will test if the evaluation functions work
   using the example programs above. You will have to replace the condition
   `false` with appropriate checks to see if your functions work.

   Note that passing the following tests do not necessarily mean that your
   evaluation functions are correct, just that some important functionality is
   correct.
*)

let cbv_works () =

   (* I never eval_cbv wrap_loop in the cbv_works(), as it is equal to doing 
   (λx.λx.x)((λy.yy)(λy.yy)) which should NEVER terminate with the CBV method of evaluation. *)
  if eval_cbv (App(identity, Int(2))) = Int(2) 
  then print_endline "Call-by-name semantics work (probably)"
  else print_endline "Call-by-name semantics definitely do not work"

let cbn_works () =

   (* (λx.λx.x)((λy.yy)(λy.yy)) = λx.x -> the identity function
   with CBN, so it does terminate and I CAN test this. *)
   if eval_cbn (wrap_loop) = identity
   then print_endline "Call-by-name semantics work (probably)"
   else print_endline "Call-by-name semantics definitely do not work"

(* The following lines simply run the above functions. *)

let () = cbv_works ()
let () = cbn_works ()

(* Write 5-10 more lambda-calculus programs to further test your implementation.
   You may write any OCaml code that you need to test your implementations. *)
   let id = App(identity, Int(2))

   let double_id = Add(id, id)

   let res = App(identity, double_id)

   let omega = Lambda("x", App(Var("x"), Var("x")))

   let x = App(identity, App(omega, omega))

   (* SIMPLY CALL eval_cbv () or eval_cbn () on the variable names. I don't add any additional
      code assigned to var to run them as they will otherwise infinitely loop on some of the test cases if I 
      assign them - especially with '# use "lambda.ml"' in utop *)