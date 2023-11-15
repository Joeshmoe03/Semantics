(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

(* The assert_eqf function takes two arguments and checks if they are equal. The *)
(* first argument is a function that takes an "empty" argument (),and *)
(* returns some value. The second argument is a function call (that will *)
(* return something) or a value. *)

(* For some of the problems, You will need to change the part with "failwith ..." *)
(* to something else. You may also add your own tests to check your code. *)

let student_tests : suite = [
  Test ("Student-Provided Tests For Problem 1-3", [
    ("case1", assert_eqf (fun () -> 42) prob3_ans );
    ("case2", assert_eqf (fun () -> 25) (prob3_case2 17) );
    ("case3", assert_eqf (fun () -> prob3_case3) 64);
  ]);

]
