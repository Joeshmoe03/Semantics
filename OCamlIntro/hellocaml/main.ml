
exception Ran_tests
let suite = student_tests @ graded_tests

let exec_tests () =
  let o = run_suite suite in
  Printf.printf "%s\n" (outcome_to_string o) ;
  raise Ran_tests
  
(*let () = exec_tests ()*)

