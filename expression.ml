(*
                         CS 51 Problem Set 2
                 A Language for Symbolic Mathematics
                             Spring 2018
 *)

(*======================================================================
Problem 2: A language for symbolic mathematics

Before reading this code (or in tandem), read the problem set 2
writeup. It provides context and crucial information for completing
the problems.

The type definition of the expression type can be found at the top of
expressionLibrary.ml. You will be using this ADT for this part of the
problem set. In that file, we also provide you with unary and binary
operators that operate on expressions.

The module ExpressionLibrary is opened to provide you with access to
the expression data type and helpful functions that you will use for
this part of the problem set.
......................................................................*)

open ExpressionLibrary ;;

(*......................................................................
Tips for problem 2:

1. READ THE WRITEUP, particularly for the derivative function.
 
2. Use the type definitions provided at the top of
   expressionLibrary.ml as a reference, and don't change any of the
   code in that file. It provides functions such as "parse" and
   "to_string_smart" that will be helpful in this part of the problem
   set.
......................................................................*)

(*......................................................................
Problem 2.1: The function "contains_var" tests whether an 
expression contains a variable "x". For example:

# contains_var (parse "x^4") ;;
- : bool = true
# contains_var (parse "4+3") ;;
- : bool = false
......................................................................*)

let rec contains_var (e : expression) : bool =
  match e with
  | Num _ -> false
  | Var -> true
  | Binop (_, a, b) -> contains_var a || contains_var b
  | Unop (_, a) -> contains_var a ;;

(*......................................................................
Problem 2.2: The function "evaluate" evaluates an expression for a
particular value of x. Don't worry about specially handling the
"divide by zero" case. For example:

# evaluate (parse "x^4 + 3") 2.0
- : float = 19.0
......................................................................*)

let rec evaluate (e : expression) (x : float) : float =
  match e with
  | Num n -> n
  | Var -> x
  | Binop (b, e1, e2) ->
      (let ev1, ev2 = evaluate e1 x, evaluate e2 x in
      match b with
      | Add -> ev1 +. ev2
      | Sub -> ev1 -. ev2
      | Mul -> ev1 *. ev2
      | Div -> ev1 /. ev2
      | Pow -> ev1 ** ev2)
  | Unop (u, e1) ->
      let ev1 = evaluate e1 x in
      match u with
      | Sin -> sin ev1
      | Cos -> cos ev1
      | Ln -> log ev1
      | Neg -> -. ev1 ;;

(*......................................................................
Problem 2.d: The "derivative" function returns the expression that
represents the derivative of the argument expression. We provide the
skeleton of the implementation here along with a few of the cases;
you're responsible for filling in the remaining parts that implement
the derivative transformation provided in the figure in the
writeup. See the writeup for instructions.
......................................................................*)

let rec derivative (e : expression) : expression =
  match e with
  | Num _ -> Num 0.
  | Var -> Num 1.
  | Unop (u, e1) ->
     (match u with
      | Sin -> Binop (Mul, Unop (Cos, e1), derivative e1)
      | Cos -> Binop (Mul, Unop (Neg, Unop (Sin, e1)), derivative e1)
      | Ln -> Binop (Div, derivative e1, e1)
      | Neg -> Unop (Neg, derivative e1))
  | Binop (b, e1, e2) ->
     match b with
     | Add -> Binop (Add, derivative e1, derivative e2)
     | Sub -> Binop (Sub, derivative e1, derivative e2)
     | Mul -> Binop (Add, Binop (Mul, e1, derivative e2),
                    Binop (Mul, derivative e1, e2))
     | Div -> Binop (Div, Binop (Sub, Binop (Mul, derivative e1, e2), 
                    Binop (Mul, e1, derivative e2)), Binop (Pow, e2, Num 2.))
     | Pow ->
        (* split based on whether the exponent has any variables *)
        if contains_var e2
        then Binop (Mul, Binop (Pow, e1, e2), 
                   Binop (Add, Binop (Mul, derivative e2, Unop (Ln, e1)), 
                         Binop (Div, Binop (Mul, derivative e1, e2), e1)))
        else Binop (Mul, Binop (Mul, e2, derivative e1), 
                   Binop (Pow, e1, Binop (Sub, e2, Num 1.))) ;;
     
(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
  print_string ("Checking expression: " ^ strs ^ "\n");
  let parsed = parse strs in
  (print_string "contains variable : ";
   print_string (string_of_bool (contains_var parsed));
   print_endline " ";
   print_string "Result of evaluation: ";
   print_float (evaluate parsed xval);
   print_endline " ";
   print_string "Result of derivative: ";
   print_endline " ";
   print_string (to_string (derivative parsed));
   print_endline " ") ;;
  
(*......................................................................
Problem 2.4: Zero-finding. See writeup for instructions.
......................................................................*)

let rec find_zero (expr : expression)
              (guess : float)
              (epsilon : float)
              (limit : int)
            : float option =
  let ev_guess = evaluate expr guess in         
  let next_guess = guess -. (ev_guess /. (evaluate (derivative expr) guess)) in
  if limit > 0 then 
    if abs_float ev_guess < epsilon then Some guess
    else find_zero expr next_guess epsilon (limit - 1)
  else None ;;  

(*......................................................................
Problem 2.5: Challenge problem -- exact zero-finding. This problem is
not counted for credit and is not required. Just leave it
unimplemented if you do not want to do it. See writeup for
instructions.
......................................................................*)

let find_zero_exact (e : expression) : expression option =
  failwith "find_zero_exact not implemented" ;;

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_part2 () : int = 240 ;;
