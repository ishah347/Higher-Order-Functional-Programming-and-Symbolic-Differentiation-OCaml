(* 
			 CS 51 Problem Set 2
	    Higher Order Functional Programming -- Testing
			     Spring 2017
 *)

open Mapfold ;;
  
let test () =
  assert ((negate_all []) = []);
  assert ((negate_all [1; -2; 0]) = [-1; 2; 0]);
  assert ((negate_all [0]) = [0]);
  assert ((negate_all [-1; -2; -3]) = [1; 2; 3]);
  assert ((negate_all [1; 2; 3]) = [-1; -2; -3]);

  assert ((sum []) = 0);
  assert ((sum [0]) = 0);
  assert ((sum [1]) = 1);
  assert ((sum [1; 2; 3]) = 6);
  assert ((sum [-1; -2; -3]) = -6);
  assert ((sum [1; -2; 3]) = 2);

  assert ((sum_rows []) = []);
  assert ((sum_rows [[1; 3]; [2; 4]; [3; 3]]) = [4; 6; 6]);
  assert ((sum_rows [[-1; -3]; [-2; -4]]) = [-4; -6]);
  assert ((sum_rows [[1; 3]; [2; 4]]) = [4; 6]);
  assert ((sum_rows [[-1; 3]; [-2; 4]]) = [2; 2]);

  assert ((filter_odd []) = []);
  assert ((filter_odd [0; 2; 4; -6]) = []);
  assert ((filter_odd [1; 3; -5]) = [1; 3; -5]);
  assert ((filter_odd [1; 4; 5]) = [1; 5]);

  assert ((num_occurs 5 []) = 0);
  assert ((num_occurs (-5) [1; 3; 5; 5; 4]) = 0);
  assert ((num_occurs 5 [1; 3; 7; 7; 4]) = 0);
  assert ((num_occurs 5 [1; 3; 5; 5; 4]) = 2);

  assert ((super_sum []) = 0);
  assert ((super_sum [[]; []]) = 0);
  assert ((super_sum [[1]]) = 1);
  assert ((super_sum [[1; 2; 0]; []; [5]]) = 8);
  assert ((super_sum [[1; 2; 3]; [-1; -2; -3]]) = 0);

  assert ((filter_range [] (0, 1)) = []);
  assert ((filter_range [1] (0, 1)) = [1]);
  assert ((filter_range [-1; -3; -4; -5; -2] (-4, -1)) = [-1; -3; -4; -2]);
  assert ((filter_range [1; 3; 4; 5; 2] (1, 5)) = [1; 3; 4; 5; 2]);
  assert ((filter_range [1; 3; 4; 5; 2] (7, 8)) = []);
  assert ((filter_range [1; 3; 4; 5; 2] (4, 1)) = []);

  assert ((floats_of_ints []) = []);
  assert ((floats_of_ints [1; 2]) = [1.; 2.]);
  assert ((floats_of_ints [-1; 1; 2]) = [-1.; 1.; 2.]);

  assert ((log10s []) = []);
  assert ((log10s [-1.0; -10.0; 0.0]) = [None; None; None]);
  assert ((log10s [1.0; 10.0]) = [Some 0.; Some 1.]);
  assert ((log10s [-1.0; -10.0; 1.0; 10.0]) = [None; None; Some 0.; Some 1.]);

  assert ((deoptionalize []) = []);
  assert ((deoptionalize [None; None]) = []);
  assert ((deoptionalize [Some 6; Some 3]) = [6; 3]);
  assert ((deoptionalize [None; None; Some 6]) = [6]);
  assert ((deoptionalize [Some 6.]) = [6.]);
  assert ((deoptionalize [Some 'a']) = ['a']);
  assert ((deoptionalize [Some "a"]) = ["a"]);
  assert ((deoptionalize [Some true]) = [true]);
  assert ((deoptionalize [Some ()]) = [()]);

  assert ((some_sum []) = 0);
  assert ((some_sum [None]) = 0);
  assert ((some_sum [Some 1]) = 1);
  assert ((some_sum [Some 1; Some 2; Some 3]) = 6);
  assert ((some_sum [Some 1; Some (-2); None]) = -1);

  assert ((mult_odds []) = 1);
  assert ((mult_odds [0; 2; 4; -6]) = 1);
  assert ((mult_odds [1; 3; -5]) = -15);
  assert ((mult_odds [1; 4; 5]) = 5);

  assert ((concat []) = []);
  assert ((concat [[1]]) = [1]);
  assert ((concat [[]; []]) = []);
  assert ((concat [[1; 2]; [3; 4; 5]; [6; 7]]) = [1; 2; 3; 4; 5; 6; 7]);
  assert ((concat [[1; 2]; []; [3; 4; 5]; [6; 7]]) = [1; 2; 3; 4; 5; 6; 7]);
  assert ((concat [[6.]; [7.]]) = [6.; 7.]);
  assert ((concat [['a']; ['b']]) = ['a'; 'b']);
  assert ((concat [["a"]; ["b"]]) = ["a"; "b"]);
  assert ((concat [[true]; [false]]) = [true; false]);
  assert ((concat [[Some 6]; []]) = [Some 6]);

  assert ((filter_by_year [] 2010) = []);
  assert ((filter_by_year [("Joe", 2010); ("Bob", 2010); ("Ben", 2009)] 2011) = 
	[]);
  assert ((filter_by_year [("Joe", 2010); ("Bob", 2010); ("Ben", 2010)] 2010) = 
  	["Joe"; "Bob"; "Ben"]);
  assert ((filter_by_year [("Joe", 2011); ("Bob", 2010)] 2011) = ["Joe"]);

;;

test();;
print_endline "All tests passed.";;
