open Expression ;;
open ExpressionLibrary ;;

let test () =
  assert (contains_var (parse "x+3"));
  assert (contains_var (parse "x+3."));
  assert (contains_var (parse "3-2*x"));
  assert (contains_var (parse "x^x+x"));
  assert (contains_var (parse "cos x"));
  assert (not (contains_var (parse "2")));
  assert (not (contains_var (parse "3+3")));
  assert (not (contains_var (parse "sin3")));
  assert (not (contains_var (parse "sin3.")));

  assert ((evaluate (parse "3.0") 3.0) = 3.);
  assert ((evaluate (parse "3") 1.0) = 3.);
  assert ((evaluate (parse "x") 2.0) = 2.);
  assert ((evaluate (parse "3") 1.0) = 3.);
  assert ((evaluate (parse "3+3") 1.0) = 6.);
  assert ((evaluate (parse "x+3") 1.0) = 4.);
  assert ((evaluate (parse "3+x") 1.0) = 4.);
  assert ((evaluate (parse "x+x") 1.0) = 2.);
  assert ((evaluate (parse "3-3") 1.0) = 0.);
  assert ((evaluate (parse "x-3") 1.0) = -2.);
  assert ((evaluate (parse "3-x") 1.0) = 2.);
  assert ((evaluate (parse "x-x") 1.0) = 0.);
  assert ((evaluate (parse "3*3") 1.0) = 9.);
  assert ((evaluate (parse "x*3") 1.0) = 3.);
  assert ((evaluate (parse "3*x") 1.0) = 3.);
  assert ((evaluate (parse "x*x") 1.0) = 1.);
  assert ((evaluate (parse "3/3") 1.0) = 1.);
  assert ((evaluate (parse "x/3") 6.0) = 2.);
  assert ((evaluate (parse "3/x") 6.0) = 0.5);
  assert ((evaluate (parse "x/x") 6.0) = 1.);
  assert ((evaluate (parse "3^3") 1.0) = 27.);
  assert ((evaluate (parse "x^3") 1.0) = 1.);
  assert ((evaluate (parse "3^x") 1.0) = 3.);
  assert ((evaluate (parse "x^x") 2.0) = 4.);
  assert ((evaluate (parse "sin1") 2.0) > 0.84147 &&
    (evaluate (parse "sin1") 2.0) < 0.84148);
  assert ((evaluate (parse "sinx") 2.0) > 0.90929 &&
    (evaluate (parse "sinx") 2.0) < 0.90930);
  assert ((evaluate (parse "cos1") 2.0) > 0.54030 &&
    (evaluate (parse "cos1") 2.0) < 0.54031);
  assert ((evaluate (parse "cosx") 2.0) > -0.41615 &&
    (evaluate (parse "cosx") 2.0) < -0.41614);
  assert ((evaluate (parse "ln1") 2.0) = 0.);
  assert ((evaluate (parse "lnx") 2.0) > 0.69314 &&
    (evaluate (parse "lnx") 2.0) < 0.69315);
  assert ((evaluate (parse "~1") 2.0) = -1.);
  assert ((evaluate (parse "~~1") 2.0) = 1.);
  assert ((evaluate (parse "~x") 2.0) = -2.);
  assert ((evaluate (parse "~~x") 2.0) = 2.);
  assert ((evaluate (parse "5+x*4") 2.0) = 13.);
  assert ((evaluate (parse "lnx+5") 1.0) = 5.);
  assert ((evaluate (parse "~ln(cosx)") 1.0) > 0.61562 &&
    (evaluate (parse "~ln(cosx)") 1.0) < 0.61563);
  assert ((evaluate (parse "sin(5+2-7)") 1.0) = 0.);
  assert ((evaluate (parse "sin(5+2*x-7)") 1.0) = 0.);

  assert ((evaluate (derivative (parse "3.0")) 3.0) = 0.);
  assert ((evaluate (derivative (parse "x")) 2.0) = 1.);
  assert ((evaluate (derivative (parse "3+3")) 1.0) = 0.);
  assert ((evaluate (derivative (parse "x+3")) 2.0) = 1.);
  assert ((evaluate (derivative (parse "3+x")) 2.0) = 1.);
  assert ((evaluate (derivative (parse "x+x")) 2.0) = 2.);
  assert ((evaluate (derivative (parse "3-2")) 1.0) = 0.);
  assert ((evaluate (derivative (parse "x-3")) 1.0) = 1.);
  assert ((evaluate (derivative (parse "3-x")) 1.0) = -1.);
  assert ((evaluate (derivative (parse "x-x")) 2.0) = 0.);
  assert ((evaluate (derivative (parse "3*3")) 1.0) = 0.);
  assert ((evaluate (derivative (parse "x*3")) 2.0) = 3.);
  assert ((evaluate (derivative (parse "3*x")) 2.0) = 3.);
  assert ((evaluate (derivative (parse "x*x")) 3.0) = 6.);
  assert ((evaluate (derivative (parse "3/3")) 1.0) = 0.);
  assert ((evaluate (derivative (parse "x/2")) 6.0) = 0.5);
  assert ((evaluate (derivative (parse "3/x")) 3.0) > -0.33334 &&
    (evaluate (derivative (parse "3/x")) 3.0) < -0.33333);
  assert ((evaluate (derivative (parse "x/x")) 6.0) = 0.);
  assert ((evaluate (derivative (parse "3^3")) 1.0) = 0.);
  assert ((evaluate (derivative (parse "x^3")) 2.0) = 12.);
  assert ((evaluate (derivative (parse "3^x")) 2.0) > 9.88751 && 
  	(evaluate (derivative (parse "3^x")) 2.0) < 9.88752);
  assert ((evaluate (derivative (parse "x^x")) 2.0) > 6.77258 && 
  	(evaluate (derivative (parse "x^x")) 2.0) < 6.77259);
  assert ((evaluate (derivative (parse "sin1")) 2.0) = 0.);
  assert ((evaluate (derivative (parse "sinx")) 2.0) > -0.41615 &&
    (evaluate (derivative (parse "sinx")) 2.0) < -0.41614);
  assert ((evaluate (derivative (parse "cos1")) 2.0) = 0.);
  assert ((evaluate (derivative (parse "cosx")) 2.0) > -0.90930 &&
    (evaluate (derivative (parse "cosx")) 2.0) < -0.90929);
  assert ((evaluate (derivative (parse "ln1")) 2.0) = 0.);
  assert ((evaluate (derivative (parse "ln3")) 2.0) = 0.);
  assert ((evaluate (derivative (parse "lnx")) 2.0) = 0.5);
  assert ((evaluate (derivative (parse "~1")) 2.0) = 0.);
  assert ((evaluate (derivative (parse "~~1")) 2.0) = 0.);
  assert ((evaluate (derivative (parse "~x")) 2.0) = -1.);
  assert ((evaluate (derivative (parse "~~x")) 2.0) = 1.);
  assert ((evaluate (derivative (parse "5+x^4")) 2.0) = 32.);
  assert ((evaluate (derivative (parse "lnx*5")) 1.0) = 5.);
  assert ((evaluate (derivative (parse "~ln(cosx)")) 1.0) > 1.55740 &&
    (evaluate (derivative (parse "~ln(cosx)")) 1.0) < 1.55741);
  assert ((evaluate (derivative (parse "sin(5+10-7)")) 1.0) = 0.);
  assert ((evaluate (derivative (parse "sin(5+2*x-7)")) 1.0) = 2.);

  assert ((find_zero (parse "2") 2. 2. 2) = None);
  assert ((find_zero (parse "2") 2. 2.1 2) = Some 2.);
  assert ((find_zero (parse "0") 30. 30. 30) = Some 30.);
  assert ((find_zero (parse "x") 0. 1. 0) = None);
  assert ((find_zero (parse "x") 0. 1. 0) = None);
  assert ((find_zero (parse "x") 0. 1. (-1)) = None);
  assert ((find_zero (parse "x") 0. 0. 1) = None);
  assert ((find_zero (parse "x") 0. (-1.) 1) = None);
  assert ((find_zero (parse "x") 0. 0.1 1) = Some 0.);
  assert ((find_zero (parse "(x-1)^3") 0.5 0.1 1) = None);
  assert ((find_zero (parse "(x-1)^3") 0.5 0.1 2) > Some 0.66666 &&
	(find_zero (parse "(x-1)^3") 0.5 0.1 2) < Some 0.66667);
  assert ((find_zero (parse "ln(x/2)") 1. 0.1 2) = None);
  assert ((find_zero (parse "ln(x/2)") 1. 0.5 2) > Some 1.69314 &&
	(find_zero (parse "ln(x/2)") 1. 0.5 2) < Some 1.69315);


  assert (contains_var (parse "3-2*x"));
  assert (contains_var (parse "x^x+x"));
  assert (contains_var (parse "cos x"));
  assert (not (contains_var (parse "2")));
  assert (not (contains_var (parse "3+3")));
  assert (not (contains_var (parse "sin3")));
  assert (not (contains_var (parse "sin3.")));

;;

test();;
print_endline "All tests passed.";;
