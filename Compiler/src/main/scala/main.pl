lambda_func4(XInner, Rtn4) :- Rtn4 is XInner.
lambda_func12(Rtn12) :- lambda_func4(5, R0), Rtn12 is R0.
func_f(XX, RtnV21) :- (true, true, lambda_func12(Rtn12), RtnV21 is Rtn12 ; RtnV21 is XX).

