#' Check a Stan file for syntax errors & possibly compile (see description)
#'
#' This function calls \code{\link{check_syntax}}; if no syntax errors are detected, the model is then compiled with \code{\link{compile}} \strong{if-and-only-if} the stan file has a first line that is precisely \code{\\compile}.
#'
#' @param code_path Character string describing the path to the Stan code.
#'
#' @return NULL (invisibly); Side effects: if the model was compiled, an eponymous executable binary is placed in a folder called `stan_tmp` (along with some other helper files).
#' @export
#'
#' @family Model checking & compilation functions
#'
#' @examples
#' \dontrun{
#' check_and_maybe_compile('my_model.stan')
#' }
check_syntax_and_maybe_compile = function(code_path){

	#first a syntax check
	if(!check_syntax(code_path)){ #check_syntax returns TRUE if passed
		return(invisible(NULL))
	}
	#check if the compile string begins the stan file
	if(readLines(code_path,n=1)=='//compile:1'){
		compile(code_path)
	}

	return(invisible(NULL))
}

