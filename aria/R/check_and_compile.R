#' Check syntax & compile
#'
#' This

#' @param code_path Character string describing the path to the Stan code.
#'
#' @return NULL (invisibly)
#' @export
#'
#' @family Model checking & compilation functions
#'
#' @examples
#' \dontrun{
#' check_and_compile(
#' 	code_path = 'stan/my_mod.stan'
#' )
#' }
check_and_compile = function(code_path){
	aria:::check_syntax_and_maybe_compile(code_path,compile=1)
	return(invisible(NULL))
}
