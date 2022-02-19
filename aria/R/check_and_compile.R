#' Check syntax & compile
#'
#' A function intended primarily for programmatic checks/compilation. Values suplied as arguments will override any conflicting values specified in the .stan file on \"\\\\aria ..." lines


#' @param code_path Character string describing the path to the Stan code.
#' @param block Boolean (default: FALSE) indicating whether to block the main R console process until complete.
#' @param syntax_ignore Character string (or vector thereof); any syntax warnings including this string will be ignored.
#' @param make_local Character string (or vector thereof); lines to write to make/local during compilation of performance exe.
#' @param compile Boolean (default: TRUE) indicating whether to compile the model.
#' @param compile_debug Boolean (default: TRUE) indicating whether to compile the debugging version of the model.
#' @param run_debug Boolean (default: TRUE) indicating whether to run the debugging version of the model using automatically-generated synthetic data.
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
check_and_compile = function(
	code_path
	, block = FALSE
	, syntax_ignore = NULL
	, make_local = NULL
	, compile = TRUE
	, compile_debug = NULL
	, run_debug = NULL
){
	if(!is.null(compile)){
		if(!(compile %in% c(T,F))){
			cat(aria:::red('Oops! "compile" argument to aria::check_and_compile must be NULL or a boolean.'))
		}
	}
	aria:::check_syntax_and_maybe_compile(
		code_path
		, block = TRUE
		, aria_args = list(
			syntax_ignore = syntax_ignore
			, make_local = make_local
			, compile = compile
			, compile_debug = compile_debug
			, run_debug = run_debug
		)
	)
	return(invisible(NULL))
}
