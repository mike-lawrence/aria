#' Check a Stan file for syntax errors & possibly compile (see description)
#'
#' This function calls \code{\link{check_syntax}}; if no syntax errors are detected, the model is then compiled with \code{\link{compile}} \strong{if-and-only-if} the stan file has a first line that is precisely \code{\\compile:1}.
#'
#' @param code_path Character string describing the path to the Stan code.
#'
#' @return NULL (invisibly); Side effects: if the model was compiled, an eponymous executable binary is placed in a folder called `aria` (along with some other helper files).
#' @export
#'
#' @family Model checking & compilation functions
#'
#' @examples
#' \dontrun{
#' check_and_maybe_compile('my_model.stan')
#' }
check_syntax_and_maybe_compile = function(code_path){
	#check if this is the first compile this session:
	if(aria:::rstan_message_necessary){
		cat(crayon::cyan('Note to rstan users: To enhance Rstudio\'s "Check on save" feature for Stan files, aria has overriden the `rstan:::rstudio_stanc()` function. To undo this override, use `aria::disable_rstudio_syntax_compile()`. This message is shown the first time you use the "Check on save" within a session.\n\n'))
		utils::assignInNamespace(
			"rstan_message_necessary"
			, {FALSE}
			, ns = "aria"
			, envir = as.environment("package:aria")
		)
	}
	if(readLines(code_path,n=1)=='//compile:1'){
		temp_file = tempfile()
		write(
			paste0('aria:::check_and_compile_("',code_path,'")')
			, file = temp_file
		)
		rstudioapi::jobRunScript(
			path = temp_file
			, name = paste0('Checking & compiling "',code_path,'"')
			, workingDir = getwd()
		)
	}else{
		check_syntax(code_path)
	}
	return(invisible(NULL))

}
check_and_compile_ = function(code_path){
	if(!check_syntax(code_path)){ #check_syntax returns TRUE if passed
		return(invisible(NULL))
	}
	compile(code_path)
	return(invisible(NULL))
}
