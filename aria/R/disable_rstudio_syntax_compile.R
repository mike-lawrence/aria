#' Disable enhanced syntax check (inc. compile) in RStuidio
#'
#' This function returns Rstudio (& rstan) to their default behaviour.
#'
#' @return NULL (invisibly)
#' @export
#'
#' @family Model checking & compilation functions
#'
#' @examples
#' \dontrun{
#' disable_rstudio_syntax_compile()
#' }
disable_rstudio_syntax_compile = function(){
	#code copied (& `rstan:::` added) from rstan
	#  maybe better to use system.file to source it?
	rstan_rstudio_stanc = function (filename) {
		output <- rstan:::stanc(filename, allow_undefined = TRUE)
		message(filename, " is syntactically correct.")
		return(invisible(output))
	}
	utils::assignInNamespace(
		"rstudio_stanc"
		, rstan_rstudio_stanc
		, ns = "rstan"
		, envir = as.environment("package:rstan")
	)
	return(invisible())
}
