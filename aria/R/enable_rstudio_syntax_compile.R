#' Enable enhanced syntax check (inc. compile) in RStuidio
#'
#' This function overrides \code{\link{rstan::rstudio_stanc}} with \code{\link{aria::check_syntax_and_maybe_compile}} thus enhancing Rstudio's "source on save" feature. To undo this override, use \code{\link{aria::disable_rstudio_syntax_compile}}
#'
#' @return NULL (invisibly)
#' @export
#'
#' @family Model checking & compilation functions
#'
#' @examples
#' \dontrun{
#' enable_rstudio_syntax_compile()
#' }
enable_rstudio_syntax_compile = function(){
	#only run if rstan is present
	if(nzchar(system.file(package='rstan'))){
		utils::assignInNamespace(
			"rstudio_stanc"
			, check_syntax_and_maybe_compile
			, ns = "rstan"
			, envir = as.environment("package:rstan")
		)
	}
	return(invisible(NULL))
}
