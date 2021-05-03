rstan_message_necessary = TRUE
.onLoad <- function(libname, pkgname){
	#only run if rstan is present
	if(nzchar(system.file(package='rstan'))){
		suppressWarnings(enable_rstudio_syntax_compile())
	}else{
		rstan_message_necessary = FALSE
		utils::assignInNamespace(
			"rstan_message_necessary"
			, {rstan_message_necessary}
			, ns = "aria"
			, envir = as.environment("package:aria")
		)
	}
	return(invisible(NULL))
}
