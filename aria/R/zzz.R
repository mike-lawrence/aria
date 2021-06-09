rstan_message_necessary = FALSE
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
	options('aria_sotto_vocce' = FALSE)
	return(invisible(NULL))
}

#hack until crayon is fixed
red = function(x){
	paste0('\U001b[31m',x)
}
cyan = function(x){
	paste0('\U001b[36m',x)
}
blue = function(x){
	paste0('\U001b[34m',x)
}
