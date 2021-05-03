.onLoad <- function(libname, pkgname){
	packageStartupMessage('Loading aria')
	#only run if rstan is present
	if(nzchar(system.file(package='rstan'))){
		suppressWarnings(enable_rstudio_syntax_compile())
		packageStartupMessage('To enhance Rstudio\'s "Check on save" feature for Stan files, aria has overriden the `rstan:::rstudio_stanc` function. To undo this override, use `aria::disable_rstudio_syntax_compile`.')
	}
	return(invisible(NULL))
}
