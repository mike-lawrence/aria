check_and_compile_as_job = function(){
	aria_args_file = fs::path('aria','aria_args',ext='rds')
	aria_args = readRDS(aria_args_file)
	options('aria_sotto_vocce'=aria_args$aria_sotto_vocce)
	if(!aria:::check_syntax(aria_args)){ #check_syntax returns TRUE if passed
		return(invisible(NULL))
	}
	aria:::compile(aria_args)
	fs::file_delete(aria_args_file)
	return(invisible(NULL))
}
