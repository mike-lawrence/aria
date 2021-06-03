check_syntax_and_maybe_compile_ = function(code_path){
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
		aria_sotto_vocce = getOption('aria_sotto_vocce')
		if(is.null(aria_sotto_vocce)){
			aria_sotto_vocce = 'NULL'
		}

		temp_file = tempfile()
		write(
			paste0('aria:::check_and_compile_("',code_path,'",aria_sotto_vocce=',aria_sotto_vocce,')')
			, file = temp_file
		)
		rstudioapi::jobRunScript(
			path = temp_file
			, name = paste0('Checking & compiling "',code_path,'"')
			, workingDir = getwd()
		)
	}else{
		aria:::check_syntax_(code_path)
	}
	return(invisible(NULL))

}
check_and_compile_ = function(code_path,aria_sotto_vocce){
	if(is.null(aria_sotto_vocce)){ aria_sotto_vocce = FALSE }
	options('aria_sotto_vocce'=eval(parse(text=aria_sotto_vocce)))
	if(!aria:::check_syntax_(code_path)){ #check_syntax returns TRUE if passed
		return(invisible(NULL))
	}
	aria:::compile_(code_path)
	return(invisible(NULL))
}
