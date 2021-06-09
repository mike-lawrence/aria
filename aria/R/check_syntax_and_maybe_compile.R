check_syntax_and_maybe_compile = function(code_path){

	fs::dir_create('aria')
	aria_args_file = fs::path('aria','aria_args',ext='qs')

	#check if this is the first compile this session:
	if(aria:::rstan_message_necessary){
		cat(aria:::cyan('Note to rstan users: To enhance Rstudio\'s "Check on save" feature for Stan files, aria has overriden the `rstan:::rstudio_stanc()` function. To undo this override, use `aria::disable_rstudio_syntax_compile()`. This message is shown the first time you use the "Check on save" within a session.\n\n'))
		utils::assignInNamespace(
			"rstan_message_necessary"
			, {FALSE}
			, ns = "aria"
			, envir = as.environment("package:aria")
		)
	}

	# set aria_sotto_vocce
	aria_sotto_vocce = getOption('aria_sotto_vocce')
	if(is.null(aria_sotto_vocce)){
		aria_sotto_vocce = FALSE
	}

	#set default aria_args
	aria_args = list(
		code_path = code_path
		, aria_sotto_vocce = aria_sotto_vocce
		, compile=0
	)

	#extract any supplied by user
	orig_warning_state = options(warn=-1)
	aria_args_lines = system2(
		command = "grep"
		, args = c(
			"^//aria:"
			, code_path
		)
		, stdout = TRUE
	)
	options(orig_warning_state)
	if(length(aria_args_lines)>0){
		(
			aria_args_lines
			%>% stringr::str_remove(stringr::fixed('//aria:'))
			%>% stringr::str_trim()
			%>% stringr::str_split(stringr::fixed('='))
		)-> aria_args_lines_split
		for(line_split in aria_args_lines_split){
			eval(parse(text=paste0(
				'aria_args[['
				, dQuote(line_split[1],F)
				, ']]='
				, line_split[2]
			)))
		}
	}

	#if compiling, save args & launch as job
	if(aria_args$compile){
		qs::qsave(aria_args,aria_args_file)
		temp_file = tempfile()
		write(
			'aria:::check_and_compile()'
			, file = temp_file
		)
		rstudioapi::jobRunScript(
			path = temp_file
			, name = paste0('Checking & compiling "',code_path,'"')
			, workingDir = getwd()
		)
	}else{
		aria:::check_syntax(aria_args)
	}
	return(invisible(NULL))

}
