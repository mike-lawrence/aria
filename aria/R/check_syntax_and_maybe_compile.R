check_syntax_and_maybe_compile = function(code_path,block=F,aria_args=NULL){

	fs::dir_create('aria')
	aria_args_file = fs::path('aria','aria_args',ext='rds')
	if(is.null(aria_args)){
		aria_args = list()
	}
	aria_args$code_path = code_path

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
	aria_args$aria_sotto_vocce = aria_sotto_vocce

	#extract any supplied by user
	orig_warning_state = options(warn=-1)
	aria_args_lines = system2(
		command = "grep"
		, args = c(
			"^//aria:"
			, dQuote(code_path,F)
		)
		, stdout = TRUE
	)
	options(orig_warning_state)
	if(length(aria_args_lines)>0){
		(
			aria_args_lines
			%>% stringr::str_remove(stringr::fixed('//aria:'))
			%>% stringr::str_trim()
			%>% stringr::str_split_fixed(stringr::fixed('='),n=2)
		)-> aria_args_lines_mat
		for(line_num in 1:nrow(aria_args_lines_mat)){
			(
				aria_args_lines_mat[line_num,1]
				%>% stringr::str_trim()
				%>% paste0("aria_args[['",.,"']]")
			) ->
				aria_args_list_string
			if(is.null(eval(parse(text=aria_args_list_string)))){
				(
					aria_args_lines_mat[line_num,2]
					%>% stringr::str_trim()
					%>% paste0(aria_args_list_string,'<<-',.)
					%>% parse(text=.)
					%>% eval()
				)

			}
		}
	}
	#if compiling, save args & launch as job
	if(aria_args$compile){
		saveRDS(aria_args,aria_args_file)
		temp_file = tempfile()
		write(
			'aria:::check_and_compile_as_job()'
			, file = temp_file
		)
		rstudioapi::jobRunScript(
			path = temp_file
			, name = paste0('Checking & compiling "',code_path,'"')
			, workingDir = getwd()
		)
		if(block){
			while(fs::file_exists(aria_args_file)){
				Sys.sleep(1)
			}
		}
	}else{
		aria:::check_syntax(aria_args)
	}
	return(invisible(NULL))

}
