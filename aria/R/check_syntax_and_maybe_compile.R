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
			dQuote("^\\/\\/\\s*aria:\\s*",F)
			, dQuote(aria_args$code_path,F)
		)
		, stdout = TRUE
	)
	options(orig_warning_state)
	if(length(aria_args_lines)>0){
		#parse lines to args_list
		(
			aria_args_lines
			%>% stringi::stri_remove_empty()
			%>% stringr::str_remove("^\\/\\/\\s*aria:\\s*")
			%>% stringr::str_trim()
			%>% tibble::tibble(line=.)
			%>% tidyr::separate(
				line
				, into = c('arg','value')
				, sep = '=|\\+='
				, extra = 'merge'
			)
			%>% dplyr::mutate(
				arg = stringr::str_trim(arg)
				, value = stringr::str_trim(value)
			)
			%>% dplyr::group_by(arg)
			%>% dplyr::group_split()
			%>% purrr::map(
				.f = ~list(arg=.x$arg[1],value=.x$value)
			)
			%>% purrr::set_names(.,purrr::map(.,'arg'))
			%>% purrr::map(.,'value')
			# %>% group_map(.f=~as.list(.x$value))
		) ->
			found_aria_args
		#add only those args not already present (so that programmatic calls of aria::check_and_compile(...) override found values)
		for(i in 1:length(found_aria_args)){
			if(!(names(found_aria_args)[i] %in% names(aria_args))){
				aria_args[[names(found_aria_args)[i]]] = found_aria_args[[i]]
			}
		}
	}
	aria_args$compile = validate_boolean_aria_arg(aria_args$compile,'compile',FALSE)
	aria_args$compile_debug = validate_boolean_aria_arg(aria_args$compile_debug,'compile_debug',TRUE)
	aria_args$run_debug = validate_boolean_aria_arg(aria_args$compile,'run_debug',TRUE)
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
