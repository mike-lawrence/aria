check_syntax = function(aria_args){
	code_file = fs::path_file(aria_args$code_path)
	mod_name = fs::path_ext_remove(code_file)

	stanc_syntax_check_run = processx::run(
		command = fs::path(cmdstanr::cmdstan_path(),cmdstanr:::stanc_cmd())
		, args = c(
			code_file
			, '--include-paths','.'
			, '--warn-pedantic'
			, '--warn-uninitialized'
			, '--name', mod_name
			,'--o', tempfile()
		)
		, wd = fs::path_dir(aria_args$code_path)
		, error_on_status = F
	)
	stderr = stanc_syntax_check_run$stderr
	if((stderr!='')&(!is.null(aria_args$syntax_ignore))){
		stderr_lines = stringr::str_split(stderr,'\n')[[1]]
		stderr_vec = NULL
		for(line in stderr_lines){
			toss = FALSE
			for(ignore_string in aria_args$syntax_ignore){
				if(stringr::str_detect(line,ignore_string)){
					toss = TRUE
				}
			}
			if(!toss){
				stderr_vec = c(stderr_vec,line)
			}
		}
		stderr = paste(stderr_vec,collapse='\n')
	}
	syntax_check_passed = !((stderr!='')|(stanc_syntax_check_run$stdout!=''))
	if(!syntax_check_passed){
		if(!getOption('aria_sotto_vocce')){
			beepr::beep(system.file("sounds/critical_stop.wav", package="aria"))
		}
		stanc_syntax_check_run$stdout = stringr::str_remove_all(stanc_syntax_check_run$stdout,stringr::fixed('./'))
		stanc_syntax_check_run$stdout = stringr::str_replace_all(stanc_syntax_check_run$stdout, 'Info: ', '\nInfo:\n')
		cat(aria:::blue(stanc_syntax_check_run$stdout),'\n\n',sep='')
		cat(aria:::red(stanc_syntax_check_run$stderr),'\n',sep='')
	}else{
		cat(aria:::blue('  ✓ Syntax check passed\n'))
	}
	if(sys.parent()==0){ #function is being called from the global env
		return(invisible(NULL))
	}else{
		return(syntax_check_passed)
	}
}
