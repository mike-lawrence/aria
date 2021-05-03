#' Check Stan code for syntax errors
#'
#' @param code_path Character string describing the path to the Stan code.
#'
#' @return Returns NULL invisibly if called from the global environment; if called from inside a function, returns a boolean value with TRUE indicating that the check has passed.
#' @export
#'
#' @family Model checking & compilation functions
#'
#' @examples
#' \dontrun{
#' check_syntax('stan_temp/my_model.stan')
#' }
check_syntax = function(code_path){

	#get some paths, create stan_temp
	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	fs::dir_create('stan_temp')

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
		, wd = fs::path_dir(code_path)
		, error_on_status = F
	)
	if((stanc_syntax_check_run$stdout!='')|(stanc_syntax_check_run$stderr!='')){
		stanc_syntax_check_run$stdout = stringr::str_remove_all(stanc_syntax_check_run$stdout,stringr::fixed('./'))
		stanc_syntax_check_run$stdout = stringr::str_replace_all(stanc_syntax_check_run$stdout, 'Info: ', '\nInfo:\n')
		cat(crayon::blue(stanc_syntax_check_run$stdout),'\n\n',sep='')
		cat(crayon::red(stanc_syntax_check_run$stderr),'\n',sep='')
		syntax_check_passed = FALSE
	}else{
		cat(crayon::blue('  ✓ Syntax check passed\n'))
		syntax_check_passed = TRUE
	}
	if(sys.parent()==0){ #function is being called from the global env
		return(invisible(NULL))
	}else{
		return(syntax_check_passed)
	}
}
