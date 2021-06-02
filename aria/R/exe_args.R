#' Show the available arguments for a compiled cmdstan exe
#'
#' @param code_path Character string describing the path to the Stan code.
#'
#' @return NULL (invisibly); Side effects: Prints the help message listing all available arguments accepted by the cmdstan exe
#' @export
#'
#' @examples
#' \dontrun{
#' show_exe_args('my_model.stan')
#' }
show_exe_args = function(code_path){

	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	fast_exe_file = fs::path('aria','exes',mod_name,'fast')

	#look for exe
	if(!fs::file_exists(exe_path)){
		stop(paste0('Compiled exe not found, exe `aria::compile("',code_path,'")` first.'))
	}
	#execute
	out = processx::exe(
		command = paste0('./',fast_exe_file)
		, args = 'help-all'
	)
	crayon::blue(cat(out$stdout))
	if(!out$stderr!=''){
		cat(crayon::red(out$stderr))
	}
	return(invisible(NULL))
}

