#' Delete temporary files associated with a specific model
#'
#' @param code_path Character string describing the path to the Stan code.
#'
#' @return Returns NULL invisibly
#' @export
#'
#' @family Cleaning functions
#'
#' @examples
#' \dontrun{
#' clean('my_model.stan')
#' }
clean = function(code_path){
	#get some paths, create aria
	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	exe_dir = fs::path('aria','exes',mod_name)
	fs::dir_delete(exe_dir)
	return(invisible(NULL))
}
