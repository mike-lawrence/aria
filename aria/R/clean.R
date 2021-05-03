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

	#get some paths, create stan_tmp
	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	fs::file_delete(fs::dir_ls('stan_tmp',glob=paste0('*',mod_name,'*')))

	return(invisible(NULL))
}
