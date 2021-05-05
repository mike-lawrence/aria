#' Delete temporary files associated with a specific model
#'
#' @param code_path Character string describing the path to the Stan code.
#' @param keep_exe Logical value (default:FALSE) indicating whether to keep the exe folder.
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
clean = function(code_path,keep_exe=F){
	#get some paths, create aria
	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	mod_path = fs::path('aria',mod_name)
	if(fs::dir_exists(mod_path)){
		if(!keep_exe){
			fs::dir_delete(mod_path)
		}else{
			fs::dir_delete(fs::dir_ls(mod_path,regex='exe',invert=T))
		}
	}
	return(invisible(NULL))
}
