#' Delete ALL temporary files
#'
#' @return Returns NULL invisibly
#' @export
#'
#' @family Cleaning functions
#'
#' @examples
#' \dontrun{
#' clean_all()
#' }
clean_all = function(){
	if(fs::dir_exists('stan_tmp')){
		fs::dir_delete('stan_tmp')
	}
	return(invisible(NULL))
}
