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
	if(fs::dir_exists('aria')){
		fs::dir_delete('aria')
	}
	return(invisible(NULL))
}
