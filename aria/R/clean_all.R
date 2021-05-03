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
	fs::dir_delete('stan_tmp')
	return(invisible(NULL))
}
