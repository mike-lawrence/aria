#' Collect output from aria
#'
#' @param out_path Character string describing the path to a file where the final sampling output was saved.

#' @return a
#' @export
#'
#' @examples
#' \dontrun{
#' post = aria::coda('sampled/sampled.qs')
#' post$print_info()
#' }
coda = function(out_path){
	return(class_score$new(out_path))
}
