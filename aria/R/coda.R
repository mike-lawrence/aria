#' Collect output from aria
#'
#' @param out_path Character string describing the path to a file where the final sampling output was saved.

#' @return a tibble with samples, plus attributes: `stdout`, `stderr`, `adapt_info`, & `sampling_info`
#' @export
#'
#' @examples
#' \dontrun{
#' p = aria::coda('sampled/sampled.qs')
#' }
coda = function(out_path){
	qs::qread(out_path)
}
