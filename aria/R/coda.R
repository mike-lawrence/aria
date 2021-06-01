#' Collect output from aria
#'
#' @return a named list with elements `samples`, `stdout`, `stderr`, `info`, & `time`.
#' @export
#'
#' @examples
#' \dontrun{
#' p = aria::coda()
#' }
coda = function(){
	qs::qread(fs::path('aria','sampled',ext='qs'))
}
