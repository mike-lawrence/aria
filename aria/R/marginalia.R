#' Collect marginalia from aria
#'

#' @return a named list of tibbles : `time`, `stdout`, `stderr`
#' @export
#'
#' @examples
#' \dontrun{
#' aria::marginalia()
#' }
marginalia = function(){
	qs::qread(fs::path('aria','marginalia',ext='qs'))
}
