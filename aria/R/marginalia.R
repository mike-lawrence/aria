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
	readRDS(fs::path('aria','marginalia',ext='rds'))
}
