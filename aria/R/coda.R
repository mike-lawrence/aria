#' Collect output from aria
#'
#' @param out_path Character string describing the path to a file where the final sampling output was saved.

#' @return An instance of the "composition" class, with methods `nc_info()` and `draws()`.
#' @export
#'
#' @examples
#' \dontrun{
#' post = aria::coda('sampled/sampled.nc')
#' post$nc_info()
#' post$draws(variables='means') #draws_rvars format
#' post$draws(groups='generated_quantities') #can specify groups instead
#' }
coda = function(out_path){
	return(class_compositon$new(out_path))
}
