#' Collect output from aria
#'
#' @param out_path Character string describing the path to a file where the final sampling output was saved.

#' @return a
#' @export
#'
#' @examples
#' \dontrun{
#' p = aria::coda('sampled/sampled.qs')
#' }
coda = function(out_path){
	nc = RNetCDF::open.nc(out_path)
	nc_groups = list()
	for(grp_name in c('adapt_info','sample_stats','parameters','transformed_parameters','generated_quantities')){
		nc_groups[[grp_name]] = RNetCDF::grp.inq.nc(nc,grp_name)$self
	}
	return(nc_groups)
}
