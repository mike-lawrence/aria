#' Derive a posterior through composition of a model & data
#'
#' @param data list() (or \code{tibble::lst()}) object containing the data.
#' @param code_path Character string describing the path to the Stan code.
#' @param out_path Character string describing the path to a file where the final sampling output should be saved (in .qs format)
#' @param num_chains Integer value indicating the number of chains. If NULL (the default), \code{\link{parallel::detectCores()}/2} will be used. If negative, \code{\link{parallel::detectCores()}/2-num_cores} will be used. Otherwise, \code{num_cores} will be used.
#' @param chain_num_start Integer value (default: 1) indicating An offset for the numeric chain identifiers. Useful if you have already run a set of chains, collected the results, and want to run more chains.
#' @param exe_args_list list() object with a named hierarchical structure matching what exe expects in terms of runtime arguments (viewable via \code{\link{aria::exe_args}}). If NULL (the default), aria will select some defaults.

#' @return NULL (invisibly); Side effects: \code{num_chains} sampling processes are launched in the background with progress monitored by an RStudio Job.
#' @export
#'
#' @examples
#' \dontrun{
#' #example for setting seed argument
#' compose(
#'     data = my_data
#'     , model = 'my_model.stan'
#'     , exe_args_list = list(
#'         random = list(
#'             seed = 123
#'         )
#'     )
#' )
#' }
#'
compose = function(
	data
	, code_path
	, out_path
	, overwrite = FALSE
	, num_chains = NULL
	, chain_num_start = 1
	, exe_args_list = NULL
){
	# as little as possible will happen in this function,
	#  saving the bulk for the composer

	# set aria_sotto_vocce
	aria_sotto_vocce = getOption('aria_sotto_vocce')
	if(is.null(aria_sotto_vocce)){
		aria_sotto_vocce = FALSE
	}

	mod_name = fs::path_ext_remove(fs::path_file(code_path))
	data_dir = fs::path('aria','data')
	run_dir = fs::path('aria','sampling')

	#ensure we can write the outputs
	if(!fs::dir_exists(fs::path_dir(out_path))){
		stop(aria:::red(paste0('The output directory "',fs::path_dir(out_path),'" does not exist.')))
	}
	if(fs::file_exists(out_path)){
		if(!overwrite){
			stop(aria:::red(paste0('The file "',out_path,'" already exists. If you wish to overwrite, set `overwrite=TRUE`')))
		}else{
			fs::file_delete(out_path)
		}
	}

	# ensure dirs exist
	fs::dir_create(data_dir)
	if(fs::dir_exists(run_dir)){
		fs::dir_delete(run_dir)
	}
	fs::dir_create(run_dir)

	#get the digest and thereby path
	data_digest = digest::digest(data,algo='xxhash64')
	data_file = fs::path(data_dir,data_digest,ext='json')
	#if it doesn't already exist, write as json
	if(!fs::file_exists(data_file)){
		write(
			x = jsonlite::toJSON(data,digits=NA,auto_unbox=T)
			, file = data_file
		)
	}

	#launch the conductor
	temp_file = tempfile()
	write(
		'aria:::conductor()'
		, file = temp_file
	)
	job_id = rstudioapi::jobRunScript(
		path = temp_file
		, name = paste('Composing',mod_name)
		, workingDir = getwd()
		# , exportEnv = 'R_GlobalEnv'
	)

	#write sampling_info
	sampling_info = list(
		aria_sotto_vocce = aria_sotto_vocce
		, mod_name = mod_name
		, code_path = code_path
		, out_path = out_path
		, num_chains = num_chains
		, chain_num_start = chain_num_start
		, exe_args_list = exe_args_list
		, data_file = data_file
		, job_id = job_id
	)
	qs::qsave(
		sampling_info
		, fs::path(run_dir,'info',ext='qs')
		, preset = 'fast'
	)
	return(invisible(NULL))
}

