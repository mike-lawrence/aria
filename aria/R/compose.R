#' Derive a posterior through composition of a model & data
#'
#' @param data list() (or \code{tibble::lst()}) object containing the data.
#' @param code_path Character string describing the path to the Stan code.
#' @param num_chains Integer value indicating the number of chains. If NULL (the default), \code{\link{parallel::detectCores()}/2} will be used. If negative, \code{\link{parallel::detectCores()}/2-num_cores} will be used. Otherwise, \code{num_cores} will be used.
#' @param chain_id_start Integer value (default: 1) indicating An offset for the numeric chain identifiers. Useful if you have already run a set of chains, collected the results, and want to run more chains.
#' @param exe_args_list list() object with a named hierarchical structure matching what exe expects in terms of runtime arguments (viewable via \code{\link{aria::exe_args}}). If NULL (the default), aria will select some defaults.
#'
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
	, num_chains = NULL
	, chain_id_start = 1
	, exe_args_list = NULL
){
	# as little as possible will happen in this function,
	#  saving the bulk for the composer

	mod_name = fs::path_ext_remove(fs::path_file(code_path))
	data_dir = fs::path('aria','data')
	run_dir = fs::path('aria','sampling')

	# ensure dirs exist
	fs::dir_create(data_dir)
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
	write('aria:::conductor_()',file=temp_file)
	conductor_job_id = aria:::jobRunScript(
		path = temp_file
		, name = paste('Composing',mod_name)
		, workingDir = getwd()
		, exportEnv = 'R_GlobalEnv'
	)

	#write sampling_info
	sampling_info = list(
		mod_name = mod_name
		, code_path = code_path
		, num_chains = num_chains
		, chain_id_start = chain_id_start
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

