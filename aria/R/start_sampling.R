#' Start sampling
#'
#' @param data list() (or \code{tibble::lst()}) object containing the data.
#' @param code_path Character string describing the path to the Stan code.
#' @param num_chains Integer value indicating the number of chains. If NULL (the default), \code{\link{parallel::detectCores()}/2} will be used. If negative, \code{\link{parallel::detectCores()}/2-num_cores} will be used. Otherwise, \code{num_cores} will be used.
#' @param chain_id_start Integer value (default: 1) indicating An offset for the numeric chain identifiers. Useful if you have already run a set of chains, collected the results, and want to run more chains.
#' @param exe_args_list list() object with a named hierarchical structure matching what exe expects in terms of runtime arguments (viewable via \code{\link{aria::exe_args}}). If NULL (the default), aria will select some defaults.
#' @param quiet Boolean value with FALSE (default) permitting the printing of a standard message.
#'
#' @return NULL (invisibly); Side effects: \code{num_chains} sampling processes are launched in the background.
#' @export
#'
#' @examples
#' \dontrun{
#' #example for setting seed argument
#' start_sampling(
#'     data = my_data
#'     , model = 'my_model.stan'
#'     , exe_args_list = list(
#'         random = list(
#'             seed = 123
#'         )
#'     )
#' )
#' }
start_sampling = function(
	data
	, code_path
	, num_chains = NULL
	, chain_id_start = 1
	, exe_args_list = NULL
	, quiet = FALSE
){

	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	run_path = fs::path('aria','runs')
	exe_path = fs::path('aria','exes',mod_name,mod_name)
	qs_path = fs::path('aria','exes',mod_name,mod_name,ext='qs')

	#look for exe
	if(!fs::file_exists(exe_path)){
		stop(paste0('Compiled exe not found, run `aria::compile("',code_path,'")` first.'))
	}

	# ensure run_path exists
	fs::dir_create(run_path)

	#look for existing csvs
	if(length(fs::dir_ls(path=run_path,recurse=T,regexp='.csv'))>0){
		stop('CSVs already found in aria dir. Run aria::clean_csv() first.')
	}

	#look for existing run_info
	if(fs::file_exists(fs::path(run_path,'run_info',ext='rds'))){
		stop('Existing run info found in aria dir. Is sampling already running? If not, run aria::clean_run() first.')
	}


	#prep the data json
	dat_json = jsonlite::toJSON(data,digits=NA,auto_unbox=T)
	json_path = fs::path_ext_set(fs::path(run_path,'data'),ext='.json')
	write(dat_json,json_path)

	if(is.null(num_chains)){
		num_chains = parallel::detectCores()/2
	}else{
		if(num_chains<0){
			num_chains = parallel::detectCores()/2 - num_chains
		}
	}

	if(is.null(exe_args_list)){
		exe_args_list = list(sample=list())
	}
	#aria-default deviations from the cmdstan-defaults
	exe_args_list = add_run_arg_if_missing(exe_args_list,'sample','save_warmup',1)
	exe_args_list = add_run_arg_if_missing(exe_args_list,'data','file',json_path)
	exe_args_list = add_run_arg_if_missing(exe_args_list,'random','seed',base::sample(.Machine$integer.max, 1))
	exe_args_list = add_run_arg_if_missing(exe_args_list,'output','refresh',0)
	exe_args_list = add_run_arg_if_missing(exe_args_list,'output','sig_figs',18)
	# exe_args_list = add_run_arg_if_missing(exe_args_list,'output','diagnostic_file','diagnostic.csv')

	#create the run info list
	run_info = tibble::lst(
		mod_meta = qs::qread(qs_path)
		, mod_name = mod_name
		, num_chains = num_chains
		, exe_args_list = exe_args_list
		, num_warmup = ifelse(
			is.null(exe_args_list$sample$num_warmup)
			, 1e3
			, exe_args_list$sample$num_warmup
		)
		, num_samples = ifelse(
			is.null(exe_args_list$sample$num_samples)
			, 1e3
			, exe_args_list$sample$num_samples
		)
		, num_total = num_samples+num_warmup
		, start_time = Sys.time()
		, chains = list()
	)

	#iterate to start the chains
	chain_num_sequence = chain_id_start:(num_chains+chain_id_start)
	for(this_chain_num in chain_num_sequence){
		this_chain_path = fs::path(run_path,this_chain_num)
		fs::dir_create(this_chain_path)
		exe_args_list$output$file = fs::path(this_chain_path,'out.csv')
		this_process = processx::process$new(
			command = paste0('./',exe_path)
			, args = c(
				paste0('id=',this_chain_num)
				, exe_args_list_to_vec(exe_args_list)
			)
			, stdout = fs::path(this_chain_path,'stdout.txt')
			, stderr = fs::path(this_chain_path,'stderr.txt')
			, cleanup = FALSE
		)
		this_chain_info = list()
		this_chain_info$id = this_chain_num
		this_chain_info$path = this_chain_path
		this_chain_info$pid = this_process$get_pid()
		run_info$chains[[as.character(this_chain_num)]] = this_chain_info
	}
	qs::qsave(
		run_info
		, fs::path(run_path,'run_info',ext='qs')
		, preset = 'fast'
	)
	if(!quiet){
		cat(crayon::cyan('Started sampling for chains',min(chain_num_sequence),'through',max(chain_num_sequence),'\nUse `aria::monitor()` to launch a monitor and collect results.'))
	}
	return(invisible(NULL))
}

#helper functions not exported ----
exe_args_list_to_vec = function(x,name=NULL){
	if(!is.list(x)){
		return(paste0(name,'=',x))
	}else{
		out = name
		for(i in 1:length(x)){
			out = c(out,exe_args_list_to_vec(x[[i]],names(x)[i]))
		}
		return(out)
	}
}
add_run_arg_if_missing = function(x,meta,name,value){
	if(is.null(x[[meta]])){
		x[[meta]] = list()
	}
	if(is.null(x[[meta]][[name]])){
		x[[meta]][[name]] = value
	}
	return(x)
}
