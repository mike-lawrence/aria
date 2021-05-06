#' Sample the posterior given data & a Stan model
#'
#' @param data list() (or \code{tibble::lst()}) object containing the data.
#' @param code_path Character string describing the path to the Stan code.
#' @param num_chains Integer value indicating the number of chains. If NULL (the default), \code{\link{parallel::detectCores()}/2} will be used. If negative, \code{\link{parallel::detectCores()}/2-num_cores} will be used. Otherwise, \code{num_cores} will be used.
#' @param run_args_list list object with a two-level hierarchical structure matching what cmdstan expects in terms of runtime arguments. If NULL (the default), aria will select some defaults.
#'
#' @return NULL (invisibly); Side effects: \code{num_chains} sampling processes are launched in the background.
#' #' @export
#'
#' @examples
#' \dontrun{
#' #example for setting seed argument
#' sample(
#'     data = my_data
#'     , model = 'my_model.stan'
#'     , run_args_list = list(
#'         random = list(
#'             seed = 123
#'         )
#'     )
#' )
#' }
sample = function(
	data
	, code_path
	, num_chains = NULL
	, run_args_list = NULL
){

	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	exe_path = fs::path('aria',mod_name,'exe',fs::path_ext_remove(code_file))

	if(!fs::file_exists(exe_path)){
		stop(paste0('Compiled exe not found, run `aria::compile("',code_path,'")` first.'))
	}

	dat_json = jsonlite::toJSON(data,digits=NA,auto_unbox=T)
	dat_hash = digest::digest(dat_json,algo='xxhash64')
	dat_path = fs::path('aria',mod_name,dat_hash)
	fs::dir_create(dat_path)
	json_path = fs::path_ext_set(fs::path(dat_path,'data'),ext='.json')
	write(dat_json,json_path)

	if(is.null(num_chains)){
		num_chains = parallel::detectCores()/2
	}else{
		if(num_chains<0){
			num_chains = parallel::detectCores()/2 - num_chains
		}
	}

	if(is.null(run_args_list)){
		run_args_list = list(sample=list())
	}
	#aria-default deviations from the cmdstan-defaults
	run_args_list = add_run_arg_if_missing(run_args_list,'sample','save_warmup',1)
	run_args_list = add_run_arg_if_missing(run_args_list,'data','file',json_path)
	run_args_list = add_run_arg_if_missing(run_args_list,'random','seed',base::sample(.Machine$integer.max, 1))
	run_args_list = add_run_arg_if_missing(run_args_list,'output','refresh',0)
	run_args_list = add_run_arg_if_missing(run_args_list,'output','sig_figs',18)
	# run_args_list = add_run_arg_if_missing(run_args_list,'output','diagnostic_file','diagnostic.csv')

	#check for existing chains, start seed with num_existing+1
	fs::dir_ls(dat_path,regex='')
	found_chain_nums = as.numeric(fs::path_file(fs::dir_ls(dat_path,type='dir')))
	if(length(found_chain_nums)>0){
		chain_num_offset = max(found_chain_nums)
		cat(crayon::cyan('Found ',length(found_chain_nums),' existing chain folders.'))
	}else{
		chain_num_offset = 0
	}
	chain_num_sequence = (1+chain_num_offset):(num_chains+chain_num_offset)
	for(this_chain_num in chain_num_sequence){
		this_chain_path = fs::path(dat_path,this_chain_num)
		fs::dir_create(this_chain_path)
		run_args_list$output$file = fs::path(this_chain_path,'out.csv')
		this_process = processx::process$new(
			command = paste0('./',exe_path)
			, args = c(
				paste0('id=',this_chain_num)
				, run_args_list_to_vec(run_args_list)
			)
			, stdout = fs::path(this_chain_path,'stdout.txt')
			, stderr = fs::path(this_chain_path,'stderr.txt')
			, cleanup = FALSE
		)
		saveRDS(this_process$get_pid(), fs::path(this_chain_path,'pid.rds'))
	}
	cat(crayon::cyan('Started sampling for chains',min(chain_num_sequence),'through',max(chain_num_sequence),'\nUse `aria::monitor()` to launch a monitor job in RStudio.'))
	return(invisible(NULL))
}

#helper functions not exported ----
run_args_list_to_vec = function(x){
	out = c()
	for(i in 1:length(x)){
		out = c(out,names(x)[i])
		y = x[[i]]
		for(j in 1:length(y)){
			out = c(out,paste0(names(y)[j],'=',y[[j]]))
		}
	}
	return(out)
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
