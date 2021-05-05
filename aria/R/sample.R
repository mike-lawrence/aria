#' Title
#'
#' @param data list() (or \code{tibble::lst()}) object containing the data.
#' @param code_path Character string describing the path to the Stan code.
#'
#' @return NULL (invisibly); Side effects: background processes are
#' @export
#'
#' @examples
#' \dontrun{
#' sample(my_data,'my_model.stan')
#' }
sample = function(
	data
	, code_path
	, num_warmup = 1e3
	, num_samples = 1e3
	, num_chains = NULL
	, start_seed = 1
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
	cat(crayon::cyan('Starting sampling for chains',min(chain_num_sequence),'through',max(chain_num_sequence),'\n'))
	for(this_chain_num in chain_num_sequence){
		this_chain_path = fs::path(dat_path,this_chain_num)
		fs::dir_create(this_chain_path)
		this_process = processx::process$new(
			command = paste0('./',exe_path)
			, args = c(
				'sample'
				, paste0('num_samples=',num_samples)
				, paste0('num_warmup=',num_warmup)
				, 'save_warmup=1'
				, 'data'
				, paste0('file=',json_path)
				, 'output'
				, paste0('file=',fs::path(this_chain_path,'out.csv'))
				, paste0('diagnostic_file=',fs::path(this_chain_path,'diagnostic.csv'))
				, 'refresh=0'
				, 'sig_figs=18'
				, paste0('profile_file=',fs::path(this_chain_path,'profile.csv'))
				, 'random'
				, paste0('seed=',this_chain_num)
			)
			, stdout = fs::path(this_chain_path,'stdout.txt')
			, stderr = fs::path(this_chain_path,'stderr.txt')
			, cleanup = FALSE
		)
		saveRDS(this_process$get_pid(), fs::path(this_chain_path,'pid.rds'))
	}
	cat(crayon::cyan('Sampling started; use `aria::monitor()` to launch a monitor job in RStudio.'))
	return(invisible(NULL))
}
