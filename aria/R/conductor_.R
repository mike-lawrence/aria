#' @importFrom magrittr "%>%"

conductor_ = function(aria_sotto_vocce){
	if(is.null(aria_sotto_vocce)){ aria_sotto_vocce = FALSE }
	options('aria_sotto_vocce'=eval(parse(text=aria_sotto_vocce)))
	Sys.sleep(1) #to give sample() time to save sampling_info

	sampling_info_file = fs::path('aria','sampling','info',ext='qs')
	sampling_info = qs::qread(sampling_info_file)
	attach(sampling_info)
	# sampling_info has:
	# 	mod_name
	# 	code_path
	#	num_chains
	# 	chain_id_start
	# 	exe_args_list
	# 	data_file
	#	job_id

	run_dir = fs::path('aria','sampling')
	debug_exe_file = fs::path('aria','exes',mod_name,'debug')
	fast_exe_file = fs::path('aria','exes',mod_name,'fast')
	mod_info_file = fs::path('aria','exes',mod_name,'info',ext='qs')

	#look for exe
	if(!fs::file_exists(fast_exe_file)){
		stop(crayon::red('Compiled exe not found, run `aria::compile("',code_path,'")` first.'))
	}
	#look for existing csvs
	if(length(fs::dir_ls(path=run_dir,recurse=T,regexp='.csv'))>0){
		stop(crayon::red('CSVs already found in aria dir. Run aria::clean_csv() first.'))
	}

	if(!runtime_check_(debug_exe_file,data_file)){
		return(invisible(NULL))
	}


	if(is.null(num_chains)){
		num_chains = parallel::detectCores()/2
	}else{
		if(num_chains<0){
			num_chains = parallel::detectCores()/2 - num_chains
		}
	}
	sampling_info$num_chains = num_chains

	if(is.null(exe_args_list)){
		exe_args_list = list(sample=list())
	}
	#aria-default deviations from the cmdstan-defaults
	exe_args_list = add_run_arg_if_missing(exe_args_list,'sample','save_warmup',1)
	exe_args_list = add_run_arg_if_missing(exe_args_list,'data','file',data_file)
	exe_args_list = add_run_arg_if_missing(exe_args_list,'random','seed',base::sample(.Machine$integer.max, 1))
	exe_args_list = add_run_arg_if_missing(exe_args_list,'output','refresh',0)
	exe_args_list = add_run_arg_if_missing(exe_args_list,'output','sig_figs',18)
	# exe_args_list = add_run_arg_if_missing(exe_args_list,'output','diagnostic_file','diagnostic.csv')
	sampling_info$exe_args_list = exe_args_list

	sampling_info$mod_info = qs::qread(mod_info_file)

	#create the run info list
	sampling_info$num_warmup = ifelse(
		is.null(exe_args_list$sample$num_warmup)
		, 1e3
		, exe_args_list$sample$num_warmup
	)

	sampling_info$num_samples = ifelse(
		is.null(exe_args_list$sample$num_samples)
		, 1e3
		, exe_args_list$sample$num_samples
	)
	sampling_info$num_total = sampling_info$num_samples+sampling_info$num_warmup
	sampling_info$start_time = Sys.time()
	sampling_info$chains = list()

	#iterate to start the chains
	chain_num_sequence = chain_id_start:(sampling_info$num_chains+chain_id_start)
	for(this_chain_num in chain_num_sequence){
		this_chain_dir = fs::path(run_dir,this_chain_num)
		fs::dir_create(this_chain_dir)
		exe_args_list$output$file = fs::path(this_chain_dir,'out.csv')
		this_process = processx::process$new(
			command = paste0('./',fast_exe_file)
			, args = c(
				paste0('id=',this_chain_num)
				, exe_args_list_to_vec(exe_args_list)
			)
			, stdout = fs::path(this_chain_dir,'stdout.txt')
			, stderr = fs::path(this_chain_dir,'stderr.txt')
			, cleanup = FALSE
		)
		this_chain_info = list()
		this_chain_info$id = this_chain_num
		this_chain_info$path = this_chain_dir
		this_chain_info$pid = this_process$get_pid()
		sampling_info$chains[[as.character(this_chain_num)]] = this_chain_info
	}

	qs::qsave(sampling_info,sampling_info_file,preset='fast')
	cat(crayon::cyan('Started sampling for chains',min(chain_num_sequence),'through',max(chain_num_sequence),'\n'))

	#init lists of lists
	out = list(
		samples = list()
		, stdout = list()
		, stderr = list()
	)
	#loop until no more pids
	while(length(sampling_info$chains)){
		#save current state
		qs::qsave(sampling_info,sampling_info_file,preset='fast')

		#update job status
		rstudioapi::jobSetStatus(
			sampling_info$job_id
			, paste0(
				length(sampling_info$chains)
				, ' chains running, '
				, sampling_info$num_chains - length(sampling_info$chains)
				, ' chains completed'
			)
		)

		#get list of running processes
		all_running_processes = ps::ps_pids()
		for(chain in sampling_info$chains){
			if(!(chain$pid %in% all_running_processes)){
				#chain is complete, gather
				out$stdout[[as.character(chain$id)]] = read_stan_std(fs::path(chain$path,'stdout.txt'))
				out$stderr[[as.character(chain$id)]] = read_stan_std(fs::path(chain$path,'stderr.txt'))
				out$samples[[as.character(chain$id)]] = read_stan_csv_samples(fs::path(chain$path,'out.csv'))
				fs::dir_delete(chain$path)
				sampling_info$chains[[which(names(sampling_info$chains)==chain$id)]] = NULL
			}
		}
	}
	fs::dir_delete(fs::path('aria','sampling'))
	for(i in 1:length(out)){
		out[[i]] = dplyr::bind_rows(out[[i]],.id='chain')
	}
	out$info = sampling_info
	out$time = times_from_sampled(out)
	qs::qsave(
		out
		, file = fs::path('aria','sampled',ext='qs')
		, preset = 'fast'
	)
	if(!getOption('aria_sotto_vocce')){
		beepr::beep(system.file("sounds/tada.wav", package="aria"))
	}
	cat(crayon::cyan('Composition complete. To access the results, see ?aria::coda()'))

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
add_run_arg_if_missing = function(x,info,name,value){
	if(is.null(x[[info]])){
		x[[info]] = list()
	}
	if(is.null(x[[info]][[name]])){
		x[[info]][[name]] = value
	}
	return(x)
}

add_attr = function(data,name,value){
	attr(data,name) = value
	return(data)
}

read_stan_std = function(x){
	chars = readChar(x,file.info(x)$size,useBytes=TRUE)
	messages = stringi::stri_remove_empty(
		stringr::str_split(chars,stringr::fixed('\n\n'))[[1]]
	)
	if(length(messages)>0){
		return(tibble::tibble(message=messages,order=1:length(messages)))
	}else{
		return(NULL)
	}
}

read_stan_csv_comments = function(x){
	data.table::fread(
		cmd = paste0("grep '^[#a-zA-Z]' --color=never '", x, "'")
		, data.table = FALSE
		, colClasses = "character"
		, col.names = 'V1'
		, stringsAsFactors = FALSE
		, fill = TRUE
		, sep = ""
		, header = FALSE
	)$V1
}

read_stan_csv_samples = function(x){(
	data.table::fread(
		cmd = paste0("grep -v '^#' --color=never '", x, "'")
		, data.table = FALSE
		, sep = ','
		, colClasses = 'double'
		, header = TRUE
	)
	%>% tibble::as_tibble()
	%>% dplyr::mutate(sample = 1:dplyr::n())
	%>% dplyr::select(sample,dplyr::everything())
)}

times_from_sampled = function(sampled){
	elapsed = dplyr::filter(sampled$stdout,stringr::str_starts(message,' Elapsed Time'))
	if(nrow(elapsed)==0){
		return(NA)
	}
	#get times from stdout
	(
		elapsed
		%>% dplyr::select(-order)
		%>% dplyr::mutate(
			message = stringr::str_remove_all(message,'Elapsed Time:')
			, message = stringr::str_remove_all(message,'seconds')
			, message = stringr::str_remove_all(message,' ')
			, message = stringr::str_remove_all(message,"\\s*\\([^\\)]+\\)")
			# , message = stringr::str_replace(message,'\\n',',')
		)
		%>% tidyr::separate(
			col = message
			, into = c('warmup','sampling','total')
			, sep='\n'
			, convert = TRUE
		)
		%>% dplyr::arrange(chain)
	)
}
