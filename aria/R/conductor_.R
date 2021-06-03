#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

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
	# 	chain_num_start
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

	active_chains = list() #this will be ugly AF but is a standin until I learn R6

	#iterate to start the chains
	chain_num_sequence = chain_num_start:(chain_num_start-1+sampling_info$num_chains)
	for(this_chain_num in chain_num_sequence){
		this_chain_dir = fs::path(run_dir,this_chain_num)
		fs::dir_create(this_chain_dir)
		samples_file = fs::path(this_chain_dir,'out.csv')
		stdout_file = fs::path(this_chain_dir,'stdout.txt')
		stderr_file = fs::path(this_chain_dir,'stderr.txt')
		exe_args_list$output$file = samples_file
		this_process = processx::process$new(
			command = paste0('./',fast_exe_file)
			, args = c(
				paste0('id=',this_chain_num)
				, exe_args_list_to_vec(exe_args_list)
			)
			, stdout = stdout_file
			, stderr = stderr_file
			, cleanup = FALSE
		)
		this_chain_info = list()
		this_chain_info$name = as.character(this_chain_num)
		this_chain_info$path = this_chain_dir
		this_chain_info$pid = this_process$get_pid()
		this_chain_info$ps_handle = ps::ps_handle(this_chain_info$pid)
		this_chain_info$alive = TRUE
		this_chain_info$iter_done = 0
		this_chain_info$adapt_info = list(step_size=NULL,mass_matrix=NULL)
		this_chain_info$output = list(
			'samples' = list(
				name = 'samples'
				, file = samples_file
				, parser = samples_csv_to_tbl
				, parsed = tibble::tibble()
				, old_extra = NULL
				, header = NULL
				, skip = 0
				, last_file_size = 0
			)
			, 'stdout' = list(
				name = 'stdout'
				, file = stdout_file
				, parser = std_txt_to_tbl
				, parsed = NULL
				, old_extra = NULL
				, skip = 0
				, last_file_size = 0
				, parse_on_next_pass = FALSE

			)
			, 'stderr' = list(
				name = 'stderr'
				, file = stdout_file
				, parser = std_txt_to_tbl
				, parsed = NULL
				, old_extra = NULL
				, skip = 0
				, last_file_size = 0
				, parse_on_next_pass = FALSE
			)
		)
		active_chains[[this_chain_info$name]] = this_chain_info
	}
	cat(crayon::cyan('Started sampling for chains',min(chain_num_sequence),'through',max(chain_num_sequence),'\n'))
	inactive_chains = list()

	sample_csv_col_names = strsplit(
		system2(
			command = "grep"
			, args = c(
				"'^lp'"
				, "--color=never"
				, active_chains[[1]]$output[['samples']]$file
			)
			, stdout = TRUE
		)
		, ','
	)[[1]]


	#loop until no more active chains
	while(length(active_chains)){
		# #save current state
		# qs::qsave(active_chains,'active.qs',preset='fast')
		# qs::qsave(inactive_chains,'inactive.qs',preset='fast')

		#update job status
		rstudioapi::jobSetStatus(
			sampling_info$job_id
			, paste0(
				length(active_chains)
				, ' chains running, '
				, length(inactive_chains)
				, ' chains completed'
			)
		)

		#loop over the active chains
		for(this_chain_name in names(active_chains)){
			#mark as dead if not active
			if(!ps::ps_is_running(active_chains[[this_chain_name]]$ps_handle)){
				active_chains[[this_chain_name]]$alive = FALSE
			}
			#loop over outputs
			for(this_output_name in names(active_chains[[this_chain_name]]$output)){
				last_file_size = active_chains[[this_chain_name]]$output[[this_output_name]]$last_file_size
				new_file_size = file.size(
					active_chains[[this_chain_name]]$output[[this_output_name]]$file
				)
				do_parse = FALSE
				if( this_output_name=='samples'){
					if(last_file_size>new_file_size){
						do_parse = TRUE
					}
				}else{ #std
					#for std's, we wait for a quiet period
					if(last_file_size>new_file_size){
						active_chains[[this_chain_name]]$output[[this_output_name]]$parse_on_next_pass = TRUE
					}else{
						if(active_chains[[this_chain_name]]$output[[this_output_name]]$parse_on_next_pass){
							do_parse = TRUE
							active_chains[[this_chain_name]]$output[[this_output_name]]$parse_on_next_pass = FALSE
						}
					}
				}
				# parse as necessary
				if(do_parse){
					# pause so filesize doesn't change while we're reading
					if(active_chains[[this_chain_name]]$alive){
						ps::ps_suspend(active_chains[[this_chain_name]]$ps_handle)
					}
					#assign file size to entry in active chains
					active_chains[[this_chain_name]]$output[[this_output_name]]$last_file_size = file.size(
						active_chains[[this_chain_name]]$output[[this_output_name]]$file
					)
					#parse
					active_chains[[this_chain_name]]$output[[this_output_name]]$parsed %<>%
						dplyr::bind_rows(
							active_chains[[this_chain_name]]$output[[this_output_name]]$parser(
								file = active_chains[[this_chain_name]]$output[[this_output_name]]$file
								, skip = active_chains[[this_chain_name]]$output[[this_output_name]]$skip
								, sample_csv_col_names = sample_csv_col_names
								, chain_name = this_chain_name
								, num_warmup = sampling_info$num_warmup
							)
						)
					# update skip
					active_chains[[this_chain_name]]$output[[this_output_name]]$skip = nrow(
						active_chains[[this_chain_name]]$output[[this_output_name]]$parsed
					)
					#resume the chain if it's not already dead
					if(active_chains[[this_chain_name]]$alive){
						ps::ps_resume(active_chains[[this_chain_name]]$ps_handle)
					}
				}
			}
		}
		#loop again to collect newly-inactive chains and remove them from the active list
		for(this_chain_name in names(active_chains)){
			if(!active_chains[[this_chain_name]]$alive){
				#loop over outputs to parse one last time
				for(this_output_name in names(active_chains[[this_chain_name]]$output)){
					#parse
					active_chains[[this_chain_name]]$output[[this_output_name]] = (
						dplyr::bind_rows(
							active_chains[[this_chain_name]]$output[[this_output_name]]$parsed
							, active_chains[[this_chain_name]]$output[[this_output_name]]$parser(
								file = active_chains[[this_chain_name]]$output[[this_output_name]]$file
								, skip = active_chains[[this_chain_name]]$output[[this_output_name]]$skip
								, sample_csv_col_names = sample_csv_col_names
								, chain_name = this_chain_name
								, num_warmup = sampling_info$num_warmup
							)
						)
					)

				}
				#clean up chain info
				# if we have adapt info, make it a matrix
				# if(nrow(chain$adapt_info$mass_matrix)==1){
				# 	chain$adapt_info$mass_matrix = diag(chain$adapt_info$mass_matrix)
				# }
				inactive_chains[[this_chain_name]] = active_chains[[this_chain_name]]$output
				inactive_chains[[this_chain_name]]$output$adapt_info = active_chains[[this_chain_name]]$adapt_info
				active_chains[[this_chain_name]] = NULL
			}
		}
	}
	#no more active chains, so clean up
	fs::dir_delete(fs::path('aria','sampling'))
	(
		inactive_chains
		%>% purrr::map_dfr(list('samples'))
		%>% dplyr::arrange(chain,iteration)
	) -> out
	attr(out,'stdout') = purrr::map_dfr(inactive_chains,list('stdout'))
	attr(out,'stderr') = purrr::map_dfr(inactive_chains,list('stderr'))
	attr(out,'adapt_info') = purrr::map(inactive_chains,list('adapt_info'))
	attr(out,'sampling_info') = sampling_info
	# out$info = sampling_info
	# out$time = times_from_sampled(out)
	qs::qsave(
		out
		, file = fs::path(sampling_info$out_path)
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
