#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

conductor_ = function(){
	Sys.sleep(1) #to give sample() time to save sampling_info

	sampling_info_file = fs::path('aria','sampling','info',ext='qs')
	sampling_info = qs::qread(sampling_info_file)
	attach(sampling_info)
	# sampling_info has:
	#   aria_sotto_vocce
	# 	mod_name
	# 	code_path
	#	num_chains
	# 	chain_num_start
	# 	exe_args_list
	# 	data_file
	#	job_id
	options('aria_sotto_vocce'=aria_sotto_vocce)

	debug_exe_file = fs::path('aria','exes',mod_name,'stan_debug_exe')
	fast_exe_file = fs::path('aria','exes',mod_name,'stan_exe')
	mod_info_file = fs::path('aria','exes',mod_name,'info',ext='qs')

	#look for exe
	if(!fs::file_exists(fast_exe_file)){
		stop(aria:::red('Compiled exe not found, run `aria:::compile("',code_path,'")` first.'))
	}else{
		if(!fs::file_exists(debug_exe_file)){
			cat(aria:::cyan('No debug exe found, using performance exe for debug check. Line numbers in any resulting errors may not match model file.'))
			debug_run_file = fast_exe_file
		}else{
			debug_run_file = debug_exe_file
		}
	}

	debug_out = aria:::run_debug(debug_run_file,data_file,return_header=T)
	if(length(debug_out)==1){
		return(invisible(NULL))
	}else{
		sampling_info$samples_header = c(
			debug_out[1:2]
			, 'stepsize__','treedepth__','n_leapfrog__','divergent__','energy__'
			, debug_out[3:length(debug_out)]
		)
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
	exe_args_list = aria:::add_run_arg_if_missing(exe_args_list,'sample','save_warmup',1)
	exe_args_list = aria:::add_run_arg_if_missing(exe_args_list,'data','file',data_file)
	exe_args_list = aria:::add_run_arg_if_missing(exe_args_list,'random','seed',base::sample(.Machine$integer.max, 1))
	exe_args_list = aria:::add_run_arg_if_missing(exe_args_list,'output','refresh',0)
	exe_args_list = aria:::add_run_arg_if_missing(exe_args_list,'output','sig_figs',18)
	# exe_args_list = add_run_arg_if_missing(exe_args_list,'output','diagnostic_file','diagnostic.csv')
	sampling_info$exe_args_list = exe_args_list

	sampling_info$mod_info = qs::qread(mod_info_file)

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

	active_chains = list()

	#iterate to start the chains
	chain_name_sequence = as.character(
		chain_num_start:(chain_num_start-1+sampling_info$num_chains)
	)
	for(this_chain_name in chain_name_sequence){
		active_chains[[this_chain_name]] = aria:::class_chain$new(
			name = this_chain_name
			, sampling_info = sampling_info
		)
		# ps::ps_set_nice(
		# 	p = ps::ps_handle(active_chains[[this_chain_name]]$process$get_pid())
		# 	,value=as.integer(20)
		# )
	}
	inactive_chains = list()

	#set niceness so we don't compete with the sampling processes
	#    we'll also sleep in the main loop
	ps::ps_set_nice(value=as.integer(20))

	last_loop_start_time = Sys.time()
	#loop until no more active chains
	while(length(active_chains)){
		#handle sleeping:
		last_loop_duration = Sys.time()-last_loop_start_time
		if(last_loop_duration<1){
			Sys.sleep(1-last_loop_duration)
		}
		last_loop_start_time = Sys.time()
		#loop over the active chains
		for(this_chain_name in names(active_chains)){
			# store the pre-ingesting state
			this_chain_still_active = active_chains[[this_chain_name]]$process$is_alive()
			# ingest, forcing if no longer active
			active_chains[[this_chain_name]]$ingest(force=!this_chain_still_active)
			if(nrow(active_chains[[this_chain_name]]$samples$parsed)==sampling_info$num_total){
				this_chain_still_active = FALSE
			}
			if(!this_chain_still_active){
				active_chains[[this_chain_name]]$finalize()
				inactive_chains[[this_chain_name]] = active_chains[[this_chain_name]]
				active_chains[[this_chain_name]] = NULL
			}
		}
		#Update progress ----
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
		# job output:
		#first the top bar:
		# aria:::cat_top_bar(sampling_info)
		aria:::cat_top_bar(sampling_info,last_loop_start_time)

		#loop over chains in sequence to cat their progress:
		for(this_chain_name in chain_name_sequence){
			if(this_chain_name %in% names(active_chains)){
				active_chains[[this_chain_name]]$cat_progress()
			}else{
				inactive_chains[[this_chain_name]]$cat_progress()
			}
		}
		#gather stderr messages & cat
		(
			dplyr::bind_rows(
				purrr::map_dfr(active_chains,list('stderr','parsed'))
				, purrr::map_dfr(inactive_chains,list('stderr','parsed'))
			)
			%>% aria:::cat_stderr()
		)
	}
	#no more active chains, cleanup:
	# ps::ps_set_nice(value=as.integer(0))
	(
		inactive_chains
		%>% purrr::map_dfr(list('samples','parsed'))
		%>% dplyr::arrange(chain,iteration)
	) -> out
	attr(out,'adapt_info') = purrr::map(inactive_chains,list('samples','adapt_info'))
	attr(out,'stdout') = purrr::map_dfr(inactive_chains,list('stdout','parsed'))
	attr(out,'stderr') = purrr::map_dfr(inactive_chains,list('stderr','parsed'))
	attr(out,'time') = purrr::map_dfr(inactive_chains,list('times_from_stdout'))
	qs::qsave(
		out
		, file = fs::path(sampling_info$out_path)
		, preset = 'fast'
	)
	if(!getOption('aria_sotto_vocce')){
		beepr::beep(system.file("sounds/tada.wav", package="aria"))
	}
	cat(
		aria:::cyan(' \n \n \nComposition complete.')
		, aria:::cyan('Access the results via:')
		, aria:::blue(
			paste0(
				'aria::coda("'
				, fs::path(sampling_info$out_path)
				,'")'
			)
		)
		, sep = '\n'
	)
	rm(inactive_chains)
	fs::dir_delete(fs::path('aria','sampling'))
	return(invisible(NULL))
}

#helper functions not exported ----

#recursive function
exe_args_list_to_vec = function(x,name=NULL){
	if(!is.list(x)){ # at end of branch
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


# convert seconds to pretty text (cribbed from https://github.com/r-lib/prettyunits)
vague_dt = function(seconds) {
	`%s%` = function(lhs, rhs) {
		do.call(
			sprintf,
			c(list(lhs), as.list(rhs))
		)
	}
	vague_dt_terse = list(
		list(c = expression(seconds < 50), s = expression("%2ds" %s% round(seconds))),
		list(c = expression(minutes < 50), s = expression("%2dm" %s% round(minutes))),
		list(c = expression(hours < 18),   s = expression("%2dh" %s% round(hours))),
		list(c = expression(days < 30),    s = expression("%2dd" %s% round(days))),
		list(c = expression(days < 335),   s = expression("%2dM" %s% round(days / 30))),
		list(c = TRUE,                     s = expression("%2dy" %s% round(years)))
	)
	pieces = list(
		minutes = seconds / 60,
		hours = seconds / 60 / 60,
		days = seconds / 60 / 60 / 24,
		years = seconds / 60 / 60 / 24 / 365.25
	)
	for (p in vague_dt_terse) {
		if (eval(p$c, pieces)) return(eval(p$s, pieces))
	}
}

cat_top_bar = function(sampling_info,last_loop_start_time){
	loop_duration_txt = stringr::str_trim(aria:::vague_dt(Sys.time() - last_loop_start_time))
	bar_prefix_width = nchar('0: 100% [')
	bar_suffix_width = nchar('] 99m?')
	bar_width = getOption('width') - (bar_prefix_width+bar_suffix_width)
	warmup_width = round(
		sampling_info$num_warmup
		/ (
			sampling_info$num_warmup
			+ sampling_info$num_samples
		)
		* bar_width
	)
	sampling_width = bar_width - warmup_width
	cat(
		'\U00C' #next-page character (serves to clear and place cursor at [0,0] )
		, loop_duration_txt
		, strrep(' ', bar_prefix_width-nchar(loop_duration_txt))
		, strrep('\U2592',warmup_width)
		, strrep('\U2593',sampling_width)
		, strrep(' ', bar_suffix_width)
		, '\n'
		, sep=''
	)

}
cat_stderr = function(stderr){
	if(nrow(stderr)>0){
		stderr = dplyr::filter(
			stderr
			, !stringr::str_starts(message,'Informational Message')
		)
		if(nrow(stderr)==0){
			return(invisible(NULL))
		}
		cat( ' \n'
			, strrep('!',getOption('width'))
			, '\nSTDERR:\n'
			, sep=''
		)
		(
			stderr
			%>% dplyr::mutate(
				message = stringr::str_replace_all(message,'\n',' ')
				, message = stringr::str_replace_all(message,'  ',' ')
				, message = stringr::str_replace_all(message,': ',':\n')
				, message = stringr::str_trim(message)
			)
			%>% dplyr::group_by(message)
			%>% dplyr::summarise(
				which_chains = list(unique(chain))
				, .groups = 'drop'
			)
		) -> unique_stderr
		for(row in 1:nrow(unique_stderr)){
			cat(
				' \n \n[chains:'
				,paste(unique_stderr$which_chains[row],collapse=', ')
				,']\n'
			)
			cat(unique_stderr$message[row])
		}
	}
	return(invisible(NULL))
}
