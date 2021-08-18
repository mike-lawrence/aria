#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

conductor = function(){
	#prelude ----
	sampling_info_file = fs::path('aria','sampling','info',ext='rds')
	while(!fs::file_exists(sampling_info_file)){
		Sys.sleep(.001)
	}
	sampling_info = readRDS(sampling_info_file)
	# stop()
	options('aria_sotto_vocce'=sampling_info$aria_sotto_vocce)
	debug_exe_file = fs::path('aria','exes',sampling_info$mod_name,'stan_debug_exe')
	fast_exe_file = fs::path('aria','exes',sampling_info$mod_name,'stan_exe')
	mod_info_file = fs::path('aria','exes',sampling_info$mod_name,'info',ext='rds')

	# do debug run ----
	#look for exes
	if(!fs::file_exists(fast_exe_file)){
		stop(aria:::red('Compiled exe not found, run `aria:::compile("',sampling_info$code_path,'")` first.'))
	}else{
		if(!fs::file_exists(debug_exe_file)){
			cat(aria:::cyan('No debug exe found, using performance exe for debug check. Line numbers in any resulting errors may not match model file.\n'))
			debug_run_file = fast_exe_file
		}else{
			debug_run_file = debug_exe_file
		}
	}
	#run the debug, obtaining samples_col_names
	debug_out = aria:::run_debug(debug_run_file,sampling_info$data_file,return_header=T)
	if(length(debug_out)==1){
		return(invisible(NULL))
	}else{
		sampling_info$samples_col_names = c(
			debug_out[1:2]
			#add some that are omitted in the debug mode
			, 'stepsize__','treedepth__','n_leapfrog__','divergent__','energy__'
			, debug_out[3:length(debug_out)]
		)
		sampling_info$samples_col_classes = list(
			integer = 4:6
			, numeric = c(1:3,7:length(sampling_info$samples_col_names))
		)

	}

	# set up exe_args & other entries in sampling_info ----
	# exe_args:
	if(is.null(sampling_info$exe_args_list)){
		sampling_info$exe_args_list = list('sample'=list())
	}
	(
		sampling_info$exe_args_list
		#TODO: add_run_arg_if_missing doesn't support delving more than 1 layer deep
		%>% aria:::add_run_arg_if_missing('sample','save_warmup',1)
		%>% aria:::add_run_arg_if_missing('data','file',sampling_info$data_file)
		%>% aria:::add_run_arg_if_missing('output','refresh',0)
		%>% aria:::add_run_arg_if_missing('output','sig_figs',18)
		%>% aria:::add_run_arg_if_missing('sample','save_warmup',1)
		# %>% add_run_arg_if_missing('output','diagnostic_file','diagnostic.csv')
	) -> sampling_info$exe_args_list
	#others:
	sampling_info$num_chains %<>% ifelse( !is.null(.) , . , parallel::detectCores()/2 )
	sampling_info$num_chains %<>% ifelse( .>0 , . , parallel::detectCores()/2 - . ) #negative user-supplied
	sampling_info$num_warmup %<>% ifelse( !is.null(.) , . , 1e3 )
	sampling_info$num_samples %<>% ifelse( !is.null(.) , . , 1e3 )
	sampling_info$num_total = sampling_info$num_samples+sampling_info$num_warmup
	sampling_info$start_time = Sys.time()
	sampling_info$mod_info = readRDS(mod_info_file)

	#initialize the nc4 file ----
	sampling_info %<>% aria:::initialize_nc()

	#Start the chains ----
	active_chains = list()
	chain_name_sequence = as.character(
		sampling_info$chain_num_start:(sampling_info$chain_num_start-1+sampling_info$num_chains)
	)
	for(this_chain_name in chain_name_sequence){

		active_chains[[this_chain_name]] = aria:::class_chain$new(
			name = this_chain_name
			, sampling_info = sampling_info
		)
	}
	inactive_chains = list()

	#set niceness so we don't compete with the sampling processes
	#    we'll also sleep in the main loop
	#ps::ps_set_nice(value=as.integer(20))


	# main monitoring loop ----
	spinner_chars = c('|','/','－','\\')
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
			if(!this_chain_still_active){
				active_chains[[this_chain_name]]$finalize()
				inactive_chains[[this_chain_name]] = active_chains[[this_chain_name]]
				active_chains[[this_chain_name]] = NULL
			}
		}
		#Update progress ----

		#first the top bar:
		# aria:::cat_top_bar(sampling_info)
		aria:::cat_top_bar(sampling_info)

		#loop over chains in sequence to cat their progress:
		nlines = list()
		for(this_chain_name in chain_name_sequence){
			if(this_chain_name %in% names(active_chains)){
				active_chains[[this_chain_name]]$cat_progress()
				nlines[[this_chain_name]] = active_chains[[this_chain_name]]$samples$parsed_nlines
			}else{
				inactive_chains[[this_chain_name]]$cat_progress()
				nlines[[this_chain_name]] = inactive_chains[[this_chain_name]]$samples$parsed_nlines
			}
		}
		#update job status
		if(!is.null(sampling_info$job_id)){
			rstudioapi::jobSetStatus(
				sampling_info$job_id
				, paste0(
					round(min(unlist(nlines))/sampling_info$num_total*100)
					, '%'
				)
			)
		}
		diag_start_sample = 1
		diag_sample_count = min(unlist(nlines))
		if(diag_sample_count>sampling_info$num_warmup){
			diag_start_sample = sampling_info$num_warmup + 1
			diag_sample_count = diag_sample_count - sampling_info$num_warmup
		}

		if(diag_sample_count>2){
			(
				RNetCDF::var.get.nc(
					ncfile = sampling_info$nc_groups$sample_stats
					, variable = 'lp'
					, start = c(1,1)
					, count = c(diag_sample_count,sampling_info$num_chains)
				)
				%>% posterior::rhat()
			) -> rhat_lp
			cat(' \nr\U0302:',format(round(1.00,2),nsmall=2),'\n \n')
			(
				RNetCDF::var.get.nc(
					ncfile = sampling_info$nc_groups$sample_stats
					, variable = 'treedepth'
					, start = c(1,1)
					, count = c(diag_sample_count,sampling_info$num_chains)
				)
				%>% {function(x){
					dimnames(x) = list()
					dimnames(x)[[1]] = paste('V',1:nrow(x),sep='')
					dimnames(x)[[2]] = chain_name_sequence
					return(x)
				}}()
				%>% tibble::as_tibble(rownames = 'draw')
				%>% tidyr::pivot_longer(-draw,names_to='chain')
				%>% dplyr::group_by(chain,value)
				%>% dplyr::summarise(count=dplyr::n(),.groups='drop_last')
				%>% dplyr::mutate(
					count = round(count/sum(count)*100)
					, value = factor(value,levels=c(min(value):max(value)))
				)
				%>% dplyr::arrange(value)
				%>% tidyr::pivot_wider(
					names_from = value
					, values_from = count
					, values_fill = 0
				)
				%>% as.data.frame()
				# %>% {function(x){names(x)=}}
			) -> treedepth
			cat('Treedepth %\n')
			dimnames(treedepth)[[1]] = strrep(' ',1:nrow(treedepth))
			dimnames(treedepth)[[2]][1] = ' '
			print(treedepth)
			cat(' \n')
		}
		#
		loop_duration_txt = stringr::str_trim(aria:::vague_dt(Sys.time() - last_loop_start_time))
		this_spinner_char = spinner_chars[1]
		cat(
			'Composer loop duration: '
			, loop_duration_txt
			, ' '
			, spinner_chars[1]
			, '\n'
			, sep = ''
		)
		spinner_chars = c(spinner_chars[2:length(spinner_chars)],this_spinner_char)
		#gather stderr messages & cat
		(
			dplyr::bind_rows(
				purrr::map_dfr(active_chains,list('stderr','parsed'))
				, purrr::map_dfr(inactive_chains,list('stderr','parsed'))
			)
			%>% aria:::cat_stderr()
		)
	}
	RNetCDF::close.nc(sampling_info$nc)
	#no more active chains, cleanup:
	# ps::ps_set_nice(value=as.integer(0))
	marginalia = list(
		time = purrr::map_dfr(inactive_chains,list('times_from_stdout'))
		, stdout = purrr::map_dfr(inactive_chains,list('stdout','parsed'))
		, stderr =  purrr::map_dfr(inactive_chains,list('stderr','parsed'))
		# , nc = sampling_info$nc
	)
	saveRDS(marginalia,fs::path('aria','marginalia',ext='rds'))
	if(!getOption('aria_sotto_vocce')){
		beepr::beep(system.file("sounds/tada.wav", package="aria"))
	}
	# cat(
	# 	aria:::cyan(' \n \n \nComposition complete.\n')
	# 	, aria:::cyan('Results have been put in a variable named ')
	# 	, aria:::blue('aria_out')
	# 	, aria:::cyan(' in the global environment.')
	# 	, sep = ''
	# )
	cat(
		aria:::cyan(' \n \n \nComposition complete.')
		, aria:::cyan('\nAccess the results via:')
		, aria:::blue(
			paste0(
				'aria::coda("'
				, fs::path(sampling_info$out_path)
				,'")'
			)
		)
		, aria:::cyan('\nAccess marginalia (timing, stdout, stderr) via:')
		, aria:::blue('aria::marginalia()')
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

cat_top_bar = function(sampling_info){
	bar_prefix_width = nchar('0:[')
	bar_suffix_width = nchar(']100 99m? dvrg 1/bfmi essB essT')
	bar_width = floor(.8*(getOption('width') - (bar_prefix_width+bar_suffix_width)))
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
		, strrep(' ', bar_prefix_width)
		, strrep('\U2592',warmup_width)
		, strrep('\U2593',sampling_width)
		# ']100 99m? dvrg 1/bfmi bulk tail'
		, '   %  eta dvrg 1/bfmi essB essT\n'
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
