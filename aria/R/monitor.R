#' Monitor sampling and collect results
#'
#' @param out_path Character string describing the path to which aria will save a ArViz-compliant NetCDF4 file.
#' @param as_job Boolean value, when TRUE (default) launches RStudio Jobs to convey progress information.

#' @return NULL (invisibly); Side effects: Launches a background process (as an RStudio job if run from RStudio and \code{as_job} is TRUE) to monitor the output of sampling
#' @export
#'
#' @examples
#' \dontrun{
#' aria::start_sampling(my_data,'my_model.stan')
#' aria::monitor()
#' #when sampling is complete, retrieve the outputs:
#' sampled = qs::qread('aria/sampled.qs')
#' str(sampled,max=1) #show the list elements
#' }
monitor = function(out_path='aria/sampled.qs',as_job=TRUE){

	sampling_meta_path = fs::path('aria','sampling','sampling_meta',ext='qs')
	#look for sampling_meta
	if(!fs::file_exists(sampling_meta_path)){
		stop('No run info found; did you forget to call aria::start_sampling()?')
	}
	sampling_meta = qs::qread(sampling_meta_path)

	#set out_path (should really have a path-is-writeable check of some sort here)
	sampling_meta$out_path = out_path
	qs::qsave(sampling_meta,sampling_meta_path,preset='fast')

	#check if we're in rstudio and force as_job=F if not
	if(nzchar(system.file(package='rstudioapi'))){
		if(!eval(parse(text='rstudioapi::isAvailable()'))){ # using eval() to avoid inducing dependency
			as_job = FALSE
		}
	}else{
		as_job = FALSE
	}

	#start monitor_
	if(as_job){
		temp_file = tempfile()
		write(
			'aria:::start_jobs_and_monitor_()'
			, file = temp_file
		)
		sampling_meta$monitor_job_id = aria:::jobRunScript(
			path = temp_file
			, name = sampling_meta$mod_name
			, workingDir = getwd()
			, exportEnv = 'R_GlobalEnv'
		)
		qs::qsave(sampling_meta,sampling_meta_path,preset='fast')
		return(invisible(NULL))
	}else{
		return(aria:::monitor_())
	}
}

#helper functions not exported ----
start_jobs_and_monitor_ = function(){
	sampling_meta_path = fs::path('aria','sampling','sampling_meta',ext='qs')
	sampling_meta = qs::qread(sampling_meta_path)
	for(i_chain in 1:length(sampling_meta$chains)){
		if(is.null(sampling_meta$chains[[i_chain]]$job_id)){
			sampling_meta$chains[[i_chain]]$job_id = aria:::jobAdd(
				name = paste0('└Chain ',sampling_meta$chains[[i_chain]]$id)
				, status = 'Initializing'
				, progressUnits = as.integer(sampling_meta$num_total)
				, actions = list(
					stop = function(id){
						sampling_meta_path = fs::path('aria','sampling','sampling_meta',ext='qs')
						sampling_meta = qs::qread(sampling_meta_path)
						for(chain in sampling_meta$chains){
							if(chain$job_id==id){
								ps::ps_interrupt(ps::ps_handle(chain$pid))
							}
						}
						#aria:::jobRemove(id)
						return(invisible(NULL))
					}
					# , info = function(id) { ... }
				)
				, running = TRUE
				, autoRemove = FALSE
				, show = FALSE
			)
			qs::qsave(sampling_meta,sampling_meta_path,preset='fast')
		}
	}
	aria:::monitor_()
}


#' @importFrom magrittr "%>%"
monitor_ = function(){
	sampling_meta_path = fs::path('aria','sampling','sampling_meta',ext='qs')
	sampling_meta = qs::qread(sampling_meta_path)

	#should put a check here if the processes died before writing any output

	#init lists of lists
	out = list(
		samples = list()
		, stdout = list()
		, stderr = list()
	)

	#loop until no more pids
	while(length(sampling_meta$chains)){
		#save current state
		qs::qsave(sampling_meta,sampling_meta_path,preset='fast')

		#update meta job
		if(!is.null(sampling_meta$meta_job_id)){
			aria:::jobSetStatus(
				sampling_meta$meta_job_id
				, paste0(
					length(sampling_meta$chains)
					, ' chains running, '
					, sampling_meta$num_chains - length(sampling_meta$chains)
					, ' chains completed'
				)
			)
		}

		#get list of running processes
		all_running_processes = ps::ps_pids()
		for(chain in sampling_meta$chains){
			if(!(chain$pid %in% all_running_processes)){
				#chain is complete, gather
				out$stdout[[as.character(chain$id)]] = read_stan_std(fs::path(chain$path,'stdout.txt'))
				out$stderr[[as.character(chain$id)]] = read_stan_std(fs::path(chain$path,'stderr.txt'))
				out$samples[[as.character(chain$id)]] = read_stan_csv_samples(fs::path(chain$path,'out.csv'))
				fs::dir_delete(chain$path)
				if(!is.null(chain$job_id)){
					aria:::jobAddProgress(chain$job_id,sampling_meta$num_total)
				}
				sampling_meta$chains[[which(names(sampling_meta$chains)==chain$id)]] = NULL
			}
		}
	}
	fs::dir_delete(fs::path('aria','sampling'))
	for(i in 1:length(out)){
		out[[i]] = dplyr::bind_rows(out[[i]],.id='chain')
	}
	out$meta = sampling_meta
	out$time = times_from_sampled(out)
	qs::qsave(out,file=sampling_meta$out_path,preset='fast')
	beepr::beep()
	return(invisible(NULL))
}


nodep_hack = function(pkg,fn){
	# utils::assignInNamespace(
	# 	fn
	# 	, function(...){stop('[aria] Oops! aria tried to use a function it failed to import. Please send this error message to the maintainer.')}
	# 	, ns = 'aria'
	# 	, envir = as.environment('package:aria')
	# )
	if(nzchar(system.file(package=pkg))){
		utils::assignInNamespace(
			fn
			, utils::getFromNamespace(
				fn
				, ns = pkg
				, envir = as.environment(paste0('package:',pkg))
			)
			, ns = 'aria'
			, envir = as.environment('package:aria')
		)
	}
}

nodep_hack_default = function(...){stop('[aria] Oops! aria tried to use a function it failed to import. Please send this error message to the maintainer.')}

jobRunScript = nodep_hack_default
nodep_hack('rstudioapi','jobRunScript')

jobRemove = nodep_hack_default
nodep_hack('rstudioapi','jobRemove')

jobAdd = nodep_hack_default
nodep_hack('rstudioapi','jobAdd')

jobAddProgress = nodep_hack_default
nodep_hack('rstudioapi','jobAddProgress')


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

read_stan_csv_meta = function(x){
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
