#' Monitor sampling and collect results
#'
#' @param var Character string describing the desired name of the variable to save to the global workspace.
#' @param as_job Boolean value, when TRUE (default) launches RStudio Jobs to convey progress information.

#' @return NULL (invisibly); Side effects: if .
#' @export
#'
#' @examples
#' \dontrun{
#' #example for setting seed argument
#' aria::start_sampling(my_data,'my_model.stan')
#' aria::monitor()
#' }
monitor = function(var='aria_out',as_job=TRUE){

	run_info_path = fs::path('aria','runs','run_info',ext='rds')
	#look for run_info
	if(!fs::file_exists(run_info_path)){
		stop('No run info found; did you forget to call aria::start_sampling()?')
	}
	run_info = qs::qread(run_info_path)

	#set out_path (should really have a path-is-writeable check of some sort here)
	run_info$out_path = out_path
	qs::qsave(run_info,run_info_path,preset='fast')

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
			paste(var,"= aria:::start_jobs_and_monitor_()")
			, file = temp_file
		)
		run_info$monitor_job_id = aria:::jobRunScript(
			path = temp_file
			, name = run_info$mod_name
			, workingDir = getwd()
			, exportEnv = 'R_GlobalEnv'
		)
		qs::qsave(run_info,run_info_path,preset='fast')
		return(invisible(NULL))
	}else{
		return(aria:::monitor_())
	}
}

#helper functions not exported ----
start_jobs_and_monitor_ = function(){
	run_info_path = fs::path('aria','runs','run_info',ext='rds')
	run_info = qs::qread(run_info_path)
	for(i_chain in 1:length(run_info$chains)){
		if(is.null(run_info$chains[[i_chain]]$job_id)){
			run_info$chains[[i_chain]]$job_id = aria:::jobAdd(
				name = paste0('└Chain ',run_info$chains[[i_chain]]$id)
				, status = 'Initializing'
				, progressUnits = as.integer(run_info$num_total)
				, actions = list(
					stop = function(id){
						run_info_path = fs::path('aria','runs','run_info',ext='rds')
						run_info = qs::qread(run_info_path)
						for(chain in run_info$chains){
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
			qs::qsave(run_info,run_info_path,preset='fast')
		}
	}
	aria:::monitor_()
}


#' @importFrom magrittr "%>%"
monitor_ = function(){
	run_info_path = fs::path('aria','runs','run_info',ext='qs')
	run_info = qs::qread(run_info_path)

	#init lists of lists
	out = list(
		samples = list()
		, stdout = list()
		, stderr = list()
	)

	#loop until no more pids
	while(length(run_info$chains)){
		#save current state
		qs::qsave(run_info,run_info_path,preset='fast')

		#update meta job
		if(!is.null(run_info$meta_job_id)){
			aria:::jobSetStatus(
				run_info$meta_job_id
				, paste0(
					length(run_info$chains)
					, ' chains running, '
					, run_info$num_chains - length(run_info$chains)
					, ' chains completed'
				)
			)
		}

		#get list of running processes
		all_running_processes = ps::ps_pids()
		for(chain in run_info$chains){
			if(!(chain$pid %in% all_running_processes)){
				#chain is complete, gather
				out$stdout[[as.character(chain$id)]] = read_stan_std(fs::path(chain$path,'stdout.txt'))
				out$stderr[[as.character(chain$id)]] = read_stan_std(fs::path(chain$path,'stderr.txt'))
				out$samples[[as.character(chain$id)]] = read_stan_csv_samples(fs::path(chain$path,'out.csv'))
				fs::dir_delete(chain$path)
				if(!is.null(chain$job_id)){
					aria:::jobAddProgress(chain$job_id,run_info$num_total)
				}
				run_info$chains[[which(names(run_info$chains)==chain$id)]] = NULL
			}
		}
	}
	fs::dir_delete(fs::path('aria','runs'))
	for(i in 1:length(out)){
		out[[i]] = dplyr::bind_rows(out[[i]],.id='chain')
	}
	(
		out$samples
		# %>% add_attr('meta',meta)
		%>% add_attr('stdout',out$stdout)
		%>% add_attr('stderr',out$stderr)
		%>% return()
	)
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
