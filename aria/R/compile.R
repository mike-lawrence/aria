compile = function(aria_args){

	#get some paths, create aria
	code_file = fs::path_file(aria_args$code_path)
	mod_name = fs::path_ext_remove(code_file)
	debug_exe_file = fs::path('aria','exes',mod_name,'stan_debug_exe')
	fast_exe_file = fs::path('aria','exes',mod_name,'stan_exe')
	debug_json_file = fs::path('aria','exes',mod_name,'debug',ext='json')
	mod_info_file = fs::path('aria','exes',mod_name,'info',ext='rds')
	fs::dir_create('aria','exes',mod_name)

	#run autoformater so we only recompile on functional changes
	new_txt = processx::run(
		command = fs::path(cmdstanr::cmdstan_path(),cmdstanr:::stanc_cmd())
		, args = c(
			code_file
			, '--include-paths','.'
			, '--auto-format'
			, '--name', mod_name
			,'--o', tempfile()
		)
		, wd = fs::path_dir(aria_args$code_path)
		, error_on_status = F
	)$stdout
	if(fs::file_exists(mod_info_file)){
		new_digest = digest::digest(new_txt,algo='xxhash64')
		old_digest = digest::digest(readRDS(mod_info_file)$mod_txt,algo='xxhash64')
		if((old_digest==new_digest)){
			cat(aria:::blue('  ✓ Compiled exe is up to date.'))
			if(!getOption('aria_sotto_vocce')){
				beepr::beep(system.file("sounds/tada.wav", package="aria"))
			}
			if(sys.parent()==0){ #function is being called from the global env
				return(invisible(NULL))
			}else{
				return(TRUE)
			}
		}
	}

	#compile debug exe
	if(is.null(aria_args$compile_debug)){
		aria_args$compile_debug = TRUE
	}
	if(aria_args$compile_debug){
		cat(aria:::blue('  Compiling debugging exe...\U00D'))
		tmpfile = fs::file_copy(
			aria_args$code_path
			, new_path = fs::file_temp(ext='stan')
		)
		exe_file_for_compile = fs::path_ext_remove(tmpfile)
		debug_make_run = processx::run(
			command = cmdstanr:::make_cmd()
			, args = c(
				paste0('-j',parallel::detectCores())
				, exe_file_for_compile
				# , stringr::str_replace(exe_file_for_compile,stringr::fixed(' '),stringr::fixed('\\ '))
				, paste(
					'STANCFLAGS +='
					, '--include-paths', dQuote(fs::path_abs(fs::path_dir(aria_args$code_path)),F)
					, '--name', mod_name
				)
			)
			, wd = cmdstanr::cmdstan_path()
			, error_on_status = F
			, spinner = T
		)

		if(debug_make_run$stderr!=''){
			if(!getOption('aria_sotto_vocce')){
				beepr::beep(system.file("sounds/critical_stop.wav", package="aria"))
			}
			cat(aria:::blue(debug_make_run$stdout))
			cat('\n\n')
			cat(aria:::red(debug_make_run$stderr))
			cat('\n\n')
			if(sys.parent()==0){ #function is being called from the global env
				return(invisible(NULL))
			}else{
				return(FALSE)
			}
		}
		#move exe & delete the hpp
		fs::file_move(exe_file_for_compile,debug_exe_file)
		fs::file_delete(fs::path_ext_set(exe_file_for_compile,'hpp'))
		# show success
		cat(aria:::blue('  ✓ Compiled debugging exe  \n'))
	}


	if(is.null(aria_args$run_debug)){
		aria_args$run_debug = TRUE
	}
	if(aria_args$compile_debug & aria_args$run_debug){
		#Generate data for debug
		debug_data = processx::run(
			command = fs::path(cmdstanr::cmdstan_path(),cmdstanr:::stanc_cmd())
			, args = c(
				code_file
				, '--include-paths','.'
				, '--debug-generate-data'
				, '--name', mod_name
				,'--o', tempfile()
			)
			, wd = fs::path_dir(aria_args$code_path)
		)$stdout
		write(debug_data,debug_json_file)

		#Perform runtime check
		runtime_check_passed = aria:::run_debug(debug_exe_file,debug_json_file)
		fs::file_delete(debug_json_file)
		if(!runtime_check_passed){
			if(sys.parent()==0){ #function is being called from the global env
				return(invisible(NULL))
			}else{
				return(FALSE)
			}
		}
	}

	#compile fast exe
	cat(aria:::blue('  Compiling performance exe ...\U00D'))
	tmpfile = fs::file_copy(
		aria_args$code_path
		, new_path = fs::file_temp(ext='stan')
	)
	exe_file_for_compile = fs::path_ext_remove(tmpfile)
	make_local_path = fs::path(cmdstanr::cmdstan_path(),'make','local')
	if(fs::file_exists(make_local_path)){
		found_make_local = TRUE
		fs::file_move(make_local_path,fs::path(make_local_path,ext='orig'))
	}else{
		found_make_local = FALSE
	}
	if(is.null(aria_args$cxx_flags)){
		cxx_flags = NULL
	}else{
		cxx_flags = paste('CXXFLAGS +=',aria_args$cxx_flags)
	}
	writeLines(
		text = c(
			cxx_flags
			, 'CXXFLAGS += -O3'
			, 'CXXFLAGS += -g0'
			, 'STAN_NO_RANGE_CHECKS=true'
			, 'STAN_CPP_OPTIMS=true'
		)
		, make_local_path
	)
	make_run = processx::run(
		command = cmdstanr:::make_cmd()
		, args = c(
			paste0('-j',parallel::detectCores())
			, exe_file_for_compile
			, paste(
				'STANCFLAGS +='
				, '--include-paths', dQuote(fs::path_abs(fs::path_dir(aria_args$code_path)),F)
				, '--name', mod_name
			)
		)
		, wd = cmdstanr::cmdstan_path()
		, error_on_status = F
		, spinner = T
	)
	if(found_make_local){
		fs::file_move(fs::path(make_local_path,ext='orig'),make_local_path)
	}
	if(make_run$stderr!=''){
		if(!getOption('aria_sotto_vocce')){
			beepr::beep(system.file("sounds/critical_stop.wav", package="aria"))
		}
		cat(aria:::blue(make_run$stdout))
		cat('\n\n')
		cat(aria:::red(make_run$stderr))
		cat('\n\n')
		if(sys.parent()==0){ #function is being called from the global env
			return(invisible(NULL))
		}else{
			return(FALSE)
		}
	}
	cat(aria:::blue('  ✓ Compiled performance exe   \n'))

	#move exe & delete the hpp
	fs::file_move(exe_file_for_compile,fast_exe_file)
	fs::file_delete(fs::path_ext_set(exe_file_for_compile,'hpp'))

	#get model info
	info_run = processx::run(
		command = fs::path(cmdstanr::cmdstan_path(),cmdstanr:::stanc_cmd())
		, args = c(
			code_file
			, '--include-paths','.'
			, '--name', mod_name
			, '--info'
			,'--o', tempfile()
		)
		, wd = fs::path_dir(aria_args$code_path)
		, error_on_status = F
	)
	mod_info = jsonlite::fromJSON(info_run$stdout)
	mod_info$cmdstan_version = cmdstanr::cmdstan_version()
	mod_info$mod_txt = new_txt
	saveRDS(mod_info,mod_info_file)
	if(!getOption('aria_sotto_vocce')){
		beepr::beep(system.file("sounds/tada.wav", package="aria"))
	}

	#finally, return
	return(invisible(NULL))
}
