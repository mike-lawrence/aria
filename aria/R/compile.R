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
	if(!fs::file_exists(mod_info_file)){
		maybe_do_debug = TRUE
	}else{
		txt_unchanged = aria:::digests_equal(new_txt,readRDS(mod_info_file)$mod_txt)
		make_local_unchanged = aria:::digests_equal(aria_args$make_local,readRDS(mod_info_file)$make_local)
		if(txt_unchanged & make_local_unchanged){
			cat(aria:::blue('  ✓ Compiled exe is up to date.'))
			aria:::yay()
			if(sys.parent()==0){ #function is being called from the global env
				return(invisible(NULL))
			}else{
				return(TRUE)
			}
		}else{
			if(txt_unchanged){
				maybe_do_debug = FALSE
			}
		}
	}

	if(maybe_do_debug){
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
				aria:::compile_fail_notify(debug_make_run,'debug')
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
			#fs::file_delete(debug_json_file)
			if(!runtime_check_passed){
				if(sys.parent()==0){ #function is being called from the global env
					return(invisible(NULL))
				}else{
					return(FALSE)
				}
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
	#write to make/local if necessary
	if(!is.null(aria_args$make_local)){
		make_local_path = fs::path(cmdstanr::cmdstan_path(),'make','local')
		#backup any existing make/local
		if(fs::file_exists(make_local_path)){
			found_make_local = TRUE
			fs::file_move(make_local_path,fs::path(make_local_path,ext='orig'))
		}else{
			found_make_local = FALSE
		}
		#write new make/local
		write(
			paste(aria_args$make_local,collapse='\n')
			, make_local_path
		)
	}
	# launch the make
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
	#restore original make/local (if present and if replaced)
	if(!is.null(aria_args$make_local)){
		if(found_make_local){
			fs::file_move(fs::path(make_local_path,ext='orig'),make_local_path)
		}else{
			fs::file_delete(make_local_path)
		}
	}
	if(make_run$stderr!=''){
		aria:::compile_fail_notify(make_run,'performance')
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
	mod_info$make_local = aria_args$make_local
	saveRDS(mod_info,mod_info_file)
	aria:::yay()
	#finally, return
	return(invisible(NULL))
}
