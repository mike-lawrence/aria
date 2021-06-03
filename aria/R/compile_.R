compile_ = function(code_path){

	#get some paths, create aria
	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	debug_exe_file = fs::path('aria','exes',mod_name,'debug')
	fast_exe_file = fs::path('aria','exes',mod_name,'fast')
	debug_json_file = fs::path('aria','exes',mod_name,'debug',ext='json')
	mod_info_file = fs::path('aria','exes',mod_name,'info',ext='qs')
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
		, wd = fs::path_dir(code_path)
		, error_on_status = F
	)$stdout
	if(fs::file_exists(mod_info_file)){
		new_digest = digest::digest(new_txt,algo='xxhash64')
		old_digest = digest::digest(qs::qread(mod_info_file)$mod_txt,algo='xxhash64')
		if((old_digest==new_digest)){
			cat(crayon::blue('  ✓ Compiled exe is up to date.'))
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
	cat(crayon::blue('  Compiling exe for runtime check...\U00D'))
	exe_file_for_compile = fs::path_abs(fs::path_ext_remove(code_path)) #must be absolute
	make_run = processx::run(
		command = cmdstanr:::make_cmd()
		, args = c(
			paste0('-j',parallel::detectCores())
			, exe_file_for_compile
			, paste(
				'STANCFLAGS +='
				, '--include-paths', fs::path_dir(exe_file_for_compile)
				, '--name', mod_name
			)
		)
		, wd = cmdstanr::cmdstan_path()
		, error_on_status = F
		, spinner = T
	)

	if(make_run$stderr!=''){
		if(!getOption('aria_sotto_vocce')){
			beepr::beep(system.file("sounds/critical_stop.wav", package="aria"))
		}
		cat(crayon::blue(make_run$stdout))
		cat('\n\n')
		cat(crayon::red(make_run$stderr))
		cat('\n\n')
		if(sys.parent()==0){ #function is being called from the global env
			return(invisible(NULL))
		}else{
			return(FALSE)
		}
	}
	cat(crayon::blue('  ✓ Compiled runtime-check exe      \n'))

	#move exe & delete the hpp
	fs::file_move(exe_file_for_compile,debug_exe_file)
	fs::file_delete(fs::path_ext_set(code_path,'hpp'))

	#Generate data for runtime check
	debug_data = processx::run(
		command = fs::path(cmdstanr::cmdstan_path(),cmdstanr:::stanc_cmd())
		, args = c(
			code_file
			, '--include-paths','.'
			, '--debug-generate-data'
			, '--name', mod_name
			,'--o', tempfile()
		)
		, wd = fs::path_dir(code_path)
	)$stdout
	write(debug_data,debug_json_file)

	#Perform runtime check
	runtime_check_passed = runtime_check_(debug_exe_file,debug_json_file)
	fs::file_delete(debug_json_file)
	if(!runtime_check_passed){
		if(sys.parent()==0){ #function is being called from the global env
			return(invisible(NULL))
		}else{
			return(FALSE)
		}
	}

	#compile fast exe
	cat(crayon::blue('  Compiling exe for sampling...\U00D'))
	exe_file_for_compile = fs::path_abs(fs::path_ext_remove(code_path)) #must be absolute
	make_run = processx::run(
		command = cmdstanr:::make_cmd()
		, args = c(
			paste0('-j',parallel::detectCores())
			, exe_file_for_compile
			# , 'CXXFLAGS+=-DSTAN_NO_RANGE_CHECKS -O3 -march=native -mtune=native'
			# , 'CXXFLAGS+=-DSTAN_NO_RANGE_CHECKS -DSTAN_CPP_OPTIMS -O3 -march=native -mtune=native'
			, 'STAN_NO_RANGE_CHECKS=true'
			, 'STAN_CPP_OPTIMS=true'
			, paste(
				'STANCFLAGS +='
				, '--include-paths', fs::path_dir(exe_file_for_compile)
				, '--name', mod_name
			)
		)
		, wd = cmdstanr::cmdstan_path()
		, error_on_status = F
		, spinner = T
	)

	if(make_run$stderr!=''){
		if(!getOption('aria_sotto_vocce')){
			beepr::beep(system.file("sounds/critical_stop.wav", package="aria"))
		}
		cat(crayon::blue(make_run$stdout))
		cat('\n\n')
		cat(crayon::red(make_run$stderr))
		cat('\n\n')
		if(sys.parent()==0){ #function is being called from the global env
			return(invisible(NULL))
		}else{
			return(FALSE)
		}
	}
	cat(crayon::blue('  ✓ Compiled sampling exe      \n'))

	#move exe & delete the hpp
	fs::file_move(exe_file_for_compile,fast_exe_file)
	fs::file_delete(fs::path_ext_set(code_path,'hpp'))

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
		, wd = fs::path_dir(code_path)
		, error_on_status = F
	)
	mod_info = jsonlite::fromJSON(info_run$stdout)
	mod_info$cmdstan_version = cmdstanr::cmdstan_version()
	mod_info$mod_txt = new_txt
	qs::qsave(mod_info,mod_info_file,preset='fast')
	if(!getOption('aria_sotto_vocce')){
		beepr::beep(system.file("sounds/tada.wav", package="aria"))
	}

	#finally, return
	if(sys.parent()==0){ #function is being called from the global env
		return(invisible(NULL))
	}else{
		return(TRUE)
	}
}


runtime_check_ = function(debug_exe_file,data_file){
	#Perform runtime check
	cat(crayon::blue('  Checking for runtime errors...\U00D'))
	debug_run = processx::run(
		command = paste0('./',debug_exe_file)
		, args = c(
			'sample'
			, 'num_samples=1'
			, 'num_warmup=0'
			, 'adapt'
			, 'engaged=0'
			, 'algorithm=fixed_param'
			, 'data'
			, paste0('file=',data_file)
			, 'output'
			, paste0('file=',tempfile())
		)
		, error_on_status = F
		, spinner = T
	)
	if(debug_run$stderr!=''){
		if(!getOption('aria_sotto_vocce')){
			beepr::beep(system.file("sounds/critical_stop.wav", package="aria"))
		}
		#                  Checking for runtime errors...
		cat(crayon::red('  Runtime error check FAILED.   \n\n'))
		if(debug_run$stdout!=''){
			cat(crayon::blue('STDOUT:\n'))
			cat(crayon::blue(debug_run$stdout),'\n\n',sep='')
		}
		cat(crayon::blue('STDERR:\n'))
		cat(crayon::red(debug_run$stderr),'\n',sep='')
		return(FALSE)
	}
	#                   Checking for runtime errors...
	cat(crayon::blue('  ✓ Runtime check passed        \n')) #spaces to overwrite old string
	return(TRUE)
}
