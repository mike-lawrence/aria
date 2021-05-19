#' Compile Stan code and check for runtime errors
#'
#' This function first checks if th Stan code has changed in functional ways (i.e. not comments nor whitespace), compiles when such changes exist then runs a simple test for runtime errors.
#'
#' @param code_path Character string describing the path to the Stan code.
#'
#' @return NULL (invisibly); Side effects: an eponymous executable binary is placed in a folder called `aria` (along with some other helper files).
#' @export
#'
#' @family Model checking & compilation functions
#'
#' @examples
#' \dontrun{
#' compile('my_model.stan')
#' }
compile = function(code_path){

	#get some paths, create aria
	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	exe_path = fs::path('aria','exes',mod_name,fs::path_ext_remove(code_file))
	dbg_path = fs::path_ext_set(paste0(exe_path,'_debug'),ext='json')
	qs_path = fs::path_ext_set(paste0(exe_path),ext='qs')
	fs::dir_create('aria','exes',mod_name)

	#If not being called by another function, do syntax check first
	if(sys.parent()==0){ #function is being called from the global env
		if(!check_syntax(code_path)){ #check_syntax returns TRUE if passed
			return(invisible(NULL))
		}
	}

	#run again for autoformat output so we only recompile on functional changes
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
	if(fs::file_exists(exe_path)){
		if(fs::file_exists(qs_path)){
			new_digest = digest::digest(new_text,algo='xxhash64')
			old_digest = digest::digest(qs::qread(qs_path)$mod_txt,algo='xxhash64')
			if((old_digest==new_digest)){
				cat(crayon::blue('  ✓ Compiled exe is up to date.'))
				if(sys.parent()==0){ #function is being called from the global env
					return(invisible(NULL))
				}else{
					return(TRUE)
				}
			}
		}
	}

	#compile exe
	cat(crayon::blue('  Compiling exe...\U00D'))
	exe_path_for_compile = fs::path_abs(fs::path_ext_remove(code_path)) #must be absolute
	make_run = processx::run(
		command = cmdstanr:::make_cmd()
		, args = c(
			exe_path_for_compile
			, paste(
				'STANCFLAGS +='
				, '--include-paths', fs::path_dir(exe_path_for_compile)
				, '--name', mod_name
			)
		)
		, wd = cmdstanr::cmdstan_path()
		, error_on_status = F
		, spinner = T
	)

	if(make_run$stderr!=''){
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
	cat(crayon::blue('  ✓ Compile complete\n'))

	#move exe & delete the hpp
	fs::file_move(exe_path_for_compile,exe_path)
	fs::file_delete(fs::path_ext_set(code_path,'hpp'))

	#Generate data for runtime check
	cat(crayon::blue('  Checking for runtime errors...\U00D'))
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
	write(debug_data,dbg_path)

	#Perform runtime check
	debug_run = processx::run(
		command = paste0('./',exe_path)
		, args = c(
			'sample'
			, 'num_samples=1'
			, 'num_warmup=0'
			, 'adapt'
			, 'engaged=0'
			, 'algorithm=fixed_param'
			, 'data'
			, paste0('file=',dbg_path)
			, 'output'
			, paste0('file=',tempfile())
		)
		, error_on_status = F
		, spinner = T
	)
	fs::file_delete(dbg_path)
	if(debug_run$stderr!=''){
		#                  Checking for runtime errors...
		cat(crayon::red('  Runtime error check FAILED.   \n\n'))
		if(debug_run$stdout!=''){
			cat(crayon::blue('STDOUT:\n'))
			cat(crayon::blue(debug_run$stdout),'\n\n',sep='')
		}
		cat(crayon::blue('STDERR:\n'))
		cat(crayon::red(debug_run$stderr),'\n',sep='')
		if(sys.parent()==0){ #function is being called from the global env
			return(invisible(NULL))
		}else{
			return(FALSE)
		}
	}
	#                   Checking for runtime errors...
	cat(crayon::blue('  ✓ Runtime check passed        \n')) #spaces to overwrite old string

	#get model metadata
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
	meta = jsonlite::fromJSON(info_run$stdout)
	meta$cmdstan_version = cmdstanr::cmdstan_version()
	meta$mod_txt = new_txt
	qs::qsave(meta,qs_path,preset='fast')

	#finally, return
	if(sys.parent()==0){ #function is being called from the global env
		return(invisible(NULL))
	}else{
		return(TRUE)
	}
}


