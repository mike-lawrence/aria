#' Compile Stan code and check for runtime errors
#'
#' This function first checks if th Stan code has changed in functional ways (i.e. not comments nor whitespace), compiles when such changes exist then runs a simple test for runtime errors.
#'
#' @param code_path Character string describing the path to the Stan code.
#'
#' @return NULL (invisibly); Side effects: an eponymous executable binary is placed in a folder called `stan_tmp` (along with some other helper files).
#' @export
#'
#' @family Model checking & compilation functions
#'
#' @examples
#' \dontrun{
#' compile('my_model.stan')
#' }
compile = function(code_path){

	#get some paths, create stan_tmp
	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	exe_path = fs::path('stan_tmp',fs::path_ext_remove(code_file))
	txt_path = fs::path_ext_set(exe_path,ext='txt')
	dbg_path = fs::path_ext_set(paste0(exe_path,'_debug'),ext='json')
	fs::dir_create('stan_tmp')

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
	new_txt_path = tempfile()
	write(new_txt,file=new_txt_path)
	if(fs::file_exists(exe_path)){
		if(fs::file_exists(txt_path)){
			old_digest = digest::digest(file=txt_path,algo='xxhash64')
			new_digest = digest::digest(file=new_txt_path,algo='xxhash64')
			if((old_digest==new_digest)){
				cat(crayon::blue('  ✓ Compiled exe is up to date.'))
				return(invisible(NULL))
			}
		}
	}
	fs::file_move(new_txt_path,txt_path)

	#compile exe
	cat(crayon::blue('  Compiling exe...\U00D'))
	# cat(crayon::blue('  Compiling exe'))
	exe_path_for_compile = fs::path_abs(fs::path_ext_remove(code_path)) #must be absolute
	make_run = processx::run(
		command = cmdstanr:::make_cmd()
		, args = c(
			exe_path_for_compile
			, paste(
				'STANCFLAGS +='
				, '--include-paths', fs::path_dir(exe_path_for_compile)
				, '--name', mod_name
				, '--o', tempfile()
			)
		)
		, wd = cmdstanr::cmdstan_path()
		, error_on_status = F
		, spinner = T
	)
	cat(crayon::blue('  ✓ Compile complete\n'))

	if(make_run$stderr!=''){
		cat(crayon::blue(make_run$stdout))
		cat('\n\n')
		cat(crayon::red(make_run$stderr))
		cat('\n\n')
		return(invisible(NULL))
	}

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
		command = paste0('./',mod_name)
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
			, paste0('file=',fs::path_ext_set(mod_name,ext='.log'))
		)
		, wd = 'stan_tmp'
		, error_on_status = F
		, spinner = T
	)
	if(debug_run$stderr!=''){
		cat(crayon::blue('  Checking for runtime errors...\U00D'))
		cat(crayon::red('  Runtime error check FAILED.   \n\n'))
		if(debug_run$stdout!=''){
			cat(crayon::blue('STDOUT:\n'))
			cat(crayon::blue(debug_run$stdout),'\n\n',sep='')
		}
		cat(crayon::blue('STDERR:\n'))
		cat(crayon::red(debug_run$stderr),'\n',sep='')
		return(invisible(NULL))
	}

	cat(crayon::blue('  ✓ Runtime check passed        \n')) #spaces to overwrite old string
	return(invisible(NULL))
}

