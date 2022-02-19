run_debug = function(debug_exe_file,data_file,return_header=FALSE){
	#Perform runtime check
	cat(aria:::blue('  Running debug check...\U00D'))
	out_file = tempfile()
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
			, paste0('file=',out_file)
		)
		, error_on_status = F
		, spinner = T
	)
	if(debug_run$stderr!=''){
		aria:::boo()
		cat(aria:::red('  Debug check FAILED.   \n\n'))
		if(debug_run$stdout!=''){
			cat(aria:::blue('STDOUT:\n'))
			cat(aria:::blue(debug_run$stdout),'\n\n',sep='')
		}
		cat(aria:::blue('STDERR:\n'))
		cat(aria:::red(debug_run$stderr),'\n',sep='')
		return(FALSE)
	}
	cat(aria:::blue('  ✓ Debug check passed  \n')) #spaces to overwrite old string

	if(return_header){(
		system2(
			command = "grep"
			, args = c(	"^lp", "--color=never", out_file)
			, stdout = TRUE
		)
		%>% strsplit(',')
		%>% unlist()
		%>% return()
	)}else{
		return(TRUE)
	}
}
