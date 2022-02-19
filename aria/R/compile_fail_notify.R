compile_fail_notify = function(run_out,kind){
	aria:::boo()
	cat(aria:::red('Oops! Compilation of ',kind,' exe failed with the following STDERR messages:\n \n'))
	cat(aria:::red(run_out$stderr))
	if(run_out$stdout!=''){
		cat('\n \n')
		cat(aria:::blue('The following STDOUT messages were also found:\n \n'))
		cat(stdout)
		cat('\n \n')
	}
}
