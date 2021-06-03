samples_csv_to_tbl = function(file,skip,sample_csv_col_names,chain_name,num_warmup){
	samples = system2(
		command = "grep"
		, args = c(
			"'^[#l]'"
			, "-v"
			, "--color=never"
			, file
		)
		, stdout = TRUE
	)
	samples = stringr::str_split(
		samples[(skip+1):length(samples)]
		, stringr::fixed(',')
		, simplify=T
	)
	samples = matrix(
		as.numeric(samples)
		, nrow = nrow(samples)
		, ncol = ncol(samples)
	)
	if(any(is.na(samples[nrow(samples),]))){
		samples = samples[1:(nrow(samples)-1),]
	}
	dimnames(samples) = list(
		(1:nrow(samples))+skip
		, sample_csv_col_names
	)
	return(
		samples
		%>% tibble::as_tibble(rownames='iteration')
		%>% dplyr::mutate(
			chain = as.numeric(chain_name)
			, iteration = as.numeric(iteration)
			, warmup = iteration<=num_warmup
		)
		%>% dplyr::select(chain,iteration,warmup,dplyr::everything())
	)
}
