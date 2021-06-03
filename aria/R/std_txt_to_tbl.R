std_txt_to_tbl = function(file,skip,sample_csv_col_names,chain_name,num_warmup){
	lines = readLines(file)
	chars = paste(lines[(skip+1):length(lines)],collapse='\n')
	messages = stringi::stri_remove_empty(
		stringr::str_split(chars,stringr::fixed('\n\n'))[[1]]
	)
	return(
		tibble::tibble(message=messages)
		%>% dplyr::mutate(chain=as.numeric(chain_name))
		%>% dplyr::select(chain,dplyr::everything())
	)
}

