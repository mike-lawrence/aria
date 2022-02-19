check_syntax = function(aria_args){
	code_file = fs::path_file(aria_args$code_path)
	mod_name = fs::path_ext_remove(code_file)

	stanc_syntax_check_run = processx::run(
		command = fs::path(cmdstanr::cmdstan_path(),cmdstanr:::stanc_cmd())
		, args = c(
			code_file
			, '--include-paths','.'
			, '--warn-pedantic'
			, '--warn-uninitialized'
			, '--name', mod_name
			,'--o', tempfile()
		)
		, wd = fs::path_dir(aria_args$code_path)
		, error_on_status = F
	)
	syntax_check_passed = TRUE
	if(stanc_syntax_check_run$stderr==''){
		cat(aria:::blue('  ✓ Syntax check passed\n'))
	}else{
		# join entries
		(
			stanc_syntax_check_run$stderr
			%>% stringr::str_split('\n')
			%>% purrr::pluck(1)
			%>% stringi::stri_remove_empty()
			%>% tibble::tibble(line=.)
			%>% dplyr::mutate(
				ew_line_set = cumsum(stringr::str_starts(line,'Warning|Error'))
			)
			%>% dplyr::group_by(ew_line_set)
			%>% dplyr::summarise(
				entry = paste(line,collapse=' ')
				, .groups = 'drop'
			)
			%>% dplyr::pull(entry)
			%>% stringr::str_replace_all('\\h+',' ')
		) ->
			ew_entries
		#split out errors
		error_list = ew_entries[stringr::str_starts(ew_entries,'Error')]
		if(length(error_list)>0){
			syntax_check_passed = FALSE
			aria:::boo()
			cat(aria:::red('Stan syntax-check encountered the following ERRORS:'),'\n',sep='')
			cat(aria:::red(paste(error_list,collapse='\n \n')),'\n \n',sep='')
		}
		#split out warnings & check if in aria_args$syntax_ignore
		(
			ew_entries[stringr::str_starts(ew_entries,'Warning')]
			%>% tibble::tibble(entry=.)
			%>% dplyr::mutate(
				briefer_entry = stringr::str_remove_all(entry,'^Warning.*: ')
				, entry_in_ignore = briefer_entry %in% aria_args$syntax_ignore
			)
		) ->
			warnings_tbl
		warnings_to_print = (warnings_tbl %>% dplyr::filter(!entry_in_ignore) %>% dplyr::pull(entry))
		if(length(warnings_to_print)>0){
			syntax_check_passed = FALSE
			aria:::boo()
			cat(aria:::red('Stan syntax-check encountered the following WARNINGS:\n \n'))
			cat(aria:::red(paste(warnings_to_print,collapse='\n \n')),'\n \n \n',sep='')
			cat(aria:::blue("You should probably fix the source of these warnings, but if you feel they are false-alarms and want to instead simply ignore them, add the following to the top of your .stan file:\n \n"))
			(
				warnings_tbl
				%>% dplyr::pull(briefer_entry)
				%>% dQuote(q=F)
				%>% paste(collapse=', ')
				%>% paste0('//aria: syntax_ignore = c(',.,')')
			) ->
				aria_syntax_ignore_recommendation
			cat(aria_syntax_ignore_recommendation,'\n \n \n',sep='')
		}
	}
	if(stanc_syntax_check_run$stdout!=''){
		cat(aria:::blue('Stan syntax-check encountered the following informational messages:\n \n'))
		cat(stanc_syntax_check_run$stdout)
	}
	if(sys.parent()==0){ #function is being called from the global env
		return(invisible(NULL))
	}else{
		return(syntax_check_passed)
	}
}
