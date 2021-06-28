class_output_samples = R6::R6Class(
	classname = 'class_output_samples'
	, inherit = class_output
	, public = list(
		sampling_info = NULL
		, sampling_start_time = NULL
		, header_nlines = 0
		, adapt_nlines = 0
		, parsed_nlines = 0
		, initialize = function(name,chain_name,sampling_info){
			self$name = name
			self$chain_name = chain_name
			self$sampling_info = sampling_info
			self$file = fs::path('aria','sampling',self$chain_name,self$name)
			return(invisible(self))
		}
		, parse_and_flush_adapt_info = function(){
			#this section is fragile to changes in cmdstan csv design
			tail_grep_cmd = paste0(
				"tail -n+"
				, self$header_nlines + self$sampling_info$num_warmup + 1
				, " '"
				, self$file
				, "' | grep '^[#]' --color=never"
				, collapse = ''
			)
			#read the contents with grep
			(
				system(
					tail_grep_cmd
					, intern = T
				)
				%>% strsplit('\n')
				%>% unlist()
				%>% stringr::str_remove('#')
				# toss lines consisting of just a space or starting with a double space
				%>% {function(x){
					x = x[x!=' '] # just before the final timing line
					x = x[substr(x,1,2)!='  '] #final timing lines
					return(x)
				}}()
				%>% stringr::str_trim()
			) -> adapt_info_lines
			self$adapt_nlines = length(adapt_info_lines)
			step_size_line_num = which(stringr::str_detect(adapt_info_lines,'Step size = '))
			(
				adapt_info_lines[(step_size_line_num+2):self$adapt_nlines]
				#split to character matrix
				%>% stringr::str_split(
					stringr::fixed(',')
					, simplify = T
				)
				#numerify
				%>% {function(x){
					matrix(as.numeric(x),nrow=nrow(x))
				}}()
			) ->
				metric
			step_size = as.numeric(stringr::str_remove(adapt_info_lines[step_size_line_num],'Step size = '))
			#write to file
			grp = self$sampling_info$nc_groups$adapt_info
			RNetCDF::var.put.nc(grp,'step_size',step_size,start=as.numeric(self$chain_name))
			RNetCDF::var.put.nc(grp,'step_size',step_size,start=as.numeric(self$chain_name))
			RNetCDF::var.put.nc(grp,'metric',metric,start=c(as.numeric(self$chain_name),1))
			#done
			return(invisible(self))
		}
		, flush_samples = function(samples){
			purrr::walk2(
				.x = samples
				, .y = names(samples)
				, .f = function(data,csv_col_name){
					start = c(
						self$parsed_nlines+1
						, as.numeric(self$chain_name)
					)
					if(stringr::str_ends(csv_col_name,'__')){
						grp_name = 'sample_stats'
						variable = stringr::str_remove(csv_col_name,'__')
					}else{
						(
							csv_col_name
							%>% stringr::str_split(stringr::fixed('.'))
							%>% unlist()
							%>% head(1)
						) -> variable
						grp_name = dplyr::case_when(
							variable %in% names(self$sampling_info$mod_info$parameters) ~ 'parameters'
							, variable %in% names(self$sampling_info$mod_info$`transfomed parameters`) ~ 'transformed_parameters'
							, variable %in% names(self$sampling_info$mod_info$`generated quantities`) ~ 'generated_quantities'
						)
					}
					(
						csv_col_name
						%>% stringr::str_split(stringr::fixed('.'))
						%>% unlist()
						%>% tail(-1)
						%>% as.numeric()
						%>% c(start,.)
					) -> start
					count = c(length(data),1,rep(1,length(start)-2))
					RNetCDF::var.put.nc(
						ncfile = self$sampling_info$nc_groups[[grp_name]]
						, variable = variable
						, data = data
						, start = start
						, count = count
					)
				}
			)
			#update parsed_nlines
			self$parsed_nlines = self$parsed_nlines + nrow(samples)
			return(invisible(self))
		}
		, ingest_header = function(){
			(
				paste0("grep '^[#l]' '",self$file,"'")
				%>% system(intern=T)
				%>% strsplit('\n')
				%>% unlist()
			) -> header
			header_nlines = which(stringr::str_starts(header,'lp'))
			if(length(header_nlines)==0){
				return(invisible(self))
			}
			self$header_nlines = header_nlines
			found_samples_col_names = unlist(strsplit(header[self$header_nlines],','))
			if(!all(self$sampling_info$samples_col_names==found_samples_col_names)){
				stop('Samples header mismatch.')
			}
			return(invisible(self))
		}
		, ingest = function(){
			if(!fs::file_exists(self$file)){
				return(invisible(self))
			}
			#get the old file size
			old_file_size = self$last_file_size
			#get the new file size
			file_size_now = file.size(self$file)
			#store the new file size for next pass
			self$last_file_size = file_size_now
			#if nothing has changed, return
			if(file_size_now==old_file_size){
				return(invisible(self))
			}
			#read the header if not done already
			if(is.null(self$header_n_lines)){
				self$ingest_header()
			}
			orig_warn = options(warn=-1)
			from_fread = NULL
			try(
				from_fread <- data.table::fread(
					cmd = paste0(
						"tail -n+"
						, self$header_nlines + self$adapt_nlines + self$parsed_nlines + 1
						, " '"
						, self$file
						, "' | grep -v '^[#]' --color=never"
					)
					, data.table = FALSE
					, sep = ','
					, header = F
					, col.names = self$sampling_info$samples_col_names
					, colClasses = self$sampling_info$samples_col_classes
				)
				, silent = T
			)
			options(orig_warn)
			if(is.null(from_fread)){
				return(invisible(self))
			}
			if(nrow(from_fread)==0){
				return(invisible(self))
			}
			# prep & flush
			(
				from_fread
				%>% tibble::as_tibble()
				#toss any rows with NA (means the csv writer was still writing that line)
				%>% tidyr::drop_na()
				# send to the sampling_info$nc4
				%>% self$flush_samples() #also updates self$parsed_nlines
			)
			#check if adaptation info is unread
			if(is.null(self$sampling_start_time)){
				#check if adaptation info is available
				if(self$parsed_nlines>self$sampling_info$num_warmup){
					self$sampling_start_time = Sys.time()
					self$parse_and_flush_adapt_info()
				}
			}
			#return
			return(invisible(self))
		}

	)
)

