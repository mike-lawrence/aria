class_output_samples = R6::R6Class(
	classname = 'class_output_samples'
	, inherit = class_output
	, public = list(
		adapt_info = list(
			step_size = NULL
			, metric = NULL
		)
		, sampling_start_time = NULL
		, header_nlines = NULL
		, adapt_nlines = 0
		, num_warmup = NULL
		, col_names = NULL
		, col_classes = NULL
		, initialize = function(name,chain_name,num_warmup,samples_col_names){
			self$name = name
			self$chain_name = chain_name
			self$num_warmup = num_warmup
			self$col_names = samples_col_names
			self$file = fs::path('aria','sampling',chain_name,name)
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
			found_col_names = unlist(strsplit(header[self$header_nlines],','))
			if(!all(self$col_names==found_col_names)){
				stop('Samples header mismatch.')
			}
			self$col_classes = list(
				integer = 4:6
				, numeric = c(1:3,7:length(self$col_names))
			)
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
			options(warn=-1)
			from_fread = NULL
			try(
				from_fread <- data.table::fread(
					cmd = paste0(
						"tail -n+"
						, self$header_nlines + self$adapt_nlines + nrow(self$parsed) + 1
						, " '"
						, self$file
						, "' | grep -v '^[#]' --color=never"
					)
					, data.table = FALSE
					, sep = ','
					, header = F
					, col.names = self$col_names
					, colClasses = self$col_classes
				)
				, silent = T
			)
			if(is.null(from_fread)){
				return(invisible(self))
			}
			if(nrow(from_fread)==0){
				return(invisible(self))
			}
			# start a pipeline
			(
				from_fread
				%>% tibble::as_tibble()
				%>% dplyr::mutate(
					iteration = (1:dplyr::n()) + nrow(self$parsed)
				)
				#toss any rows with NA (means the csv writer was still writing that line)
				%>% tidyr::drop_na()
				#add columns
				%>% dplyr::mutate(
					chain = as.numeric(self$chain_name)
					, iteration = as.numeric(iteration)
					, warmup = iteration<=self$num_warmup
					, treedepth__ = as.integer(treedepth__)
					, n_leapfrog__ = as.integer(n_leapfrog__)
					, divergent__ = as.logical(divergent__)
				)
				#sort columns
				%>% dplyr::select(chain,iteration,warmup,dplyr::everything())
				#bind to bottom of self$parsed
				%>% dplyr::bind_rows(self$parsed,.)
				# Note: this is inefficient now (copies) but with netcdf storage will be efficient
			) ->
				#assign to self$parsed
				self$parsed
			options(warn=0)
			#check if adaptation info is unread
			if(is.null(self$sampling_start_time)){
				#check if adaptation info is available
				if(nrow(self$parsed)>self$num_warmup){
					self$sampling_start_time = Sys.time()
					#this section is fragile to changes in cmdstan csv design
					tail_grep_cmd = paste0(
						"tail -n+"
						, self$header_nlines + self$num_warmup + 1
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
					#this substr
					self$adapt_info$step_size = as.numeric(stringr::str_remove(adapt_info_lines[step_size_line_num],'Step size = '))
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
					) -> self$adapt_info$metric
				}
			}
			#return
			return(invisible(self))
		}

	)
)

