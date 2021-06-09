class_output_samples = R6::R6Class(
	classname = 'class_output_samples'
	, inherit = class_output
	, public = list(
		adapt_info = list(
			step_size = NULL
			, metric = NULL
		)
		, sampling_start_time = NULL
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
			tmp = 1:length(self$sampling_info$samples_header)
			options(warn=-1)
			from_fread = NULL
			try(
				from_fread <- data.table::fread(
					cmd = paste0("grep -v '^[#l]' --color=never '", self$file, "'")
					, data.table = FALSE
					, sep = ','
					# , colClasses = 'double'
					, header = F
					, col.names = self$sampling_info$samples_header
					, colClasses = list(
						numeric = tmp[!(tmp%in%(4:6))]
						, integer = 4:6
						# , logical = 6
					)
					, skip = nrow(self$parsed)
				)
				, silent = T
			)
			if(is.null(from_fread)){
				return(invisible(self))
			}
			# start a pipeline
			(
				from_fread
				%>% tibble::as_tibble()
				%>% dplyr::mutate(
					iteration = (1:dplyr::n()) + nrow(self$parsed)
				)
				# #read the contents with grep (yields vector of lines)
				# system2(
				# 	command = "grep"
				# 	, args = c(
				# 		"'^[#l]'"
				# 		, "-v"
				# 		, "--color=never"
				# 		, self$file
				# 	)
				# 	, stdout = TRUE
				# )
				# #toss the lines we have already
				# %>% {function(x){
				# 	x[(nrow(self$parsed)+1):length(x)]
				# }}()
				# #split to character matrix
				# %>% stringr::str_split(
				# 	stringr::fixed(',')
				# 	, simplify = T
				# )
				# #numerify
				# %>% {function(x){
				# 	matrix(as.numeric(x),nrow=nrow(x))
				# }}()
				# # add dimnames
				# %>% {function(x){
				# 	dimnames(x) = list()
				# 	dimnames(x)[[1]] = nrow(self$parsed) + (1:nrow(x))
				# 	dimnames(x)[[2]] = self$sampling_info$samples_header
				# 	return(x)
				# }}()
				# # convert to tibble
				# %>% tibble::as_tibble(rownames='iteration')
				#toss any rows with NA (means the csv writer was still writing that line)
				%>% tidyr::drop_na()
				#add columns
				%>% dplyr::mutate(
					chain = as.numeric(self$chain_name)
					, iteration = as.numeric(iteration)
					, warmup = iteration<=self$sampling_info$num_warmup
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
				if(nrow(self$parsed)>self$sampling_info$num_warmup){
					self$sampling_start_time = Sys.time()
					#read the contents with grep
					comments = system2(
						command = "grep"
						, args = c(	"^[#]" , "--color=never" , self$file )
						, stdout = TRUE
					)
					comments = comments[which(comments=='# Adaptation terminated'):length(comments)]
					step_size_line = comments[2]
					self$adapt_info$step_size = substr(step_size_line,15,nchar(step_size_line))
					(
						comments
						%>% stringr::str_remove(stringr::fixed('# '))
						%>% {function(x){
							x[stringr::str_starts(x,'[0-9[-]]')]
						}}()
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

