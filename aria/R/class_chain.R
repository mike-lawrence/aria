class_chain = R6::R6Class(
	classname = 'class_chain'
	, public = list(
		name = NULL
		, sampling_info = NULL
		, dir = NULL
		, stdout = NULL
		, stderr = NULL
		, samples = NULL
		, adapt_info = NULL
		, process = NULL
		, start_time = NULL
		, stopped_time = NULL
		, times_from_stdout = NULL
		, progress_partial_chars = c('\U258F','\U258E','\U258D','\U258C','\U258B','\U258A','\U2589','\U2588')
		, initialize = function(name,sampling_info){
			self$name = name
			self$sampling_info = sampling_info
			#create dir & output instances
			self$dir = fs::path('aria','sampling',name)
			fs::dir_create(self$dir)
			self$stdout = class_output_std$new(
				name = 'stdout'
				, chain_name = self$name
				, sampling_info = self$sampling_info
			)
			self$stderr = class_output_std$new(
				name = 'stderr'
				, chain_name = self$name
				, sampling_info = self$sampling_info
			)
			self$samples = class_output_samples$new(
				name = 'samples'
				, chain_name = self$name
				, sampling_info = self$sampling_info
			)
			self$sampling_info$exe_args_list$output$file = self$samples$file
			self$adapt_info = list(step_size=NULL,mass_matrix=NULL)
			self$process = processx::process$new(
				command = fs::path('.','aria','exes',self$sampling_info$mod_name,'stan_exe')
				, args = c(
					paste0('id=',self$name)
					, exe_args_list_to_vec(self$sampling_info$exe_args_list)
				)
				, stdout = self$stdout$file
				, stderr = self$stderr$file
				#, cleanup = FALSE
			)
			self$start_time = Sys.time()
			return(invisible(self))
		}
		, ingest = function(force=F){
			self$samples$ingest()
			self$stdout$ingest(force)
			self$stderr$ingest(force)
			return(invisible(self))
		}
		, finalize = function(){
			self$stopped_time = Sys.time()
			self$get_times_from_stdout()
			self$process = NULL
			return(invisible(self))
		}
		, get_times_from_stdout = function(){
			#force a final ingest
			self$stdout$ingest(force=T)
			#extract the elapsed time messages
			elapsed = dplyr::filter(self$stdout$parsed,stringr::str_starts(message,' Elapsed Time'))
			#remove the elapsed time messages from the stored rep
			self$stdout$parsed = dplyr::filter(self$stdout$parsed,!stringr::str_starts(message,' Elapsed Time'))
			#parse:
			if(nrow(elapsed)>0){
				(
					elapsed
					%>% dplyr::mutate(
						message = stringr::str_remove_all(message,'Elapsed Time:')
						, message = stringr::str_remove_all(message,'seconds')
						, message = stringr::str_remove_all(message,' ')
						, message = stringr::str_remove_all(message,"\\s*\\([^\\)]+\\)")
						# , message = stringr::str_replace(message,'\\n',',')
					)
					%>% tidyr::separate(
						col = message
						, into = c('warmup','sampling','total')
						, sep='\n'
						, convert = TRUE
						, extra = 'drop'
					)
				) ->
					self$times_from_stdout
			}
			return(invisible(self))
		}
		, cat_progress = function(){
			cat(self$name,':',sep='')
			iter_done_num = nrow(self$samples$parsed)
			if(iter_done_num==0){
				cat(' waiting\n')
				return(invisible(self))
			}
			bar_width = getOption('width') - nchar('0: 100% [] 99m?')
			if(iter_done_num==self$sampling_info$num_total){
				cat(
					' 100% ['
					, strrep('\U2588',bar_width)
					, '] '
					, aria:::vague_dt(as.double(self$times_from_stdout$total[1],units='secs'))
					,'✓\n'
					, sep = ''
				)
				return(invisible(self))
			}
			done_prop = iter_done_num / self$sampling_info$num_total
			cat(' ',format(floor(100*done_prop),width=3),'% [',sep='')
			done_width_decimal = done_prop*bar_width
			done_width_floor = floor(done_width_decimal)
			cat(strrep('\U2588',done_width_floor))
			done_width_remainder = done_width_decimal - done_width_floor
			done_width_which_partial = round(8*done_width_remainder)
			if(done_width_which_partial>0){
				cat(self$progress_partial_chars[done_width_which_partial])
				todo_width = bar_width - done_width_floor - 1
			}else{
				todo_width = bar_width - done_width_floor
			}
			cat(strrep(' ',todo_width), '] ',sep='')
			if(iter_done_num<=self$sampling_info$num_warmup){ #still in warmup
				numerator = iter_done_num
				denominator = self$sampling_info$num_total
				this_section_elapsed = as.double(Sys.time() - self$start_time, units = 'secs')
				eta_suffix = '?'
			}else{ #done warmup, use only sampling info for time estimate
				numerator = iter_done_num - self$sampling_info$num_warmup
				denominator = self$sampling_info$num_samples
				this_section_elapsed = as.double(Sys.time() - self$samples$sampling_start_time, units = 'secs')
				eta_suffix = ' '
			}
			cat(
				aria:::vague_dt(
					(this_section_elapsed/numerator) * (denominator-numerator)
				)
				, eta_suffix
				, '\n'
				, sep=''
			)
			return(invisible(self))
		}

	)
	# , private = list()
)
