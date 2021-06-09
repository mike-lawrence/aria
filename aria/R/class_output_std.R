class_output_std = R6::R6Class(
	classname = 'class_output_std'
	, inherit = class_output
	, public = list(
		ingest_on_next_pass = FALSE
		, ingest = function(force=FALSE){
			#get the old file size
			old_file_size = self$last_file_size
			#get the new file size
			file_size_now = file.size(self$file)
			#store the new file size for next pass
			self$last_file_size = file_size_now
			if(!force){
				if(file_size_now>old_file_size){
					#file size has changed, flag to ingest next pass
					self$ingest_on_next_pass = TRUE
					return(invisible(self))
				}else{
					#file is the same as last pass
					if(!self$ingest_on_next_pass){
						#file has remained the same and we already ingested
						return(invisible(self))
					}
				}
			}
			if(!fs::file_exists(self$file)){
				return(invisible(self))
			}
			# forced or flagged, ingest time
			#TODO: can grep/sed be used to skip reading already ingested lines?
			# for now we're going to re-read all the messages
			#    TODO: this will break if they're stored in nc4
			# start a pipeline
			(
				self$file
				%>% readLines()
				%>% paste(collapse='\n')
				#"messages" delimited by double '\n'
				%>% stringr::str_split(stringr::fixed('\n\n'))
				# split yields a list of length 1 :(
				%>% purrr::pluck(1)
				# toss any empty elements
				%>% stringi::stri_remove_empty()
				# create tibble
				%>% tibble::tibble(message=.)
				# add chain
				%>% dplyr::mutate(chain=as.numeric(self$chain_name))
				# arrange columns
				%>% dplyr::select(chain,dplyr::everything())
			) ->
				#assign back to self$parsed
				self$parsed
			#return
			return(invisible(self))
		}
		# , extract_timing = function(){
		#
		# }
	)
	# , private = list()
)
