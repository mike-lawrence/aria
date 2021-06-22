class_output = R6::R6Class(
	classname = 'class_output'
	, public = list(
		name = NULL
		, chain_name = NULL
		, sampling_info = NULL
		, file = NULL
		, last_file_size = 0
		, parsed = tibble::tibble()
		, initialize = function(name,chain_name){
			self$name = name
			self$chain_name = chain_name
			self$file = fs::path('aria','sampling',chain_name,name)
			return(invisible(self))
		}
	)
	# , private = list()
)

