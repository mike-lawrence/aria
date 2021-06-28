#' Retrieve the cmdstan arguments available for a compiled executable
#'
#' @param code_path Character string describing the path to the Stan code.
#'
#' @return A tibble with rows corresponding to available arguments and columns:
#' \describe{
#'   \item{level}{Argument level in the argument hierarchy.}
#'   \item{parent}{Argument parent in the argument hierarchy (i.e. string that would have to appear before the entry if typed at the command-line).}
#'   \item{name}{Argument name (i.e. string you'd actually type).}
#'   \item{description}{Description of the role of the argument.}
#'   \item{type}{Variable type that the value of the argument should be.}
#'   \item{valid}{Description of the values that would be valid values for the argument.}
#'   \item{default}{The default value used by cmdstan if this argument is not otherwise specified.}
#'   \item{children}{List of the argument's children in the argument hierarchy (if any).}
#'   \item{order}{Order that the argument appears in the original help output.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' exe_args_tbl('my_model.stan')
#' }
exe_args_tbl = function (code_path) {
	code_file = fs::path_file(code_path)
	mod_name = fs::path_ext_remove(code_file)
	fast_exe_file = fs::path('aria','exes',mod_name,'stan_exe')
	#look for exe
	if(!fs::file_exists(fast_exe_file)){
		stop(paste0('Compiled exe not found, exe `aria::compile("',code_path,'")` first.'))
	}
	#execute
	out = processx::run(
		command = paste0('./',fast_exe_file)
		, args = 'help-all'
	)
	#stop if error
	if(out$stderr!=''){
		stop(aria:::red(out$stderr))
	}
	#parse stdout and return at end
	(
		out$stdout
		# entries are separated by double-newlines
		%>% stringr::str_split(stringr::fixed('\n\n'))
		%>% unlist()
		# toss empties
		%>% purrr::compact()
		#start a tibble with the lines
		%>% tibble::tibble(line=.)
		# toss non-entries
		%>% dplyr::filter(stringr::str_starts(line,' '))
		# split out fields as columns
		%>% tidyr::separate(
			line
			, into = c('name','description','valid','default')
			, sep = '\n'
			, fill = 'right'
		)
		# split out type
		%>% tidyr::separate(
			name
			, into = c('name','type')
			, sep = '='
			, fill = 'right'
		)
		# add lots
		%>% dplyr::mutate(
			level = (nchar(name) - nchar(stringr::str_trim(name))) /2
			, name = stringr::str_trim(name)
			, type = stringr::str_remove(type,'<')
			, type = stringr::str_remove(type,'>')
			, description = stringr::str_trim(description)
			, valid = stringr::str_trim(valid)
			, valid = stringr::str_remove(valid,'Valid values: ')
			, valid = stringr::str_remove(valid,'Valid subarguments: ')
			, default = stringr::str_trim(default)
			, default = stringr::str_remove(default,'Defaults to ')
			, order = 1:(dplyr::n())
		)
		%>% dplyr::select(level,order,dplyr::everything())
		#add parent (must be a tidy-er way of doing this...)
		%>% {function(x){
			x$parent = NA
			for(i in 1:nrow(x)){
				parent_test = (
					(x$level<x$level[i])
					&(x$order<x$order[i])
				)
				if(any(parent_test)){
					x$parent[i] = x$name[max(x$order[parent_test])]
				}
			}
			return(x)
		}}()
		# add children & clean up valid
		%>% dplyr::mutate(
			valid = stringr::str_remove(valid,stringr::fixed('['))
			, valid = stringr::str_remove(valid,stringr::fixed(']'))
			, valid = stringr::str_split(valid,', ')
			, children = dplyr::case_when(
				is.na(default) | (name=='method') ~ valid
			)
			, valid = dplyr::case_when(
				!is.na(default) ~ valid
			)
		)
		#re-order columns
		%>% dplyr::select(
			level
			, parent
			, name
			, description
			, type
			, dplyr::everything()
			, -order,order
		)
		%>% return()
	)
}
# out = exe_args_tbl('stan/mvn.stan')
# View(out)
