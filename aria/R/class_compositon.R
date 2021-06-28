class_compositon = R6::R6Class(
	classname = 'class_compositon'
	, public = list(
		out_path = NULL
		, nc = NULL
		, num_warmup = NULL
		, nc_groups = NULL
		, groups = NULL
		, variables = NULL
		, initialize = function(out_path){
			self$out_path = out_path
			self$nc = RNetCDF::open.nc(self$out_path)
			self$num_warmup = RNetCDF::att.get.nc(self$nc, 'NC_GLOBAL', 'num_warmup',fitnum=T)
			self$nc_groups = list()
			self$groups = list()
			self$variables = list()
			for(grp_name in c('adapt_info','sample_stats','parameters','transformed_parameters','generated_quantities')){
				grpinfo = RNetCDF::grp.inq.nc(self$nc,grp_name)
				self$nc_groups[[grp_name]] = grpinfo$self
				self$groups[[grp_name]] = list()
				for(varid in grpinfo$varids){
					varinfo = RNetCDF::var.inq.nc(grpinfo$self, varid)
					if(!(varinfo$name %in% c('chain','draw'))){
						dims = list()
						for(dimid in varinfo$dimids){
							diminfo = RNetCDF::dim.inq.nc(grpinfo$self, dimid)
							dims[[diminfo$name]] = list(
								name = diminfo$name
								, length = diminfo$length
							)
						}
						self$groups[[grp_name]][[varinfo$name]] = list(
							name = varinfo$name
							, dims = dims
						)
						if(!(grp_name %in% c('adapt_info','sample_stats'))){
							self$variables[[varinfo$name]] = list(
								name = varinfo$name
								, group = grp_name
								, dims = dims
							)
						}
					}
				}
			}
			return(invisible(self))
		}
		, nc_info = function(){
			RNetCDF::print.nc(self$nc)
			return(invisible(self))
		}
		, nc_to_rvar = function(var,inc_warmup=F){
			which_grp = which(purrr::map_lgl(self$groups,.f=function(x){var%in%names(x)}))
			if(length(which_grp)==0){
				stop('Variable not present.')
			}
			grp_name = names(which_grp)
			dim_counts = purrr::map_dbl(self$groups[[grp_name]][[var]]$dims,'length')
			dim_starts = rep(1,length(dim_counts))
			dim_names = names(dim_counts)
			if(!inc_warmup){
				dim_starts[dim_names=='draw'] = self$num_warmup+1
				dim_counts[dim_names=='draw'] = dim_counts[dim_names=='draw'] - self$num_warmup
			}
			(
				RNetCDF::var.get.nc(
					ncfile = self$nc_groups[[grp_name]]
					, variable = var
					, start = dim_starts
					, count = dim_counts
				)
				%>% {function(x){
					if(length(dim_counts)==2){
						dim(x) = prod(dim_counts[1:2])
					}else{
						dim(x) = c(prod(dim_counts[1:2]),dim_counts[3:length(dim_counts)])
					}
					return(x)
				}}()
				%>% posterior::rvar(
					nchains = dim_counts[2]
				)
				%>% return()
			)
		}
		, draws = function(variables=NULL,groups=NULL){
			if(is.null(variables)){
				if(is.null(groups)){
					variables = self$variables
				}else{
					if(!all(groups%in%names(self$groups))){
						cat('Oops! The following groups were not found:\n')
						for(group in groups){
							if(!(group%in%names(self$groups))){
								cat(group,'\n')
							}
						}
						stop()
					}
					variables = purrr::pluck(self$groups[groups],1)
				}
			}else{
				if(!all(variables%in%names(self$variables))){
					cat('Oops! The following variables were not found:\n')
					for(variable in variables){
						if(!(variable%in%names(self$variables))){
							cat(variable,'\n')
						}
					}
					stop()
				}
				variables = self$variables[variables]
			}
			(
				variables
				%>% purrr::map(
					.f = function(x){
						self$nc_to_rvar(x$name)
					}
				)
				%>% posterior::as_draws_rvars()
				%>% return()
			)
		}
	)
)
