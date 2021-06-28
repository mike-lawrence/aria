class_score = R6::R6Class(
	classname = 'class_score'
	, public = list(
		out_path = NULL
		, nc = NULL
		, num_warmup = NULL
		, nc_groups = NULL
		, groups = NULL
		, initialize = function(out_path){
			self$out_path = out_path
			self$nc = RNetCDF::open.nc(self$out_path)
			self$num_warmup = RNetCDF::att.get.nc(self$nc, 'NC_GLOBAL', 'num_warmup',fitnum=T)
			self$nc_groups = list()
			self$groups = list()
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
					}
				}
			}
			return(invisible(self))
		}
		, print_info = function(){
			RNetCDF::print.nc(self$nc)
			return(invisible(self))
		}
		, as_rvar = function(var,inc_warmup=F){
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
			arr = RNetCDF::var.get.nc(
				ncfile = self$nc_groups[[grp_name]]
				, variable = var
				, start = dim_starts
				, count = dim_counts
			)

			dimnames(arr) = list()
			for(i in 1:length(dim_names)){
				dimnames(arr)[[i]] = paste(dim_names[i],1:dim_counts[i],sep='.')
			}
			out = posterior::as_draws_rvars(arr)
			return(out)
		}
	)
	# , private = list(
	# 	nc = NULL
	# 	, nc_groups = NULL
	# )
)

# p = class_score$new('sampled.nc')
# str(p)
# p$print_info()
# str(RNetCDF::print.nc(p$nc_groups$parameters))
