initialize_nc = function(sampling_info){
	# add the lengths of any quantities
	for(quantity_type in c('parameters','transformed parameters','generated quantities')){
		sampling_info$mod_info[[quantity_type]] %<>% purrr::map2(
			.x = .
			, .y = names(.)
			, function(par_info,par_name){
				if(par_info$dimensions==0){
					return(par_info)
				}
				(
					sampling_info$samples_col_names[
						stringr::str_starts(
							sampling_info$samples_col_names
							, stringr::fixed(paste0(par_name,'.'))
						)
					]
					%>% stringr::str_remove(paste0(par_name,'.'))
					%>% stringr::str_split(stringr::fixed('.'),simplify=T)
					%>% apply(2,function(z){max(as.numeric(z))})
				) -> par_info$dimensions_lengths
				return(par_info)
			}
		)
	}

	#create the file pointer
	nc = RNetCDF::create.nc(
		filename = fs::path(sampling_info$out_path)
		, format = 'netcdf4'
		, prefill = FALSE
		#, share = TRUE #in case there are other readers
	)
	RNetCDF::att.put.nc(nc,'NC_GLOBAL','num_warmup','NC_USHORT',sampling_info$num_warmup)

	nc_groups = list()
	#initialize the adapt_info group
	nc_groups$adapt_info = RNetCDF::grp.def.nc(nc,'adapt_info')
	RNetCDF::dim.def.nc(nc_groups$adapt_info,'chain',sampling_info$num_chains)
	RNetCDF::var.def.nc(nc_groups$adapt_info,'chain','NC_UBYTE','chain')
	RNetCDF::var.def.nc(nc_groups$adapt_info,'step_size','NC_DOUBLE','chain')
	if(is.null(sampling_info$exe_args_list$sample$algorithm$hmc$metric)){
		metric = 'diag_e'
	}else{
		metric = sampling_info$exe_args_list$sample$algorithm$hmc$metric
	}
	if(metric!='unit_e'){
		RNetCDF::dim.def.nc(nc_groups$adapt_info,'metric.1',length(sampling_info$samples_col_names)-7)
		if(metric=='diag_e'){
			RNetCDF::var.def.nc(nc_groups$adapt_info,'metric','NC_DOUBLE',c('chain','metric.1'))
		}else{
			RNetCDF::dim.def.nc(nc_groups$adapt_info,'metric.2',length(sampling_info$samples_col_names)-7)
			RNetCDF::var.def.nc(nc_groups$adapt_info,'metric','NC_DOUBLE',c('chain','metric.1','metric.2'))
		}
	}

	#initialize the rest of the nc_groups
	for(grp_name in c('sample_stats','parameters','transformed_parameters','generated_quantities')){
		grp = RNetCDF::grp.def.nc(nc,grp_name)
		RNetCDF::dim.def.nc(grp,'chain',sampling_info$num_chains)
		RNetCDF::dim.def.nc(grp,'draw',sampling_info$num_total)
		RNetCDF::var.def.nc(grp,'chain','NC_UBYTE','chain') #num chains unlikely to exceed 2^8
		RNetCDF::var.def.nc(grp,'draw','NC_USHORT','draw') #num draws unlikely to exceed 2^16
		if(grp_name=='sample_stats'){
			vars = purrr::map(
				.x = c('lp','accept_stat','stepsize','treedepth','n_leapfrog','divergent','energy')
				, .f = function(x){
					list(
						name = x
						, dimensions = 0
						, nc_type = dplyr::case_when(
							x %in% c('accept_stat','stepsize','energy','lp') ~ 'NC_DOUBLE'
							, x %in% c('treedepth','divergent') ~ 'NC_UBYTE' #divergent is 0/1, treedepth is unlikely to exceed 2^8
							, x %in% c('n_leapfrog') ~ 'NC_USHORT' # number of leapfrogs unlikely to exceed 2^16
						)
					)
				}
			)
			names(vars) = purrr::map_chr(vars,'name')
		}else{
			vars = sampling_info$mod_info[[stringr::str_replace(grp_name,'_', ' ')]]
			vars %<>% purrr::map(
				.f = function(x){
					x$nc_type = dplyr::case_when(
						x$type=='real' ~ 'NC_DOUBLE'
						, x$type=='int' ~ 'NC_INT'
					)
					return(x)
				}
			)
		}
		if(length(vars)>0){
			purrr::walk2(
				.x = vars
				, .y = names(vars)
				, grp = grp
				, .f = function(var_info,var_name,grp){
					var_dims = c('draw','chain')
					if(var_info$dimensions>0){
						for(i in 1:var_info$dimensions){
							dim_name = paste(var_name,i,sep='.')
							RNetCDF::dim.def.nc(grp, dim_name, var_info$dimensions_lengths[i] )
							var_dims = c(var_dims,dim_name)
						}
					}
					RNetCDF::var.def.nc(grp,var_name,var_info$nc_type,var_dims)
				}
			)
		}
		nc_groups[[grp_name]] = grp
	}

	sampling_info$nc = nc
	sampling_info$nc_groups = nc_groups
	return(sampling_info)
	# RNetCDF::print.nc(nc)
	# RNetCDF::print.nc(RNetCDF::open.nc('centered_eight.nc'))
}
