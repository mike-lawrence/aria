validate_boolean_aria_arg = function(arg_value,arg_name,default){
	#parse compile if present
	if(is.null(arg_value)){
		arg_value = default
	}else{
		if(length(arg_value)>1){
			aria:::boo()
			cat(aria:::red(paste0('Oops! Too many "\\aria: ',arg_name,'" lines found in .stan file. ')))
			stop()
		}else{
			if(!(arg_value %in% c(T,F))){
				true_options = c('1','T','TRUE','True')
				true_options = unique(c(true_options,tolower(true_options)))
				false_options = c('0','F','FALSE','False')
				false_options = unique(c(false_options,tolower(false_options)))
				if(!(arg_value %in% c(true_options,false_options))){
					aria:::boo()
					cat(aria:::red(paste0("Oops! Invalid value found on \"\\\\ aria: ",arg_name,"\" line in .stan file.\n")))
					cat(aria:::red("Value found:\n\t"))
					cat(aria:::red(paste0("\\\\ aria: ",arg_name," = ",arg_value,'\n')))
					cat(aria:::blue("Valid values:\n\t"))
					cat(paste0("\\\\ aria: ",arg_name," = ",c(true_options,false_options),collapse='\n\t'))
					stop()
				}else{
					if(arg_value %in% true_options){
						arg_value = T
					}else{
						arg_value = F
					}
				}
				return(arg_value)
			}
		}
	}

}
