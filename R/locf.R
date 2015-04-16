###############################################################################
# Description: Carries last observation recorded forward for any missing values.
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 13, 2015
###############################################################################

#' @export
locf = function(x){
	new_x = x
	if(length(new_x)>1){
		for(i in 2:length(new_x)){ 
			if(is.na(new_x[i])) new_x[i] = new_x[i-1]
		}
	}
	return(new_x)
}

