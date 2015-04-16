###############################################################################
# Description: Running highest value
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 18, 2015
###############################################################################


#' Running zenith value
#' 
#' \code{zenith} Takes the running highest value in vector of values pre-ordered by time.
#' 
#' This function assumes that the values in the supplied vector have already been ordered chronologically.
#' 
#' @param x Vector to take running zenith over.
#' @return \code{zenith} returns an vector of the zenith values.
#' 
#' @export
zenith = function(x){
	maxvalue = x
	if(length(x)>1){
		for(i in 2:length(x)){
			if(!is.na(maxvalue[i]) & !is.na(maxvalue[i-1]) & maxvalue[i-1]>maxvalue[i] | is.na(maxvalue[i])) maxvalue[i] = maxvalue[i-1]
		}
	}
	return(maxvalue)
}

