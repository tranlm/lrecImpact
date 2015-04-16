###############################################################################
# Description: Function to take nadir values
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 18, 2015
###############################################################################


#' Running nadir value
#' 
#' \code{nadir} Takes the running lowest value in vector of values pre-ordered by time.
#' 
#' This function assumes that the values in the supplied vector have already been ordered chronologically.
#' 
#' @param x Vector to take running nadir over.
#' @return \code{nadir} returns an vector of the nadir values. 
#' 
#' @export
nadir = function(x){
	minvalue = x
	if(length(x)>1){
		for(i in 2:length(x)){
			if(!is.na(minvalue[i]) & !is.na(minvalue[i-1]) & minvalue[i-1]<minvalue[i] | is.na(minvalue[i])) minvalue[i] = minvalue[i-1]
		}
	}
	return(minvalue)
}

