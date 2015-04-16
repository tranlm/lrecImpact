###############################################################################
# Description: Gets the last visit before being marked LTFU
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 18, 2015
###############################################################################


#' Last observation before censoring
#' 
#' \code{Lastvisit} gets the last observation before censoring at the user specified day limit. 
#' 
#' This code assumes that the date vector is already in order and will return the last visit date before the specified number of days have passed. 
#' 
#' @param x An ordered vector of observed visits.
#' @param limit The maximum number of days that can pass before the subject is censored.
#' @return \code{Lastvisit} returns the last observed visit prior to being censored.
#' 
#' @export
Lastvisit = function(x, limit) {
	xnext = rep(NA, length(x))
	if(length(x)==1) {
		ltfuDate = x
	} else {
		xnext[1:(length(x)-1)] = x[2:length(x)]
		xdiff = xnext - as.numeric(x)
		ltfuObs = suppressWarnings(min(which(xdiff>=limit)))
		if(ltfuObs==Inf) {
			ltfuDate = x[length(x)]
		} else {
			ltfuDate = as.Date(x[ltfuObs], origin="1970-01-01")
		}
	}
	return(ltfuDate)
}
