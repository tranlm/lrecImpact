###############################################################################
# Description: Does last observation carried forward, but with a limit in how
#			   far the imputation will occur.
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 13, 2015
###############################################################################

#' Limited last observation carried forward imputation
#' 
#' \code{limitedImpute} will carry forward the last observation up to a specified threshold of days. 
#' 
#' This function is meant for indicators with missing values known to only occur for a limited amount of time. Examples include pregnancy (45-weeks) and tuberculosis treatment (8 months). Once the specified threshold has passed, the indicator reverts back to 0. If the values are non-missing, the value gets used regardless of whether it surpasses the threshold.
#' 
#' @param var Variable to be imputed forward in time. 
#' @param date Date variable corresponding to when \code{var} was measured. 
#' @param limit Number of days the forward imputation is limited to.
#' @return The function returns a vector of length \code{var}, with the limited imputation carried out.
#' 
#' @export
limitedImpute = function(var, date, limit) {
	ind = 0; indDate = NA
	new.var = rep(NA, length(var))
	for(i in 1:length(var)) {
		# Event occurs
		if(!is.na(var[i]) & var[i]==1 & ind==0) {
			ind = 1; indDate = date[i]
			new.var[i] = var[i]
		#Not indnant
		} else if(!is.na(var[i]) & var[i]==0) {
			ind = 0; indDate = NA
			new.var[i] = var[i]
		#Missing
		} else if(is.na(var[i])) {
			if(ind==0) new.var[i] = 0
			if(ind==1) {
				if(as.numeric(date[i] - indDate) <= limit) {
					new.var[i] = 1
				} else new.var[i] = 0
			}
		} else new.var[i] = var[i]
	}
	return(new.var)
}

