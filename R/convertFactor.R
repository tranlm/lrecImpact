###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 22, 2015
###############################################################################


#' Factor converter
#' 
#' \code{convertFactor} converts factored/character vectors into a data frame of indicators, with the reference group being the largest.
#' 
#' @param x Vector of string or factors
#' @param name Name of vector supplied
#' @param ref Group to be considered reference. If NULL, then the largest group will be set as the reference.
#' @return \code{convertFactor} returns a data frame with indicators for each of the factored groups.
#' 
#' @export
convertFactor = function(x, name, ref=NULL) {
	
	tab = table(x)
	if(is.null(ref)) {
		reflevel = gsub(" ", "", names(which.max(tab)), fixed=T)
	} else {
		if(ref %in% names(tab)) {
			reflevel = gsub(" ", "", ref, fixed=T)
		}
	}
	newX = NULL; index=1
	for(j in 1:length(tab)){
		if(gsub(" ", "", names(tab)[j], fixed=T) != reflevel) {
			varName = paste(name, reflevel, gsub(" ", "", names(tab)[j], fixed=T), sep=".")
			newX = cbind(newX, ifelse(x==names(tab)[j], 1, 0))
			colnames(newX)[index] = varName; index=index+1
		}
	}
	
	return(data.frame(newX))

}
