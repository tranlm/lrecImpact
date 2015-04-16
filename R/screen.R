###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 24, 2015
###############################################################################
    
#' export
screen = function(X, Y, id, time, df, family) {
	if(is.null(X)) {
		output <- geeglm(Y ~ ns(time,df), id=id, family=family, corstr="independence")
		results = summary(output)$coefficients[-c(1),]
	} else {
		output <- geeglm(Y ~ ns(time,df) + ., data=X, id=id, family=family, corstr="independence")
		results = summary(output)$coefficients[-c(1:(df+1)),]
	}
	return(results)
}


