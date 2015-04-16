###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 24, 2015
###############################################################################

#' export
getOR = function(x) {
	OR = exp(x$Estimate)
	l.ci = OR-qnorm(.975)*x$Std.err
	u.ci = OR+qnorm(.975)*x$Std.err
	return(cbind(OR, l.ci, u.ci))
}
