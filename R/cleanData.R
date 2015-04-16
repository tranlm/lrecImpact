###############################################################################
# Description: Function to clean the LREC data sets
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 13, 2015
###############################################################################

#' Clean data
#' 
#' \code{cleanData} will clean and organize a data frame such that it is ready to be discretized into person-time. Its primary purpose is to make sure there are no inconsistencies with the data and to impute missing values.
#' 
#' This function can take data with both time-varying and baseline covariates. If \code{locf=TRUE}, then missing values will be imputed in a 'last observation carried forward' approach. Furthermore, if \code{impute=TRUE}, then remaining observations that have no value to carry forward will be imputed with the median (if continuous) or mode (if character or factor). If the data includes only baseline covariates, then only the 
#' 
#' @param data A \code{data.frame} object, with the ID variable in the first column.
#' @param time Name or index of time variable (e.g. calendar date) for longitudinal data in long form. If \code{NULL}, then data is assumed to include only baseline covariates and will only impute  
#' @param locf If \code{TRUE}, all missing values will be imputed in a last observation carried forward fashion.
#' @param impute If \code{TRUE}, all remaining missing values will be imputed using either the median (if continuous) or mode (if character or factor). This option cannot be \code{TRUE} if {locf=FALSE}.
#' @param baseline A vector of baseline variable names or indicies.
#' @param ignore Vector of covariates ignoring in the imputations
#' @return \code{clean} returns an object of class "data.frame" with the data ordered by ID, time, and ordered column wise alphabetically. 
#' @export 'cleanData'
cleanData = function(data, time=NULL, locf=TRUE, impute=TRUE, baseline=NULL, ignore=NULL) {
	if(!locf & impute) stop("'locf' cannot be FALSE if 'impute' is true")
	
}
