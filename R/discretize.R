###############################################################################
# Description: Function to discretize data
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 21, 2015
###############################################################################


#' Discretizing longitudinal data
#' 
#' \code{discretize} breaks longitudinal data into equivalent time intervals using the last observed visit leading up to each new interval.
#' 
#' In addition to breaking longitudinal data into equally spaced time intervals, the code will return a data frame with the number of visits observed in the interval leading up to the current interval and within the current interval, the most recent visit observed leading up to the current interval, and the next visit expected.
#' 
#' @param dataset Longitudinal data set to be discretized in \code{data.frame} format.
#' @param date Covariate name for date variable.
#' @param startdate Covariate name for the date started from for discretization.
#' @param stopdate Covariate name for the date ending from for discretization.
#' @param outcome Covariate name for whether an event or censoring occurs. Must be one of the following: death, ltfu, eofu, transfer
#' @param range Length in days of each interval
#' @return \code{discretize} returns a data.frame with the discretized data and variables corresponding to the visits conducted.
#' 
#' @export
discretize = function(dataset, date, startdate, stopdate, outcome, range){
	
	#Creates new appt dates
	if(length(unique(dataset[,startdate]))>1 | length(unique(dataset[,stopdate]))>1 | length(unique(dataset[,outcome]))>1) {
		stop("More than one unique start date, stop date, or outcome.")
	} else if(!all(unique(dataset[,outcome]) %in% c("death", "transfer", "ltfu", "eofu"))) {
		stop("Outcome has to be one of the following: death, transfer, ltfu, or eofu")
	} else {
		startat = dataset[1,startdate]
		stopat = dataset[1,stopdate]
		status = dataset[1,outcome]
		new <- data.frame(date=seq(from=startat, to=stopat, by=range))
		new$startdate = startat
		new$stopdate = stopat
		new$outcome = status
	}
	
	#Time ordering: Death-->LTFU-->Transfer-->EOFU
	new$death = new$ltfu = new$transfer = new$eofu = 0
	if(status=="death") {
		new$death[nrow(new)] = 1
	} else if(status=="ltfu") {
		new$ltfu[nrow(new)] = 1
	} else if(status=="transfer") {
		new$transfer[nrow(new)] = 1
	} else if(status=="eofu") {
		new$eofu[nrow(new)] = 1
	} else {
		new$death = new$ltfu = new$transfer = new$eofu = NA
	}
	
	#Declares new variables
	for(i in 1:ncol(dataset)){
		if(!(names(dataset[i]) %in% c(date, startdate, stopdate, outcome))){
			new = cbind(new, rep(NA, nrow(new)))
			names(new) = c(names(new)[-ncol(new)], names(dataset[i]))
		}
	}
	new$visitCountPrev = new$visitCountCurr = new$visitNext = new$visitPrev = rep(NA, nrow(new))
	
	for(i in 1:nrow(new)){
		#Counts number of visits in each interval
		new$visitCountPrev[i] <- length(which(dataset[,date] < new$date[i] & dataset[,date] >= new$date[i]-range))
		new$visitCountCurr[i] <- length(which(dataset[,date] >= new$date[i] & dataset[,date] < new$date[i]+range))
		#Gets most recent values prior to interval date
		j <- max(which(dataset[,date] <= new$date[i] & stopat >= new$date[i]))
		new$visitPrev[i] <- dataset[,date][j]
		new$visitNext[i] <- dataset[,date][j+1]
		#Carries over all time-varying variables
		for(k in 1:ncol(dataset)){
			if(!(names(dataset[k]) %in% c(date, startdate, stopdate, outcome))) new[i,which(names(new)==names(dataset[k]))] = dataset[j,k]
		}
	}
	new$visitPrev = as.Date(new$visitPrev, origin="1970-01-01")
	new$visitNext = as.Date(new$visitNext, origin="1970-01-01")
	
	return(new)
}



