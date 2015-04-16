###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 22, 2015
###############################################################################


#' Discretized Data Summary
#' 
#' \code{discDataSummary} Summarizes the discretized data and outputs the summaries to Excel.
#' 
#' @param data Data set to be summarized
#' @param file Location of excel output file.
#' @return \code{discDataSummary} returns nothing.
#' 
#' @export
discDataSummary = function(data, file) {
	
	# EXCEL OUTPUT #
	parms = loadWorkbook(file=file)
	sheets = getSheets(parms)
	cs1 <- CellStyle(parms) + Alignment(h="ALIGN_CENTER")
	colStyle = list(
			'1'=cs1, '2'=cs1, '3'=cs1, '4'=cs1, '5'=cs1, '6'=cs1, '7'=cs1, '8'=cs1, '9'=cs1, '10'=cs1, 
			'11'=cs1, '12'=cs1, '13'=cs1, '14'=cs1, '15'=cs1, '16'=cs1, '17'=cs1, '18'=cs1, '19'=cs1, '20'=cs1, 
			'21'=cs1, '22'=cs1, '23'=cs1, '24'=cs1, '25'=cs1, '26'=cs1, '27'=cs1, '28'=cs1)
	
	# Simple counts
	counts = c(ids=length(unique(data$patient_id)), obs=nrow(data), elig=sum(with(data, tapply(elig, patient_id, function(x) max(x, na.rm=T)))), avail=sum(with(data, tapply(avail, patient_id, function(x) max(x, na.rm=T)))), enroll=sum(with(data, tapply(enroll, patient_id, function(x) max(x, na.rm=T)))), deaths = sum(data$death), ltfu=sum(data$ltfu))
	addDataFrame(x = matrix(counts), sheet = sheets[["Discretized data"]], row.names = FALSE, col.names = FALSE, startColumn = 2, startRow=3)

	# Summaries	
	n = subset(data, rank==0)
	cont.vars = c("ageb", "availRank", "cd4_locf.p100", "cd4_nadir.p100", "cd4_zenith.p100", "cd4arv.p100", "date", "daysfromenroll.p180", "enrollRank", "exprstart", "missvispreEC", "rank", "rankSinceAvail", "rankSinceEnroll", "startdate", "stopdate", "visitCountCurr", "visitCountPrev")
	cat.vars = c("arvadhere", "avail", "availImm", "availPrev", "cd4v_locf.cat.ge500.lt200", "cd4v_locf.cat.ge500.200to350", "cd4v_locf.cat.ge500.350to500", "clintype.ruralhealthcenter..sub.districthospital", "clintype.ruralhealthcenter.refhospital", "clintypeelig.ruralhealthcenter..sub.districthospital", "clintypeelig.ruralhealthcenter.refhospital", "dead.ltfu", "death", "elig", "enroll", "enrollImm", "enrollPrev", "eofu", "ltfu", "male", "onARV", "outcome", "pibasedMR", "pregnant_locf.l", "prevSeen", "stage34_locf", "tbtx_arv", "tbtx_locf.l", "transfer", "urbanclin", "urbanelig", "whoECmax.1.2", "whoECmax.1.3", "whoECmax.1.4", "whoatarvstart.1.2", "whoatarvstart.1.3", "whoatarvstart.1.4")
	base.vars = c("ageb", "availImm", "availRank", "cd4arv.p100", "clintypeelig.ruralhealthcenter..sub.districthospital", "clintypeelig.ruralhealthcenter.refhospital", "enrollImm", "enrollRank", "exprstart", "male", "missvispreEC", "pibasedMR", "startdate", "stopdate", "tbtx_arv", "urbanelig", "whoECmax.1.2", "whoECmax.1.3", "whoECmax.1.4", "whoatarvstart.1.2", "whoatarvstart.1.3", "whoatarvstart.1.4")
	time.vars = c("arvadhere", "avail", "availPrev", "cd4_locf.p100", "cd4_nadir.p100", "cd4_zenith.p100", "cd4v_locf.cat.ge500.lt200", "cd4v_locf.cat.ge500.200to350", "cd4v_locf.cat.ge500.350to500", "clintype.ruralhealthcenter..sub.districthospital", "clintype.ruralhealthcenter.refhospital", "date", "daysfromenroll.p180", "dead.ltfu", "death", "elig", "enroll", "enrollPrev", "eofu", "ltfu", "onARV", "pregnant_locf.l", "prevSeen", "rank", "rankSinceAvail", "rankSinceEnroll", "stage34_locf", "tbtx_locf.l", "transfer", "urbanclin", "visitCountCurr", "visitCountPrev", "visitNext", "visitPrev")

	# Continuous baseline covariates
	for(i in 1:length(intersect(cont.vars, base.vars))){
		if(class(data[,intersect(cont.vars, base.vars)[i]])=="Date") {
			temp = data.frame(t(summary(data[,intersect(cont.vars, base.vars)[i]])))
			temp = dcast(Var1~Var2,data=temp)[,-1]
			for(j in 1:ncol(temp)) temp[,j] = as.Date(temp[,j], origin="1970-01-01") 
		} else {
			temp = summary(data[,intersect(cont.vars, base.vars)[i]])
			temp = matrix(temp, nrow=1)
		}
		addDataFrame(x = intersect(cont.vars, base.vars)[i], sheet = sheets[["Discretized data"]], row.names = FALSE, col.names = FALSE, startColumn = 4, startRow=3+i)
		addDataFrame(x = temp, sheet = sheets[["Discretized data"]], row.names = FALSE, col.names = FALSE, startColumn = 5, startRow=3+i)
		temp = NULL
	}
	
	# Continuous time-varying covariates
	for(i in 1:length(intersect(cont.vars, time.vars))){
		if(class(data[,intersect(cont.vars, time.vars)[i]])=="Date") {
			temp = data.frame(t(summary(data[,intersect(cont.vars, time.vars)[i]])))
			temp = dcast(Var1~Var2,data=temp)[,-1]
			for(j in 1:ncol(temp)) temp[,j] = as.Date(temp[,j], origin="1970-01-01") 
		} else {
			temp = summary(data[,intersect(cont.vars, time.vars)[i]])
			temp = matrix(temp, nrow=1)
		}
		addDataFrame(x = intersect(cont.vars, time.vars)[i], sheet = sheets[["Discretized data"]], row.names = FALSE, col.names = FALSE, startColumn = 13, startRow=3+i)
		addDataFrame(x = temp, sheet = sheets[["Discretized data"]], row.names = FALSE, col.names = FALSE, startColumn = 14, startRow=3+i)
		temp = NULL
	}
	
	# Categorical baseline covariates
	for(i in 1:length(intersect(cat.vars, base.vars))){
		t1 = table(data[,intersect(cat.vars, base.vars)[i]], useNA='always')
		temp = data.frame(matrix(t1, nrow=1))
		names(temp) = names(t1)
		addDataFrame(x = intersect(cat.vars, base.vars)[i], sheet = sheets[["Discretized data"]], row.names = FALSE, col.names = FALSE, startColumn = 4, startRow=3*i+14)
		addDataFrame(x = temp, sheet = sheets[["Discretized data"]], row.names = FALSE, col.names = TRUE, startColumn = 5, startRow=3*i+15, colStyle=colStyle)
		t1 = temp = NULL
	}
	
	# Categorical time-varying covariates
	for(i in 1:length(intersect(cat.vars, time.vars))){
		t1 = table(data[,intersect(cat.vars, time.vars)[i]], useNA='always')
		temp = data.frame(matrix(t1, nrow=1))
		names(temp) = names(t1)
		addDataFrame(x = intersect(cat.vars, time.vars)[i], sheet = sheets[["Discretized data"]], row.names = FALSE, col.names = FALSE, startColumn = 13, startRow=3*i+14)
		addDataFrame(x = temp, sheet = sheets[["Discretized data"]], row.names = FALSE, col.names = TRUE, startColumn = 14, startRow=3*i+15, colStyle=colStyle)
		t1 = temp = NULL
	}

	saveWorkbook(parms, file=file)
	invisible(NULL)
	
}
