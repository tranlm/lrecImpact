###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 20, 2015
###############################################################################


#' Summary of raw data
#' 
#' \code{rawDataSummary} Summarizes raw data and exports data to an Excel results file. 
#' 
#' File needs to be created beforehand with a sheet with the name "Raw data".
#' 
#' @param file Location of the excel file to export summaries to.  
#' @return \code{rawDataSummary} returns nothing.
#' 
#' @export
rawDataSummary = function(file) {
	# Loads excel workbook
	parms = loadWorkbook(file=file)
	sheets = getSheets(parms)
	cs1 <- CellStyle(parms) + Alignment(h="ALIGN_CENTER")
	colStyle = list(
			'1'=cs1, '2'=cs1, '3'=cs1, '4'=cs1, '5'=cs1, '6'=cs1, '7'=cs1, '8'=cs1, '9'=cs1, '10'=cs1, 
			'11'=cs1, '12'=cs1, '13'=cs1, '14'=cs1, '15'=cs1, '16'=cs1, '17'=cs1, '18'=cs1, '19'=cs1, '20'=cs1, 
			'21'=cs1, '22'=cs1, '23'=cs1, '24'=cs1, '25'=cs1, '26'=cs1, '27'=cs1, '28'=cs1)
	
	# MPLRECD #
	counts_1 = c(ids=length(unique(mplrecd$patient_id)), obs=nrow(mplrecd), elig=sum(!is.na(mplrecd$eligdate)), avail=NA, enroll=sum(mplrecd$Express))
	counts_2 = c(males=sum(mplrecd$male), ECenrolb4elig=sum(mplrecd$ECenrolb4elig, na.rm=T), deaths=sum(mplrecd$Death), ltfu=sum(mplrecd$lost2fup), ltfu_notdead=sum(mplrecd$lost2fup_notdead), ltfu_6mo=sum(mplrecd$LTFU6mos))
	counts = c(counts_1, NA, counts_1)
	addDataFrame(x = matrix(counts), sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 2, startRow=3)
	mplrecd.cont.vars = c("eligdate", "exprstart", "cd4b", "cd4dateb", "cd4arv", "cd4e", "enroldate", "dob", "arvstart", "enrollage", "dod")
	mplrecd.cat.vars = c("Express", "neverelig", "whoatarvstart", "whoECmax", "pibasedstart", "pibasedMR", "pibasedelig", "tbtx_arv", "Death", "lost2fup", "male", "urbanlast", "urbanelig", "clintypelast", "clintypeelig", "lost2fup_notdead", "LTFU6mos", "ECenrolb4elig")
	cat("Variables excluding:\n"); setdiff(names(mplrecd), c(mplrecd.cont.vars, mplrecd.cat.vars))
	for(i in 1:length(mplrecd.cont.vars)){
		if(class(mplrecd[,mplrecd.cont.vars[i]])=="Date") {
			temp = data.frame(t(summary(mplrecd[,mplrecd.cont.vars[i]])))
			temp = dcast(Var1~Var2,data=temp)[,-1]
			for(j in 1:ncol(temp)) temp[,j] = as.Date(temp[,j], origin="1970-01-01") 
		} else {
			temp = summary(mplrecd[,mplrecd.cont.vars[i]])
			temp = matrix(temp, nrow=1)
		}
		addDataFrame(x = mplrecd.cont.vars[i], sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 4, startRow=3+i)
		addDataFrame(x = temp, sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 5, startRow=3+i)
		temp = NULL
	}
	for(i in 1:length(mplrecd.cat.vars)){
		t1 = table(mplrecd[,mplrecd.cat.vars[i]], useNA='always')
		temp = data.frame(matrix(t1, nrow=1))
		names(temp) = names(t1)
		addDataFrame(x = mplrecd.cat.vars[i], sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 4, startRow=3*i+14)
		addDataFrame(x = temp, sheet = sheets[["Raw data"]], row.names = FALSE, col.names = TRUE, startColumn = 5, startRow=3*i+15, colStyle=colStyle)
		t1 = temp = NULL
	}
	
	# MPLRECL #
	counts_1 = c(ids=length(unique(mplrecl$patient_id)), obs=nrow(mplrecl), elig=sum(with(mplrecl, tapply(elig, patient_id, function(x) max(x, na.rm=T)))), avail=sum(with(mplrecl, tapply(ecavail, patient_id, function(x) max(x, na.rm=T)))), enroll=sum(with(mplrecl, tapply(ec, patient_id, function(x) max(x, na.rm=T)))))
	counts_2 = c(pregnant=sum(with(mplrecl, tapply(pregnant, patient_id, function(x) max(x, na.rm=T)))==1), tbtx=sum(with(mplrecl, tapply(tbtx, patient_id, function(x) max(x, na.rm=T)))==1), noelig=sum(with(mplrecl, tapply(elig, patient_id, function(x) min(x, na.rm=T)))==0))
	counts = c(counts_1, NA, counts_2)
	addDataFrame(x = matrix(counts), sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 14, startRow=3)
	temp = merge(mplrecl, mplrecd[,c("patient_id", "eligdate", "exprstart")], by="patient_id", all.x=T)
	temp = subset(temp, apptdate>=eligdate & (apptdate<exprstart | is.na(exprstart)))
	become.inelig.v2 = tapply(temp$elig, temp$patient_id, function(x) min(x, na.rm=T))
	addDataFrame(x = sum(become.inelig.v2==0), sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 14, startRow=12)
	mplrecl.cont.vars = c("apptdate", "cd4", "cd4date", "age")
	mplrecl.cat.vars = c("elig", "pregnant", "onARV", "tbtx", "stage34", "arvadhere", "ec", "ecavail")
	for(i in 1:length(mplrecl.cont.vars)) {
		if(class(mplrecl[,mplrecl.cont.vars[i]])=="Date") {
			temp = data.frame(t(summary(mplrecl[,mplrecl.cont.vars[i]])))
			temp = dcast(Var1~Var2,data=temp)[,-1]
			for(j in 1:ncol(temp)) temp[,j] = as.Date(temp[,j], origin="1970-01-01") 
		} else {
			temp = summary(mplrecl[,mplrecl.cont.vars[i]])
			temp = matrix(temp, nrow=1)
		}
		addDataFrame(x = mplrecl.cont.vars[i], sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 16, startRow=3+i)
		addDataFrame(x = temp, sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 17, startRow=3+i)
		temp = NULL
	}
	for(i in 1:length(mplrecl.cat.vars)){
		t1 = table(mplrecl[,mplrecl.cat.vars[i]], useNA='always')
		temp = data.frame(matrix(t1, nrow=1))
		names(temp) = names(t1)
		addDataFrame(x = mplrecl.cat.vars[i], sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 16, startRow=3*i+14)
		addDataFrame(x = temp, sheet = sheets[["Raw data"]], row.names = FALSE, col.names = TRUE, startColumn = 17, startRow=3*i+15, colStyle=colStyle)
		t1 = temp = NULL
	}
	
	# MPCLINIC #
	counts = nrow(mpclinic)
	addDataFrame(x = matrix(counts), sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 26, startRow=3)
	mpclinic.cat.vars = c("clintype", "urbanclin")
	for(i in 1:length(mpclinic.cat.vars)){
		t1 = table(mpclinic[,mpclinic.cat.vars[i]], useNA='always')
		temp = data.frame(matrix(t1, nrow=1))
		names(temp) = names(t1)
		addDataFrame(x = mpclinic.cat.vars[i], sheet = sheets[["Raw data"]], row.names = FALSE, col.names = FALSE, startColumn = 28, startRow=3*i+14)
		addDataFrame(x = temp, sheet = sheets[["Raw data"]], row.names = FALSE, col.names = TRUE, startColumn = 29, startRow=3*i+15, colStyle=colStyle)
		t1 = temp = NULL
	}
	saveWorkbook(parms, file=file)
	invisible(NULL)
}

