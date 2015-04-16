###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 22, 2015
###############################################################################


countSummary = function(data, file) {

	# EXCEL OUTPUT #
	parms = loadWorkbook(file=file)
	sheets = getSheets(parms)
	cs1 <- CellStyle(parms) + Alignment(h="ALIGN_CENTER")
	colStyle = list('1'=cs1, '2'=cs1, '3'=cs1, '4'=cs1, '5'=cs1, '6'=cs1, '7'=cs1, '8'=cs1, '9'=cs1)
	
	final_t = 13
	gA1.data = subset(data, availPrev==0)
	gA2.data = subset(data, enrollPrev==0)
	time.summary = matrix(nrow=final_t+1, ncol=9, dimnames=list(paste0("t", 0:final_t), c("Remaining", "Dead", "LTFU", "RemainingAvailable", "Available", "RemainingEnroll", "Enroll", "Transfer", "EOFU")))
	for(t in 0:final_t){
		remaining = subset(data, rank==t)
		remain = nrow(remaining)
		bdead = sum(remaining$death==1)
		bltfu = sum(remaining$ltfu==1)
		notAvail = nrow(subset(remaining, availPrev==0))
		bavail = sum(subset(gA1.data, rank==t)$avail==1, na.rm=T)
		notEnroll = nrow(subset(remaining, avail==1 & enrollPrev==0))
		benroll = sum(subset(gA2.data, rank==t)$enroll==1, na.rm=T)
		btransfer = sum(remaining$transfer==1, na.rm=T)
		beofu = sum(remaining$eofu==1, na.rm=T)
		time.summary[t+1,] = c(remain, bdead, bltfu, notAvail, bavail, notEnroll, benroll, btransfer, beofu)
	}
	addDataFrame(x = time.summary, sheet = sheets[["Y,A,C Counts"]], row.names = FALSE, col.names = FALSE, startColumn = 2, startRow=4, colStyle=colStyle)
	
	time.all = time.avail.never = time.avail.imm = time.enroll.imm = matrix(nrow=final_t+1, ncol=2, dimnames=list(NULL, c("Remaining", "Events")))
	avail.never = subset(data, availPrev==0)
	avail.imm = subset(data, availImm==1)
	enroll.imm = subset(data, enrollImm==1)
	for(t in 0:final_t) {
		t.all = subset(data, rank==t)
		time.all[t+1,"Remaining"] = nrow(t.all); time.all[t+1,"Events"] = sum(t.all$dead.ltfu)
		t.avail.never = subset(avail.never, rank==t)
		time.avail.never[t+1,"Remaining"] = nrow(t.avail.never); time.avail.never[t+1,"Events"] = sum(t.avail.never$dead.ltfu)
		t.avail.imm = subset(avail.imm, rank==t)
		time.avail.imm[t+1,"Remaining"] = nrow(t.avail.imm); time.avail.imm[t+1,"Events"] = sum(t.avail.imm$dead.ltfu)
		t.enroll.imm = subset(enroll.imm, rank==t)
		time.enroll.imm[t+1,"Remaining"] = nrow(t.enroll.imm); time.enroll.imm[t+1,"Events"] = sum(t.enroll.imm$dead.ltfu)
	}
	addDataFrame(x = time.all, sheet = sheets[["Y,A,C Counts"]], row.names = FALSE, col.names = FALSE, startColumn = 13, startRow=4, colStyle=colStyle)
	addDataFrame(x = time.avail.never, sheet = sheets[["Y,A,C Counts"]], row.names = FALSE, col.names = FALSE, startColumn = 15, startRow=4, colStyle=colStyle)
	addDataFrame(x = time.avail.imm, sheet = sheets[["Y,A,C Counts"]], row.names = FALSE, col.names = FALSE, startColumn = 17, startRow=4, colStyle=colStyle)
	addDataFrame(x = time.enroll.imm, sheet = sheets[["Y,A,C Counts"]], row.names = FALSE, col.names = FALSE, startColumn = 19, startRow=4, colStyle=colStyle)
	
	saveWorkbook(parms, file=file)
	invisible(NULL)
}



