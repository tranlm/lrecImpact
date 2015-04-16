###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 24, 2015
###############################################################################

#' export
screenSummary = function(results, file) {
	
	parms = loadWorkbook(file=file)
	sheets = getSheets(parms)
	cs1 <- CellStyle(parms) + DataFormat("#,##0.000") + Alignment(h="ALIGN_CENTER")
	cs2 <- CellStyle(parms) + DataFormat("#,##0.00") + Alignment(h="ALIGN_CENTER")
	colStyle = list('1'=cs1, '2'=cs1, '3'=cs1, '4'=cs1, '5'=cs2, '6'=cs2, '7'=cs2, '8'=cs2)

	addDataFrame(x = results$screen.death.all, sheet = sheets[["Screening"]], row.names = TRUE, col.names = TRUE, startColumn = 1, startRow=2, colStyle=colStyle)
	addDataFrame(x = results$screen.ltfu.all, sheet = sheets[["Screening"]], row.names = FALSE, col.names = TRUE, startColumn = 11, startRow=2, colStyle=colStyle)
	addDataFrame(x = results$screen.dead.ltfu.all, sheet = sheets[["Screening"]], row.names = FALSE, col.names = TRUE, startColumn = 20, startRow=2, colStyle=colStyle)
	addDataFrame(x = results$screen.avail.all, sheet = sheets[["Screening"]], row.names = FALSE, col.names = TRUE, startColumn = 29, startRow=2, colStyle=colStyle)
	addDataFrame(x = results$screen.enroll.all, sheet = sheets[["Screening"]], row.names = FALSE, col.names = TRUE, startColumn = 38, startRow=2, colStyle=colStyle)
	addDataFrame(x = results$screen.transfer.all, sheet = sheets[["Screening"]], row.names = FALSE, col.names = TRUE, startColumn = 47, startRow=2, colStyle=colStyle)
	addDataFrame(x = results$screen.eofu.all, sheet = sheets[["Screening"]], row.names = FALSE, col.names = TRUE, startColumn = 56, startRow=2, colStyle=colStyle)
	
	saveWorkbook(parms, file=file)
	
}

