###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Apr 10, 2015
###############################################################################


estSummary = function(estimator, fileloc, gloc=NULL, file, timepts){
	
	######################
	## LOADS EXCEL FILE ##
	######################
	parms = loadWorkbook(file=file)
	sheets = getSheets(parms)
	cs0 <- CellStyle(parms) + DataFormat("#,##0")
	cs2 <- CellStyle(parms) + DataFormat("#,##0.00")
	cs4 <- CellStyle(parms) + DataFormat("#,##0.0000")
	colStyle = list('1'=cs4, '2'=cs4, '3'=cs4, '4'=cs4, '5'=cs4, '6'=cs4, '7'=cs4, '8'=cs4, '9'=cs4, '10'=cs4, '11'=cs4, '12'=cs4)
	
	## ASSIGNS FILE NAME FITS SAVED AS ##
	if(grepl("unadjusted", tolower(estimator))) {
		filename = "null"
	} else if(grepl("glm", tolower(estimator))) {
		filename = "glm"
	} else if(grepl("sl", tolower(estimator))) {
		filename = "SL"
	}
	
	###################
	## g-FITS (LONG) ##
	###################
	if(grepl("long", tolower(estimator))) {
		if(grepl("glm", tolower(estimator))) {
			gA1.fit = readRDS(paste0(gloc, "/glm/disc90/A1.glm.RDS"))
			gA2.fit = readRDS(paste0(gloc, "/glm/disc90/A2.glm.RDS"))
			gC1.fit = readRDS(paste0(gloc, "/glm/disc90/C1.glm.RDS"))
			gC2.fit = readRDS(paste0(gloc, "/glm/disc90/C2.glm.RDS"))
			## COEF ##
			addDataFrame(x = summary(gA1.fit)$coef, sheet = sheets[["g-LONG"]], row.names = TRUE, col.names = TRUE, startColumn = 1, startRow=4, colStyle=colStyle)
			addDataFrame(x = summary(gA2.fit)$coef, sheet = sheets[["g-LONG"]], row.names = TRUE, col.names = TRUE, startColumn = 7, startRow=4, colStyle=colStyle)
			addDataFrame(x = summary(gC1.fit)$coef, sheet = sheets[["g-LONG"]], row.names = TRUE, col.names = TRUE, startColumn = 13, startRow=4, colStyle=colStyle)	
			addDataFrame(x = summary(gC2.fit)$coef, sheet = sheets[["g-LONG"]], row.names = TRUE, col.names = TRUE, startColumn = 19, startRow=4, colStyle=colStyle)				
		} else if(grepl("sl", tolower(estimator))) {
			gA1.fit = readRDS(paste0(gloc, "/SL/disc90/A1.SL.RDS"))
			gA2.fit = readRDS(paste0(gloc, "/SL/disc90/A2.SL.RDS"))
			gC1.fit = readRDS(paste0(gloc, "/SL/disc90/C1.SL.psi_00.RDS"))
			gC2.fit = readRDS(paste0(gloc, "/SL/disc90/C2.SL.RDS"))
			## CV-RISK ##
			addDataFrame(x = as.matrix(gA1.fit$cvRisk), sheet = sheets[["g-LONG"]], row.names = TRUE, col.names = FALSE, startColumn = 1, startRow=47, colStyle=colStyle)
			addDataFrame(x = as.matrix(gA2.fit$cvRisk), sheet = sheets[["g-LONG"]], row.names = TRUE, col.names = FALSE, startColumn = 7, startRow=47, colStyle=colStyle)
			addDataFrame(x = as.matrix(gC1.fit$cvRisk), sheet = sheets[["g-LONG"]], row.names = TRUE, col.names = FALSE, startColumn = 13, startRow=47, colStyle=colStyle)	
			addDataFrame(x = as.matrix(gC2.fit$cvRisk), sheet = sheets[["g-LONG"]], row.names = TRUE, col.names = FALSE, startColumn = 19, startRow=47, colStyle=colStyle)
			## COEF ##
			addDataFrame(x = as.matrix(gA1.fit$coef), sheet = sheets[["g-LONG"]], row.names = FALSE, col.names = FALSE, startColumn = 3, startRow=47, colStyle=colStyle)
			addDataFrame(x = as.matrix(gA2.fit$coef), sheet = sheets[["g-LONG"]], row.names = FALSE, col.names = FALSE, startColumn = 9, startRow=47, colStyle=colStyle)
			addDataFrame(x = as.matrix(gC1.fit$coef), sheet = sheets[["g-LONG"]], row.names = FALSE, col.names = FALSE, startColumn = 15, startRow=47, colStyle=colStyle)	
			addDataFrame(x = as.matrix(gC2.fit$coef), sheet = sheets[["g-LONG"]], row.names = FALSE, col.names = FALSE, startColumn = 21, startRow=47, colStyle=colStyle)			
		}
	}

	########################################
	## LOADS AND EXECUTES EACH TIME POINT ##
	########################################
	for(t in 1:timepts) {
		psi_00 = readRDS(paste0(fileloc, "/", estimator, "/psi_00.", filename, ".tmle-t", t, ".RDS"))
		psi_10 = readRDS(paste0(fileloc, "/", estimator, "/psi_10.", filename, ".tmle-t", t, ".RDS"))
		psi_11 = readRDS(paste0(fileloc, "/", estimator, "/psi_11.", filename, ".tmle-t", t, ".RDS"))
		if(!grepl("LONG", estimator)) {
			gcmp_00 = readRDS(paste0(fileloc, "/", estimator, "/psi_00.", filename, ".gcmp-t", t, ".RDS"))
			gcmp_10 = readRDS(paste0(fileloc, "/", estimator, "/psi_10.", filename, ".gcmp-t", t, ".RDS"))
			gcmp_11 = readRDS(paste0(fileloc, "/", estimator, "/psi_11.", filename, ".gcmp-t", t, ".RDS"))
		}

		## VARIABLE NAMES ##
		if(t==1 & (grepl("GLM_WIDE", estimator))) {
			## AVAILABILITY ##
			gA1names = names(coef(psi_00$fit$g[[paste0("avail.",t-1)]]))
			gA1names[16:length(gA1names)] = sub(paste0("\\.", t-1,"$"), ".t", gA1names[16:length(gA1names)])
			addDataFrame(x = gA1names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 1, startRow=4)
			gA1names = names(coef(psi_10$fit$g[[paste0("avail.",t-1)]]))
			gA1names[16:length(gA1names)] = sub(paste0("\\.", t-1,"$"), ".t", gA1names[16:length(gA1names)])
			addDataFrame(x = gA1names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 15, startRow=4)
			gA1names = names(coef(psi_11$fit$g[[paste0("avail.",t-1)]]))
			gA1names[16:length(gA1names)] = sub(paste0("\\.", t-1,"$"), ".t", gA1names[16:length(gA1names)])
			addDataFrame(x = gA1names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 29, startRow=4)			
			
			## ENROLLMENT ##
			gA2names = names(coef(psi_10$fit$g[[paste0("enroll.",t-1)]]))
			gA2names[16:length(gA2names)] = sub(paste0("\\.", t-1,"$"), ".t", gA2names[16:length(gA2names)])
			addDataFrame(x = gA2names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 15, startRow=38)
			gA2names = names(coef(psi_11$fit$g[[paste0("enroll.",t-1)]]))
			gA2names[16:length(gA2names)] = sub(paste0("\\.", t-1,"$"), ".t", gA2names[16:length(gA2names)])
			addDataFrame(x = gA2names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 29, startRow=38)
			
			## TRANSFER ##
			gC1names = names(coef(psi_00$fit$g[[paste0("transfer.",t-1)]]))
			gC1names[16:length(gC1names)] = sub(paste0("\\.", t-1,"$"), ".t", gC1names[16:length(gC1names)])
			addDataFrame(x = gC1names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 1, startRow=74)
			gC1names = names(coef(psi_10$fit$g[[paste0("transfer.",t-1)]]))
			gC1names[16:length(gC1names)] = sub(paste0("\\.", t-1,"$"), ".t", gC1names[16:length(gC1names)])
			addDataFrame(x = gC1names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 15, startRow=74)
			gC1names = names(coef(psi_11$fit$g[[paste0("transfer.",t-1)]]))
			gC1names[16:length(gC1names)] = sub(paste0("\\.", t-1,"$"), ".t", gC1names[16:length(gC1names)])
			addDataFrame(x = gC1names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 29, startRow=74)
			
			## EOFU ##
			gC2names = names(coef(psi_00$fit$g[[paste0("eofu.",t-1)]]))
			addDataFrame(x = gC2names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 1, startRow=109)
			gC2names = names(coef(psi_10$fit$g[[paste0("eofu.",t-1)]]))
			addDataFrame(x = gC2names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 15, startRow=109)
			gC2names = names(coef(psi_11$fit$g[[paste0("eofu.",t-1)]]))
			addDataFrame(x = gC2names, sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 29, startRow=109)
		}
		
		###########################################
		## g-FITS (WIDE), Q-FITS, and Qstar-FITS ##
		###########################################
		if(estimator!="Unadjusted") {
			if(grepl("GLM_WIDE", estimator)) {
				## PSI_00 g ##
				addDataFrame(x = coef(psi_00$fit$g[[paste0("avail.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 1+t, startRow=4, colStyle=colStyle)
				addDataFrame(x = coef(psi_00$fit$g[[paste0("transfer.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 1+t, startRow=74, colStyle=colStyle)
				addDataFrame(x = coef(psi_00$fit$g[[paste0("eofu.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 1+t, startRow=109, colStyle=colStyle)
				## PSI_10 g ##
				if(class(psi_10$fit$g[[paste0("avail.",t-1)]])[1] != "character") {
					addDataFrame(x = coef(psi_10$fit$g[[paste0("avail.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 15+t, startRow=4, colStyle=colStyle)
				}
				addDataFrame(x = coef(psi_10$fit$g[[paste0("enroll.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 15+t, startRow=38, colStyle=colStyle)
				addDataFrame(x = coef(psi_10$fit$g[[paste0("transfer.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 15+t, startRow=74, colStyle=colStyle)
				addDataFrame(x = coef(psi_10$fit$g[[paste0("eofu.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 15+t, startRow=109, colStyle=colStyle)
				## PSI_11 g ##
				if(class(psi_11$fit$g[[paste0("avail.",t-1)]])[1] != "character") {
					addDataFrame(x = coef(psi_11$fit$g[[paste0("avail.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 29+t, startRow=4, colStyle=colStyle)					
				}
				if(class(psi_11$fit$g[[paste0("enroll.",t-1)]])[1] != "character") {
					addDataFrame(x = coef(psi_11$fit$g[[paste0("enroll.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 29+t, startRow=38, colStyle=colStyle)					
				}
				addDataFrame(x = coef(psi_11$fit$g[[paste0("transfer.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 29+t, startRow=74, colStyle=colStyle)
				addDataFrame(x = coef(psi_11$fit$g[[paste0("eofu.",t-1)]]), sheet = sheets[["g-GLM_WIDE"]], row.names = FALSE, col.names = FALSE, startColumn = 29+t, startRow=109, colStyle=colStyle)
			} else if(grepl("SL_WIDE", estimator)) {
				cat("NEED TO SUMMARIZE SL G-FITS\n")
			}
			
			############
			## Q-FITS ##
			############
			## GLM ##
			if(grepl("GLM", estimator)) {
				Qnames = names(coef(psi_00$fit$Q[[1]]))
				Qnames[16:length(Qnames)] = sub(paste0("\\.0$"), ".t", Qnames[16:length(Qnames)])
				Qfits_00 = matrix(NA, ncol=t, nrow=length(coef(psi_00$fit$Q[[1]])), dimnames=list(Qnames, paste0("t",1:t)))
				Qfits_10 = matrix(NA, ncol=t, nrow=length(coef(psi_10$fit$Q[[1]])), dimnames=list(Qnames, paste0("t",1:t)))
				Qfits_11 = matrix(NA, ncol=t, nrow=length(coef(psi_11$fit$Q[[1]])), dimnames=list(Qnames, paste0("t",1:t)))
				for(i in 1:t) {
					## PSI_00 ##
					Qfits_00[,i] = coef(psi_00$fit$Q[[i]])
					## PSI_10 ##
					Qfits_10[,i] = coef(psi_10$fit$Q[[i]])
					## PSI_11 ##
					Qfits_11[,i] = coef(psi_11$fit$Q[[i]])
				}
				addDataFrame(x = Qfits_00, sheet = sheets[[paste0("Q-", estimator)]], row.names = TRUE, col.names = FALSE, startColumn = round(0.5*t^2+1.5*t-1), startRow=4, colStyle=colStyle)
				addDataFrame(x = Qfits_10, sheet = sheets[[paste0("Q-", estimator)]], row.names = TRUE, col.names = FALSE, startColumn = round(0.5*t^2+1.5*t-1), startRow=41, colStyle=colStyle)
				addDataFrame(x = Qfits_11, sheet = sheets[[paste0("Q-", estimator)]], row.names = TRUE, col.names = FALSE, startColumn = round(0.5*t^2+1.5*t-1), startRow=78, colStyle=colStyle)
			## SL ##
			} else if(grepl("SL", estimator)) {
				Qfits_00.coef = Qfits_10.coef = Qfits_11.coef = matrix(NA, ncol=t, nrow=length(psi_00$fit$Q[[1]]$coef), dimnames=list(names(psi_00$fit$Q[[1]]$coef), paste0("t",1:t)))
				Qfits_00.cvRisk = Qfits_10.cvRisk = Qfits_11.cvRisk = matrix(NA, ncol=t, nrow=length(psi_00$fit$Q[[1]]$cvRisk), dimnames=list(names(psi_00$fit$Q[[1]]$cvRisk), paste0("t",1:t)))
				for(i in 1:t) {
					## PSI_00 ##
					Qfits_00.coef[,i] = psi_00$fit$Q[[i]]$coef
					Qfits_00.cvRisk[,i] = psi_00$fit$Q[[i]]$cvRisk
					## PSI_10 ##
					Qfits_10.coef[,i] = psi_10$fit$Q[[i]]$coef
					Qfits_10.cvRisk[,i] = psi_10$fit$Q[[i]]$cvRisk
					## PSI_11 ##
					Qfits_11.coef[,i] = psi_11$fit$Q[[i]]$coef
					Qfits_11.cvRisk[,i] = psi_11$fit$Q[[i]]$cvRisk
					
				}
				addDataFrame(x = rownames(Qfits_00.coef), sheet = sheets[["Q-SL_LONG"]], row.names = FALSE, col.names = FALSE, startColumn = round(t^2+t-1), startRow=5)
				addDataFrame(x = rownames(Qfits_10.coef), sheet = sheets[["Q-SL_LONG"]], row.names = FALSE, col.names = FALSE, startColumn = round(t^2+t-1), startRow=42)
				addDataFrame(x = rownames(Qfits_11.coef), sheet = sheets[["Q-SL_LONG"]], row.names = FALSE, col.names = FALSE, startColumn = round(t^2+t-1), startRow=79)
				for(i in 1:t) {
					addDataFrame(x = Qfits_00.coef[,i], sheet = sheets[["Q-SL_LONG"]], row.names = FALSE, col.names = FALSE, startColumn = round(t^2+t+1+(i-1)*2), startRow=5, colStyle=colStyle)
					addDataFrame(x = Qfits_00.cvRisk[,i], sheet = sheets[["Q-SL_LONG"]], row.names = FALSE, col.names = FALSE, startColumn = round(t^2+t+(i-1)*2), startRow=5, colStyle=colStyle)
					addDataFrame(x = Qfits_10.coef[,i], sheet = sheets[["Q-SL_LONG"]], row.names = FALSE, col.names = FALSE, startColumn = round(t^2+t+1+(i-1)*2), startRow=42, colStyle=colStyle)
					addDataFrame(x = Qfits_10.cvRisk[,i], sheet = sheets[["Q-SL_LONG"]], row.names = FALSE, col.names = FALSE, startColumn = round(t^2+t+(i-1)*2), startRow=42, colStyle=colStyle)
					addDataFrame(x = Qfits_11.coef[,i], sheet = sheets[["Q-SL_LONG"]], row.names = FALSE, col.names = FALSE, startColumn = round(t^2+t+1+(i-1)*2), startRow=79, colStyle=colStyle)
					addDataFrame(x = Qfits_11.cvRisk[,i], sheet = sheets[["Q-SL_LONG"]], row.names = FALSE, col.names = FALSE, startColumn = round(t^2+t+(i-1)*2), startRow=79, colStyle=colStyle)
				}				
			}
			
			################
			## Qstar-FITS ##
			################
			Qstar_00.eps = Qstar_10.eps = Qstar_11.eps = matrix(NA, ncol=t, nrow=1)
			Qstar_00.con = Qstar_10.con = Qstar_11.con = matrix(NA, ncol=t, nrow=1)
			for(i in 1:t) {
				## PSI_00 ##
				if (class(psi_00$fit$Qstar[[i]])[1]=="glm") {
					Qstar_00.eps[,i] = coef(psi_00$fit$Qstar[[i]])
					Qstar_00.con[,i] = 1
				} else if (class(psi_00$fit$Qstar[[i]])[1]=="list") {
					Qstar_00.eps[,i] = psi_00$fit$Qstar[[i]]$par
					Qstar_00.con[,i] = 0
				} 
				## PSI_10 ##
				if (class(psi_10$fit$Qstar[[i]])[1]=="glm") {
					Qstar_10.eps[,i] = coef(psi_10$fit$Qstar[[i]])
					Qstar_10.con[,i] = 1
				} else if (class(psi_10$fit$Qstar[[i]])[1]=="list") {
					Qstar_10.eps[,i] = psi_10$fit$Qstar[[i]]$par
					Qstar_10.con[,i] = 0
				}
				## PSI_11 ##
				if (class(psi_11$fit$Qstar[[i]])[1]=="glm") {
					Qstar_11.eps[,i] = coef(psi_11$fit$Qstar[[i]])
					Qstar_11.con[,i] = 1
				} else if (class(psi_11$fit$Qstar[[i]])[1]=="list") {
					Qstar_11.eps[,i] = psi_11$fit$Qstar[[i]]$par
					Qstar_11.con[,i] = 0
				}
			}
			i.col = 3*(estimator=="GLM_WIDE") + 12*(estimator=="GLM_LONG") + 21*(estimator=="SL_WIDE") + 30*(estimator=="SL_LONG")
			addDataFrame(x = Qstar_00.eps, sheet = sheets[["Qstar-FITS"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=3+t, colStyle=colStyle)
			addDataFrame(x = Qstar_10.eps, sheet = sheets[["Qstar-FITS"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=12+t, colStyle=colStyle)
			addDataFrame(x = Qstar_11.eps, sheet = sheets[["Qstar-FITS"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=21+t, colStyle=colStyle)
			addDataFrame(x = Qstar_00.con, sheet = sheets[["Qstar-FITS"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=31+t, colStyle=colStyle)
			addDataFrame(x = Qstar_10.con, sheet = sheets[["Qstar-FITS"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=47+t, colStyle=colStyle)
			addDataFrame(x = Qstar_11.con, sheet = sheets[["Qstar-FITS"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=63+t, colStyle=colStyle)
			
		}

		##################
		## CUMULATIVE-G ##
		##################
		cumgStyle = list('1'=cs4, '2'=cs4, '3'=cs4, '4'=cs4, '5'=cs4, '6'=cs4, '7'=cs0)
		i.col = 2*(estimator=="Unadjusted") + 11*(estimator=="GLM_WIDE") + 20*(estimator=="GLM_LONG") + 29*(estimator=="SL_WIDE") + 38*(estimator=="SL_LONG")		
		addDataFrame(x = t(as.matrix(summary(psi_00$cum.g[,ncol(psi_00$cum.g)]))), sheet = sheets[["cum.g"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=3+t, colStyle=cumgStyle)
		addDataFrame(x = t(as.matrix(summary(psi_10$cum.g[,ncol(psi_10$cum.g)]))), sheet = sheets[["cum.g"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=19+t, colStyle=cumgStyle)
		addDataFrame(x = t(as.matrix(summary(psi_11$cum.g[,ncol(psi_11$cum.g)]))), sheet = sheets[["cum.g"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=35+t, colStyle=cumgStyle)		

		###############
		## ESTIMATES ##
		###############
		## psi_00 ##
		i.col = 3*(estimator=="Unadjusted") + 7*(estimator=="GLM_WIDE") + 11*(estimator=="GLM_LONG") + 15*(estimator=="SL_WIDE") + 19*(estimator=="SL_LONG")		
		addDataFrame(x = do.call('cbind', summary(psi_00, est="tmle")$treatment[1:3]), sheet = sheets[["Estimates"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=10+t, colStyle=colStyle)
		addDataFrame(x = do.call('cbind', summary(psi_00, est="iptw")$treatment[1:3]), sheet = sheets[["Estimates"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=26+t, colStyle=colStyle)
		## psi_10 ##
		i.col = 25*(estimator=="Unadjusted") + 29*(estimator=="GLM_WIDE") + 33*(estimator=="GLM_LONG") + 37*(estimator=="SL_WIDE") + 41*(estimator=="SL_LONG")		
		addDataFrame(x = do.call('cbind', summary(psi_10, est="tmle")$treatment[1:3]), sheet = sheets[["Estimates"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=10+t, colStyle=colStyle)
		addDataFrame(x = do.call('cbind', summary(psi_10, est="iptw")$treatment[1:3]), sheet = sheets[["Estimates"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=26+t, colStyle=colStyle)
		## psi_11 ##
		i.col = 47*(estimator=="Unadjusted") + 51*(estimator=="GLM_WIDE") + 55*(estimator=="GLM_LONG") + 59*(estimator=="SL_WIDE") + 63*(estimator=="SL_LONG")		
		addDataFrame(x = do.call('cbind', summary(psi_11, est="tmle")$treatment[1:3]), sheet = sheets[["Estimates"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=10+t, colStyle=colStyle)
		addDataFrame(x = do.call('cbind', summary(psi_11, est="iptw")$treatment[1:3]), sheet = sheets[["Estimates"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=26+t, colStyle=colStyle)
		## GCOMP ##
		if(!grepl("LONG", estimator)) {
			i.col = 3*(estimator=="Unadjusted") + 7*(estimator=="GLM_WIDE") + 11*(estimator=="GLM_LONG") + 15*(estimator=="SL_WIDE") + 19*(estimator=="SL_LONG")		
			addDataFrame(x = do.call('cbind', summary(gcmp_00, est="gcomp")$treatment[1:3]), sheet = sheets[["Estimates"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=42+t, colStyle=colStyle)
			i.col = 25*(estimator=="Unadjusted") + 29*(estimator=="GLM_WIDE") + 33*(estimator=="GLM_LONG") + 37*(estimator=="SL_WIDE") + 41*(estimator=="SL_LONG")		
			addDataFrame(x = do.call('cbind', summary(gcmp_10, est="gcomp")$treatment[1:3]), sheet = sheets[["Estimates"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=42+t, colStyle=colStyle)
			i.col = 47*(estimator=="Unadjusted") + 51*(estimator=="GLM_WIDE") + 55*(estimator=="GLM_LONG") + 59*(estimator=="SL_WIDE") + 63*(estimator=="SL_LONG")		
			addDataFrame(x = do.call('cbind', summary(gcmp_11, est="gcomp")$treatment[1:3]), sheet = sheets[["Estimates"]], row.names = FALSE, col.names = FALSE, startColumn = i.col, startRow=42+t, colStyle=colStyle)
		}

		################
		## TX EFFECTS ##
		################
		i.row = 4*(estimator=="Unadjusted") + 13*(estimator=="GLM_WIDE") + 22*(estimator=="GLM_LONG") + 31*(estimator=="SL_WIDE") + 40*(estimator=="SL_LONG")		
		# psi_10 vs psi_00 #
		tmle.10v00 = summary(psi_10, psi_00, estimator="tmle")$effect.measures
		iptw.10v00 = summary(psi_10, psi_00, estimator="iptw")$effect.measures
		# psi_11 vs psi_10 #
		tmle.11v10 = summary(psi_11, psi_10, estimator="tmle")$effect.measures
		iptw.11v10 = summary(psi_11, psi_10, estimator="iptw")$effect.measures
		# GCOMP #
		if(!grepl("LONG", estimator)) {
			gcmp.10v00 = summary(gcmp_10, gcmp_00, estimator="gcomp")$effect.measures
			gcmp.11v10 = summary(gcmp_11, gcmp_10, estimator="gcomp")$effect.measures
		}
		
		##############################
		## AVERAGE TREATMENT EFFECT ##
		##############################
		# TMLE (10v00) #
		addDataFrame(x = tmle.10v00$ATE$estimate, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 3, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = tmle.10v00$ATE$pvalue, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 4, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(tmle.10v00$ATE$CI,2), collapse=","), sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 5, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# IPTW (10v00) #
		addDataFrame(x = iptw.10v00$ATE$estimate, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 3+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = iptw.10v00$ATE$pvalue, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 4+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(iptw.10v00$ATE$CI,2), collapse=","), sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 5+13, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# GCMP (10v00) #
		if(!grepl("LONG", estimator)) {
			addDataFrame(x = gcmp.10v00$ATE$estimate, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 3+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = gcmp.10v00$ATE$pvalue, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 4+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = paste(round(gcmp.10v00$ATE$CI,2), collapse=","), sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 5+26, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		}
		# TMLE (11v10) #
		addDataFrame(x = tmle.11v10$ATE$estimate, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 3, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = tmle.11v10$ATE$pvalue, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 4, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(tmle.11v10$ATE$CI,2), collapse=","), sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 5, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# IPTW (11v10) #
		addDataFrame(x = iptw.11v10$ATE$estimate, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 3+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = iptw.11v10$ATE$pvalue, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 4+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(iptw.11v10$ATE$CI,2), collapse=","), sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 5+13, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# GCMP (11v10) #
		if(!grepl("LONG", estimator)) {
			addDataFrame(x = gcmp.11v10$ATE$estimate, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 3+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = gcmp.11v10$ATE$pvalue, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 4+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = paste(round(gcmp.11v10$ATE$CI,2), collapse=","), sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 5+26, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		}
		
		###################
		## RELATIVE RISK ##
		###################
		# TMLE (10v00) #
		addDataFrame(x = tmle.10v00$RR$estimate, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 7, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = tmle.10v00$RR$pvalue, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 8, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(tmle.10v00$RR$CI,2), collapse=","), sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 9, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# IPTW (10v00) #
		addDataFrame(x = iptw.10v00$RR$estimate, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 7+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = iptw.10v00$RR$pvalue, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 8+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(iptw.10v00$RR$CI,2), collapse=","), sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 9+13, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# GCMP (10v00) #
		if(!grepl("LONG", estimator)) {
			addDataFrame(x = gcmp.10v00$RR$estimate, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 7+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = gcmp.10v00$RR$pvalue, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 8+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = paste(round(gcmp.10v00$RR$CI,2), collapse=","), sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 9+26, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		}
		# TMLE (11v10) #
		addDataFrame(x = tmle.11v10$RR$estimate, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 7, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = tmle.11v10$RR$pvalue, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 8, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(tmle.11v10$RR$CI,2), collapse=","), sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 9, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# IPTW (11v10) #
		addDataFrame(x = iptw.11v10$RR$estimate, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 7+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = iptw.11v10$RR$pvalue, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 8+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(iptw.11v10$RR$CI,2), collapse=","), sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 9+13, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# GCMP (11v10) #
		if(!grepl("LONG", estimator)) {
			addDataFrame(x = gcmp.11v10$RR$estimate, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 7+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = gcmp.11v10$RR$pvalue, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 8+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = paste(round(gcmp.11v10$RR$CI,2), collapse=","), sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 9+26, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		}
		
		################
		## ODDS RATIO ##
		################
		# TMLE (10v00) #
		addDataFrame(x = tmle.10v00$OR$estimate, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 11, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = tmle.10v00$OR$pvalue, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 12, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(tmle.10v00$OR$CI,2), collapse=","), sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 13, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# IPTW (10v00) #
		addDataFrame(x = iptw.10v00$OR$estimate, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 11+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = iptw.10v00$OR$pvalue, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 12+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(iptw.10v00$OR$CI,2), collapse=","), sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 13+13, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# GCMP (10v00) #
		if(!grepl("LONG", estimator)) {
			addDataFrame(x = gcmp.10v00$OR$estimate, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 11+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = gcmp.10v00$OR$pvalue, sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 12+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = paste(round(gcmp.10v00$OR$CI,2), collapse=","), sheet = sheets[["TX-Effect-10v00"]], row.names = FALSE, col.names = FALSE, startColumn = 13+26, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		}
		# TMLE (11v10) #
		addDataFrame(x = tmle.11v10$OR$estimate, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 11, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = tmle.11v10$OR$pvalue, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 12, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(tmle.11v10$OR$CI,2), collapse=","), sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 13, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# IPTW (11v10) #
		addDataFrame(x = iptw.11v10$OR$estimate, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 11+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = iptw.11v10$OR$pvalue, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 12+13, startRow=i.row+t, colStyle=list('1'=cs2))
		addDataFrame(x = paste(round(iptw.11v10$OR$CI,2), collapse=","), sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 13+13, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		# GCMP (11v10) #
		if(!grepl("LONG", estimator)) {
			addDataFrame(x = gcmp.11v10$OR$estimate, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 11+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = gcmp.11v10$OR$pvalue, sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 12+26, startRow=i.row+t, colStyle=list('1'=cs2))
			addDataFrame(x = paste(round(gcmp.11v10$OR$CI,2), collapse=","), sheet = sheets[["TX-Effect-11v10"]], row.names = FALSE, col.names = FALSE, startColumn = 13+26, startRow=i.row+t, colStyle=list('1'=CellStyle(parms) + Alignment(h="ALIGN_CENTER", wrapText=FALSE)))
		}
		
	}
	
	saveWorkbook(parms, file=file)	
	
}
