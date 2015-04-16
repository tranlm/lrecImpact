###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 22, 2015
###############################################################################


readSAS = function(.ddir) {
	
	require(sas7bdat)
	sas.origin <- as.Date("1960-01-01")
	
	## MPCLINIC ##
	## Site-Level data
	mpclinic = read.sas7bdat(file=paste0(.ddir, "mpclinic.sas7bdat"))	
	mpclinic = rename(mpclinic, c(clinic="CLINIC"))
	clintype = ifelse(mpclinic$clintype==1,'ref hospital', ifelse(mpclinic$clintype==2, '(sub)district hospital', ifelse(mpclinic$clintype==3,'rural health center', NA))) 
	mpclinic$clintype = clintype
	mpclinic$clinic_start_date = as.Date(mpclinic$clinic_start_date, origin=sas.origin)
	
	## MPLRECD ##
	## Patient-Level baseline data
	mplrecd = read.sas7bdat(file=paste0(.ddir, "mplrecd.sas7bdat"))		
	mplrecd$eligdate = as.Date(mplrecd$eligdate, origin=sas.origin)
	mplrecd$exprstart = as.Date(mplrecd$exprstart, origin=sas.origin)
	mplrecd$cd4dateb = as.Date(mplrecd$cd4dateb, origin=sas.origin)
	mplrecd$enroldate = as.Date(mplrecd$enroldate, origin=sas.origin)
	mplrecd$dob = as.Date(mplrecd$dob, origin=sas.origin)
	mplrecd$dod = as.Date(mplrecd$dod, origin=sas.origin)
	mplrecd$arvstart = as.Date(mplrecd$arvstart, origin=sas.origin)
	clintype.2 = ifelse(mplrecd$clintypelast==1,'ref hospital', ifelse(mplrecd$clintypelast==2, '(sub)district hospital', ifelse(mplrecd$clintypelast==3,'rural health center', NA))) 
	mplrecd$clintypelast = clintype.2
	clintype.3 = ifelse(mplrecd$clintypeelig==1,'ref hospital', ifelse(mplrecd$clintypeelig==2, '(sub)district hospital', ifelse(mplrecd$clintypeelig==3,'rural health center', NA))) 
	mplrecd$clintypeelig = clintype.3
	mplrecd$patient_id = as.character(mplrecd$patient_id)
	mplrecd$urbanlast = mplrecd$clintypelast = NULL	
	mplrecd = mplrecd[order(mplrecd$patient_id),]
	
	## MPLRECL ##
	#Patient-level longitudinal data
	mplrecl = read.sas7bdat(file=paste0(.ddir, "mplrecl.sas7bdat"))
	mplrecl$apptdate = as.Date(mplrecl$apptdate, origin=sas.origin)
	mplrecl$cd4date = as.Date(mplrecl$cd4date, origin=sas.origin)
	mplrecl = mplrecl[order(mplrecl$patient_id, mplrecl$apptdate),]
	mplrecl$rank = ave(rep(NA, nrow(mplrecl)), mplrecl$patient_id, FUN=seq_along)
	mplrecl$stage34[mplrecl$WHOstage %in% c(3,4)] = 1
	mplrecl$stage34[mplrecl$WHOstage %in% c(1,2) & is.na(mplrecl$stage34)] = 0	
	mplrecl$patient_id = as.character(mplrecl$patient_id)
	mplrecl$WHOstage = NULL
	mplrecl = mplrecl[order(mplrecl$patient_id, mplrecl$apptdate),]
	
	## SAVES ##
	saveRDS(mplrecl, file=paste0(.ddir, "mplrecl.RDS"))
	saveRDS(mplrecd, file=paste0(.ddir, "mplrecd.RDS"))
	saveRDS(mpclinic, file=paste0(.ddir, "mpclinic.RDS"))
	
	## ASSIGNS GLOBALLY ##
	assign("mplrecl", mplrecl, envir = .GlobalEnv)	
	assign("mplrecd", mplrecd, envir = .GlobalEnv)	
	assign("mpclinic", mpclinic, envir = .GlobalEnv)	

}
