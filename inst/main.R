###############################################################################
# Description: Analyzes impact of LREC program among HIV patients from the IeDEA-EA cohort. 
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 13, 2015
###############################################################################
rm(list = ls())
words <- function(...) paste(substitute(list(...)))[-1]


#############
## OPTIONS ##
#############
options("mc.cores" = 8, digits=4)
setwd("~/Dropbox/00-Studies/lrec_shared/results")


##############
## PACKAGES ##
##############
load.packages = words(devtools, survival, xlsx, reshape, reshape2, Hmisc, geepack, ltmle, magrittr, parallel, splines, SuperLearner, nnet, class, glmnet, polspline, gbm, e1071, FNN)
tmp = lapply(load.packages, require, character.only=T)
cat(sum(unlist(tmp)), "of", length(tmp), "packages loaded successfully\n")
load_all("~/Dropbox/00-Studies/lrecImpact")
#document("~/Dropbox/00-Studies/lrecImpact")


###############
## VARIABLES ##
###############
#.ddir = .fdir = ""
.ddir = "~/Documents/datasets/LREC/LREC_UCB_2013_July/"
.fdir = "~/Dropbox/00-Studies/lrecImpact/inst/"
.enddate = as.Date("2009-03-05")		#Data base closure date taken from info sheet
.discsize = 90							#Number of days in between each time point
.ltfusize = (365.25+30.5)/2				#Days before censoring
date0 = as.Date("2001-01-01")			#Initial date for converting date variables
.Qbounds = c(0.0001,0.9999)				#Truncation bounds for logit(Q)'s
.gbounds = c(0.01,1)					#Truncation bounds for g's
.new_gfit = FALSE
.final_t = 1							#Time-pt we care about
#Commands for cluster 
#(n.b. submit in .sh with "R CMD BATCH -${var} script.R output${var}.Rout")
#(n.b. submit using qsub with "qsub -v var=___ *.sh")
args <- commandArgs(trailingOnly = F)
(myargument <- as.numeric(sub("-", "", args[length(args)])))
(.final_t = ifelse(!is.na(myargument), myargument, 1)) 

#
###########
### DATA ##
###########
#if(FALSE) {
#	# Reads, organizes, and saves SAS data
#	readSAS(.ddir)
#} else {
#	mplrecd = readRDS(file=paste0(.ddir, "mplrecd.RDS"))
#	mplrecl = readRDS(file=paste0(.ddir, "mplrecl.RDS"))
#	mpclinic = readRDS(file=paste0(.ddir, "mpclinic.RDS"))	
#}
#
#
################################################################################
########################### EXPLORATORY DATA ANALYSIS ##########################
################################################################################
#cat("mpclinic:\n\t", ncol(mpclinic), "variables\n\t", nrow(mpclinic), " clinics\n")
#cat("mplrecd:\n\t", ncol(mplrecd), "variables\n\t", nrow(mplrecd), "rows\n\t", length(unique(mplrecd$patient_id)), "patient ids\n")
#cat("mplrecl:\n\t", ncol(mplrecl), "variables\n\t", nrow(mplrecl), "rows\n\t", length(unique(mplrecl$patient_id)), "patient ids\n")
## Verifies everyone has observations in both datasets
#setdiff(mplrecd$patient_id, unique(mplrecl$patient_id))
#setdiff(unique(mplrecl$patient_id), mplrecd$patient_id)
#
## Confirmed that variables defined (from info sheet) match variables we have
#vars.defined = words(Death, ECenrolb4elig, Express, LTFU6mos, arvstart, cd4, cd4arv, cd4b, cd4dateb, cd4e, clinicelig, cliniclast, clintypeelig, clintypelast, dob, dod, eligdate, enroldate, enrollage, exprstart, initclinic, lost2fup, lost2fup_notdead, male, missvispostEC, missvispreEC, neverelig, patient_id, pibasedMR, pibasedelig, pibasedstart, tbtx_arv, urbanelig, urbanlast, whoECmax, whoatarvstart, CLINIC, WHOstage, age, apptdate, arvadhere, cd4date, ec, ecavail, elig, encounter_type, onARV, patient_id, pregnant, stage34, tbtx) 
#cat("Variables without definitions:\n"); setdiff(union(vars.defined, c(names(mplrecd), names(mplrecl))), intersect(vars.defined, c(names(mplrecd), names(mplrecl))))
#
###############
### MPCLINIC ##
###############
#cat(sum(!is.na(mpclinic$clinic_start_date)), "of", nrow(mpclinic), "clinics have an LREC start date\n")
#cat(sum(mpclinic$urbanclin), "of", nrow(mpclinic), "clinics are in an urban area\n")
#cat("Distribution of clinics:\n"); table(mpclinic$clintype)
#
##Looks at clinics starting LREC
#startedLREC = subset(mpclinic, !is.na(clinic_start_date))
#cat("Among", nrow(startedLREC), "clinics starting LREC:\n")
#cat("\t", sum(startedLREC$urbanclin), "are in urban area\n")
#cat("\t Distribution of clinic type:\n"); table(startedLREC$clintype)
#
##############
### MPLRECD ##
##############
#cat("Of the", nrow(mplrecd), "patients in the data set:\n")
#cat("\t", sum(is.na(mplrecd$cd4arv)), "are missing CD4 at start of ARV\n")
#cat("\t", sum(mplrecd$Death), "are marked deceased\n")
#cat("\t", sum(mplrecd$lost2fup), "are marked lost to follow-up or deceased\n")
#cat("\t", sum(mplrecd$lost2fup_notdead), "are marked lost to follow-up and not dead\n")
#cat("\t", sum(mplrecd$LTFU6mos), "are marked absent from clinic for more than 6 months\n")
#
##############
### MPLRECL ##
##############
## Verifies variables are indeed time-varying
#mplrecl.variables = rep(NA, ncol(mplrecl))
#names(mplrecl.variables) = names(mplrecl)
#for(i in 1:ncol(mplrecl)){
#	mplrecl.variables[i] = ifelse(all(tapply(mplrecl[[i]], mplrecl$patient_id, function(x) var(x, na.rm=T))==0), FALSE, TRUE)	
#}
#cat(sum(mplrecl.variables), "of", length(mplrecl.variables), "covariates are time-varying\n")
#if(FALSE) {
#	# Looks at time between visits
#	time.since.last = rep(NA, nrow(mplrecl))
#	time.since.last[2:nrow(mplrecl)] = mplrecl$apptdate[2:nrow(mplrecl)] - mplrecl$apptdate[1:(nrow(mplrecl)-1)]
#	time.since.last[mplrecl$rank==1] = NA
#	summary(time.since.last)
#	png("./timeBetweenVisits.png", height=500, width=500)
#	hist(time.since.last, xlim=c(0,150), breaks=c(0, 15, 30, 45, 60, 75, 90, 105, max(time.since.last, na.rm=T)), main="Histogram of time between clinic visits")
#	mtext(paste("range = (", min(time.since.last, na.rm=T), ",", max(time.since.last, na.rm=T), ")"), side=3, line=.5)
#	dev.off()
#	
#	# Looks at missing clinic values
#	tmp = tapply(is.na(mplrecl$CLINIC), mplrecl$patient_id, sum)
#	cat("Number of missing clinic values for each person\n"); table(tmp)	
#}
#cat(sum(is.na(mplrecl$cd4) & !is.na(mplrecl$cd4date)), "patients have a CD4 date, but no recorded value.\n") 
#temp = tapply(mplrecl$CLINIC, mplrecl$patient_id, function(x) length(table(x)))
#cat("Number of clinics visited by each patient\n"); table(temp)
#
######################
### EXCEL SUMMARIES ##
######################
##rawDataSummary(file=paste0("./Results_", .discsize, "day.xlsx"))
#
#
################################################################################
############################ DATA CLEAN/ORGANIZATION ###########################
################################################################################
#
##############################
### LIMITED LOCF IMPUTATION ##
##############################
## Pregnancy
#temp = subset(mplrecl, select=c("pregnant", "apptdate"))
#pregnant_locf = mclapply(split(temp, mplrecl$patient_id), function(x) limitedImpute(x[,"pregnant"], x[,"apptdate"], 45*7))
#mplrecl$pregnant_locf.l = do.call("c", pregnant_locf)
#mplrecl$pregnant = NULL
## TB treatment
#temp = subset(mplrecl, select=c("tbtx", "apptdate"))
#tbtx_locf = mclapply(split(temp, mplrecl$patient_id), function(x) limitedImpute(x[,"tbtx"], x[,"apptdate"], 8*30.5))
#mplrecl$tbtx_locf.l = do.call("c", tbtx_locf)
#mplrecl$tbtx = NULL
#
#
########################
### MANUAL IMPUTATION ##
########################
## MPLRECL
#mplrecl$stage34[is.na(mplrecl$stage34)] = 0
#mplrecl$arvadhere[is.na(mplrecl$arvadhere)] = 0
#
## MPLRECD
#mplrecd$neverelig[is.na(mplrecd$neverelig)] = 0
#mplrecd$urbanelig[is.na(mplrecd$urbanelig)] = 0
#mplrecd$pibasedstart[is.na(mplrecd$pibasedstart)] = 0
#mplrecd$pibasedMR[is.na(mplrecd$pibasedMR)] = 0
#mplrecd$pibasedelig[is.na(mplrecd$pibasedMR)] = 0
#
#
###############################
### LAST OBS CARRIED FORWARD ##
###############################
#mplrecl$cd4_locf = do.call("c", tapply(mplrecl$cd4, mplrecl$patient_id, locf))
#mplrecl$CLINIC_locf = do.call("c", tapply(mplrecl$CLINIC, mplrecl$patient_id, locf))
#mplrecl$stage34_locf = do.call("c", tapply(mplrecl$stage34, mplrecl$patient_id, locf))
#mplrecl$cd4 = mplrecl$stage34 = mplrecl$CLINIC = NULL
#
#
##########################
### NADIR/ZENITH VALUES ##
##########################
#mplrecl$cd4_nadir = do.call("c", tapply(mplrecl$cd4_locf, mplrecl$patient_id, function(x) nadir(x)))
#mplrecl$cd4_zenith = do.call("c", tapply(mplrecl$cd4_locf, mplrecl$patient_id, function(x) zenith(x)))
#
#
############
### TRASH ##
############
## Excluded covariates
#mplrecd$cd4dateb = mplrecd$missvispostEC = mplrecd$cliniclast = mplrecd$lost2fup = mplrecd$initclinic = mplrecd$lost2fup_notdead = mplrecd$LTFU6mos = NULL
#mplrecl$cd4date = mplrecl$encounter_type = NULL
#
#
################################################################################
############################## DATA DISCRETIZATION #############################
################################################################################
##Assume L(t)-->Y(t)-->A(t)-->T(t)-->C(t) for each t
#
################
### EXCLUSION ##
################
#bad.ids = subset(mplrecd, is.na(ECenrolb4elig) | ECenrolb4elig==1)$patient_id
#mplrecl = subset(mplrecl, !patient_id %in% bad.ids)
#mplrecd = subset(mplrecd, !patient_id %in% bad.ids)
#
#
#################
### START DATE ##
#################
#elig = subset(mplrecl, elig==1)
#eligdate = tapply(elig$apptdate, elig$patient_id, min) 
#eligdate = data.frame(patient_id=names(eligdate), startdate=as.Date(eligdate, origin="1970-01-01"))
#mplrecl = merge(mplrecl, eligdate, by="patient_id", all.x=TRUE)
#mplrecl = subset(mplrecl, apptdate>=startdate)
#mplrecl = mplrecl[order(mplrecl$patient_id, mplrecl$apptdate),]
#mplrecl$rank = ave(rep(NA, nrow(mplrecl)), mplrecl$patient_id, FUN=seq_along)
#rownames(mplrecl) = NULL
#
#
################
### STOP DATE ##
################
## Imputes any missing death dates
#dod.missing = subset(mplrecd, is.na(dod) & Death==1, select=patient_id)
#dod.missing.visits = merge(mplrecl, dod.missing, by="patient_id")
#last.visits = as.Date(tapply(dod.missing.visits$apptdate, dod.missing.visits$patient_id, max) + 1, origin="1970-01-01")
#last.visits = data.frame(patient_id=names(last.visits), last.visits)
#mplrecd = merge(mplrecd, last.visits, by="patient_id", all.x=TRUE)
#mplrecd$dod[is.na(mplrecd$dod) & mplrecd$Death==1] = mplrecd$last.visits[is.na(mplrecd$dod) & mplrecd$Death==1] 
#mplrecd$last.visits = NULL
#
## Final visits before being LTFU
#lastDate = as.Date(with(mplrecl, tapply(apptdate, patient_id, Lastvisit, .ltfusize)), origin="1970-01-01")
#finalVisit = as.Date(with(mplrecl, tapply(apptdate, patient_id, max)), origin="1970-01-01")
#cat(sum(finalVisit!=lastDate), "of", length(lastDate), "ppl censored before final visit\n")
#
## Transfers
#clinics.notimpl = subset(mpclinic, is.na(clinic_start_date))$CLINIC
#temp = subset(mplrecl, CLINIC_locf %in% clinics.notimpl, select=c('patient_id', 'apptdate'))
#trans.date = data.frame(transfer.date = as.Date(tapply(temp$apptdate, temp$patient_id, min), origin="1970-01-01"))
#trans.date$patient_id = rownames(trans.date)
#cat(nrow(trans.date), "ppl transfer to non-LREC clinic\n")
#
## 1st of either: (a) death, (b) ltfu, (c) transfer, or (d) eofu
#minTC = as.data.frame(matrix(nrow=nrow(lastDate), ncol=2, dimnames=list(names(lastDate), c("ltfuDate", "dod"))))
#minTC$ltfuDate = as.Date(lastDate + .ltfusize, origin="1970-01-01")
#minTC[mplrecd$patient_id,"dod"] = mplrecd$dod
#minTC$dod = as.Date(minTC$dod, origin="1970-01-01")
#minTC[rownames(trans.date),"transferDate"] = trans.date$transfer.date
#minTC$transferDate = as.Date(minTC$transferDate, origin="1970-01-01")
#cat("We lost", sum(minTC[,1] < minTC[,2] | minTC[,3] < minTC[,2], na.rm=T), "of", sum(!is.na(minTC[,2])), "deaths due to censoring before obs.\n")
#cat("Any overlapping events?\n")
#table(minTC[,1]==minTC[,2])
#table(minTC[,1]==minTC[,3])
#table(minTC[,2]==minTC[,3])
#table(minTC[,1]==.enddate)
#table(minTC[,2]==.enddate)
#table(minTC[,3]==.enddate)
#stopdate = data.frame(patient_id=rownames(minTC), stopdate = pmin(minTC[,1], minTC[,2], minTC[,3], .enddate, na.rm=T))
#stopdate$outcome = with(stopdate,
#		ifelse(stopdate==minTC[,"dod"] & !is.na(minTC[,"dod"]), "death",
#		ifelse(stopdate==minTC[,"ltfuDate"], "ltfu",
#		ifelse(stopdate==minTC[,"transferDate"] & !is.na(minTC[,"transferDate"]), "transfer",
#		ifelse(stopdate==.enddate, "eofu", "MISSING")))))
#mplrecl = merge(mplrecl, stopdate, by="patient_id", all.x=TRUE)
#mplrecl = subset(mplrecl, startdate<stopdate)
#mplrecl = mplrecl[order(mplrecl$patient_id, mplrecl$apptdate),]
#
#
##################
### PERSON-TIME ##
##################
#mplrecl$age = mplrecl$ecavail = mplrecl$ec = mplrecl$rank = NULL
#discTemp = mclapply(split(mplrecl, mplrecl$patient_id), discretize, "apptdate", "startdate", "stopdate", "outcome", .discsize) 
#discretizedData = do.call("rbind", discTemp)
#discretizedData$dead.ltfu = discretizedData$death + discretizedData$ltfu 
#discretizedData = discretizedData[order(discretizedData$patient_id, discretizedData$date),]
#discretizedData$rank = ave(rep(NA, nrow(discretizedData)), discretizedData$patient_id, FUN=seq_along) - 1		
## September 4, 2014: Excludes those failing at t=0
#discretizedData = subset(discretizedData, !(rank==0 & dead.ltfu==1))
## Adds in baseline covariates
#mplrecd$clinicelig = mplrecd$eligdate = mplrecd$Express = mplrecd$neverelig = mplrecd$ECenrolb4elig = mplrecd$enrollage = mplrecd$dod = mplrecd$Death = mplrecd$cd4e = mplrecd$cd4b = mplrecd$pibasedelig = mplrecd$pibasedstart = NULL
#discretizedData = merge(discretizedData, mplrecd, by="patient_id", all.x=TRUE)
#mpclinic = rename(mpclinic, c(CLINIC="CLINIC_locf"))
#discretizedData = merge(discretizedData, mpclinic, by="CLINIC_locf", all.x=TRUE)
#discretizedData = discretizedData[order(discretizedData$patient_id, discretizedData$date),]
#
#
################################################################################
############################### FINAL MANAGEMENT ###############################
################################################################################
#
##################
### IMPUTATIONS ##
##################
#n = subset(discretizedData, rank==0)
#discretizedData$elig[is.na(discretizedData$elig)] = 1
#discretizedData$cd4arv[is.na(discretizedData$cd4arv)] = median(n$cd4arv, na.rm=TRUE)
#discretizedData$whoatarvstart[is.na(discretizedData$whoatarvstart)] = as.numeric(names(which.max(table(n$whoatarvstart))))
#discretizedData$whoECmax[is.na(discretizedData$whoECmax)] = as.numeric(names(which.max(table(n$whoECmax))))
#
#
####################
### TX INDICATORS ##
####################
#
## LREC Availability
#avail = with(discretizedData, ifelse(!is.na(clinic_start_date) & clinic_start_date<date+.discsize, 1, 0))
#discretizedData$avail = do.call("c", tapply(avail, discretizedData$patient_id, cummax))
#discretizedData$availPrev[2:nrow(discretizedData)] = discretizedData$avail[1:(nrow(discretizedData)-1)]
#discretizedData$availPrev[discretizedData$rank==0] = 0
##nb. Many ppl become eligible AFTER LREC has already started.
#
## LREC Enrollment
#enroll = with(discretizedData, ifelse(!is.na(exprstart) & exprstart<date+.discsize, 1, 0))
#discretizedData$enroll = do.call("c", tapply(enroll, discretizedData$patient_id, cummax))
## Four ppl transfer, causing them to be enrolled w/o availability; marked NOT enrolled at time points
#discretizedData$exprstart[discretizedData$avail==0 & discretizedData$enroll==1] = NA
#discretizedData$enroll[discretizedData$avail==0 & discretizedData$enroll==1] = 0
#discretizedData$enrollPrev[2:nrow(discretizedData)] = discretizedData$enroll[1:(nrow(discretizedData)-1)] 
#discretizedData$enrollPrev[discretizedData$rank==0] = 0
#with(discretizedData, table(avail, enroll))
#
#
#######################
### ADD'L COVARIATES ##
#######################
#
## Time since EC availability
#rank.since.ecavail =  do.call("c", tapply(discretizedData$avail, discretizedData$patient_id, cumsum))
#discretizedData$rankSinceAvail = pmax(0, rank.since.ecavail-1)
## Time since EC enrollment
#rank.since.ecenroll =  do.call("c", tapply(discretizedData$enroll, discretizedData$patient_id, cumsum))
#discretizedData$rankSinceEnroll = pmax(0, rank.since.ecenroll-1)
#
## Rank of EC availability
#ecAvail = subset(discretizedData, avail==1 & rankSinceAvail==0, select=c("patient_id", "rank"))
#ecAvail = rename(ecAvail, c(rank="availRank"))
#discretizedData = merge(discretizedData, ecAvail, by="patient_id", all.x=TRUE)
#discretizedData = discretizedData[order(discretizedData$patient_id, discretizedData$date),]
## Rank of EC enrollment
#ecEnroll = subset(discretizedData, enroll==1 & rankSinceEnroll==0, select=c("patient_id", "rank"))
#ecEnroll = rename(ecEnroll, c(rank="enrollRank"))
#discretizedData = merge(discretizedData, ecEnroll, by="patient_id", all.x=TRUE)
#discretizedData = discretizedData[order(discretizedData$patient_id, discretizedData$date),]
#
## Age
#discretizedData$ageb = as.numeric(discretizedData$startdate-discretizedData$dob) / 365.25
#discretizedData$dob = NULL
#
## CD4V categories
#discretizedData$cd4v_locf.cat = with(discretizedData, ifelse(cd4_locf<200, "lt200", ifelse(cd4_locf<350, "200to350", ifelse(cd4_locf<500, "350to500", ifelse(cd4_locf>=500, "ge500", NA)))))
#discretizedData$cd4v_locf.cat = factor(discretizedData$cd4v_locf.cat) 
#discretizedData$cd4v_locf.cat = relevel(discretizedData$cd4v_locf.cat, ref="lt200")
#
## Indicator that EC available immediately
#availImm = subset(discretizedData, rank==0, select=c("patient_id", "avail"))
#availImm = rename(availImm, c(avail="availImm"))
#discretizedData = merge(discretizedData, availImm, by="patient_id", all.x=TRUE)
#discretizedData = discretizedData[order(discretizedData$patient_id, discretizedData$date),]
#
## Indicator that person enrolled in EC immediately
#enrollImm = subset(discretizedData, rank==0, select=c("patient_id", "enroll"))
#enrollImm = rename(enrollImm, c(enroll="enrollImm"))
#discretizedData = merge(discretizedData, enrollImm, by="patient_id", all.x=TRUE)
#discretizedData = discretizedData[order(discretizedData$patient_id, discretizedData$date),]
#
## Seen in last visit
#discretizedData$prevSeen = ifelse(discretizedData$visitCountPrev==0, 0, 1)
#discretizedData$prevSeen[discretizedData$rank==0] = 1
#
## Converts factors to indicators
#whoatarvstart = convertFactor(discretizedData$whoatarvstart, "whoatarvstart", ref="1")
#whoECmax = convertFactor(discretizedData$whoECmax, "whoECmax", ref="1")
#clintypeelig = convertFactor(discretizedData$clintypeelig, "clintypeelig")
#clintype = convertFactor(discretizedData$clintype, "clintype")
#cd4v_locf.cat = convertFactor(discretizedData$cd4v_locf.cat, "cd4v_locf.cat", ref="ge500")
#discretizedData = cbind(discretizedData, whoatarvstart, whoECmax, clintypeelig, clintype, cd4v_locf.cat)
#discretizedData$whoatarvstart = discretizedData$whoECmax = discretizedData$clintypeelig = discretizedData$clintype = discretizedData$cd4v_locf.cat = NULL
#
## Converts all cd4 variables to per 100
#discretizedData$cd4_locf.p100 = discretizedData$cd4_locf / 100
#discretizedData$cd4arv.p100 = discretizedData$cd4arv / 100 
#discretizedData$cd4_nadir.p100 = discretizedData$cd4_nadir / 100
#discretizedData$cd4_zenith.p100 = discretizedData$cd4_zenith / 100
#discretizedData$cd4_locf = discretizedData$cd4arv = discretizedData$cd4_nadir = discretizedData$cd4_zenith = NULL  
#
## 02/13/2014: Removing variable d/t positivity problems
#discretizedData$arvstart = NULL
#
## Converts dates to per 180 days
#discretizedData$daysfromenroll.p180 = as.numeric(discretizedData$date - discretizedData$enroldate) / 180
#discretizedData$enroldate = NULL
#
## Ensures time-ordering: L(t)-->Y(t)-->A(t)-->T(t)-->C(t) 
#discretizedData$avail[discretizedData$dead.ltfu==1] = discretizedData$enroll[discretizedData$dead.ltfu==1] = discretizedData$transfer[discretizedData$dead.ltfu==1] = discretizedData$eofu[discretizedData$dead.ltfu==1] = NA
#discretizedData$eofu[discretizedData$transfer==1] = NA
#
## Orders and saves
#discretizedData = discretizedData[order(discretizedData$patient_id, discretizedData$date),]
#discretizedData = discretizedData[,order(names(discretizedData))]
#summary(discretizedData)
#dim(discretizedData)
#discretizedData$date.p180 = as.numeric(discretizedData$date-date0) / 365.25
#saveRDS(discretizedData, paste0(.ddir, "discData_disc", .discsize, "_ltfu", .ltfusize, ".RDS"))
#
#
######################
### EXCEL SUMMARIES ##
######################
#discDataSummary(data=discretizedData, file=paste0("./Results_", .discsize, "day.xlsx"))
#countSummary(data=discretizedData, file=paste0("./Results_", .discsize, "day.xlsx"))
#
#
################################################################################
################################### SCREENING ##################################
################################################################################
#confounders = list("ageb", "arvadhere", "cd4_locf.p100", "cd4_nadir.p100", "cd4_zenith.p100", "cd4arv.p100", c("cd4v_locf.cat.ge500.lt200", "cd4v_locf.cat.ge500.200to350", "cd4v_locf.cat.ge500.350to500"), c("clintype.ruralhealthcenter..sub.districthospital", "clintype.ruralhealthcenter.refhospital"), c("clintypeelig.ruralhealthcenter..sub.districthospital", "clintypeelig.ruralhealthcenter.refhospital"), "date.p180", "daysfromenroll.p180", "male", "missvispreEC", "onARV", "pibasedMR", "pregnant_locf.l", "prevSeen", "stage34_locf", "tbtx_arv", "tbtx_locf.l", "urbanclin", "urbanelig", "visitCountCurr", c("whoECmax.1.2", "whoECmax.1.3", "whoECmax.1.4"), c("whoatarvstart.1.2", "whoatarvstart.1.3", "whoatarvstart.1.4"))
#
#
############
### DEATH ##
############
#screen.death = mclapply(confounders, function(x) screen(X = discretizedData[, x, drop=FALSE], Y = discretizedData$death, id = discretizedData$patient_id, time = discretizedData$rank, df = 2, family = "binomial"))
#screen.death.rank = screen(X = NULL, Y = discretizedData$death, id = discretizedData$patient_id, time = discretizedData$rank, df = 2, family = "binomial")
#screen.death = rbind(screen.death.rank, do.call("rbind", screen.death))
#screen.death.all = cbind(screen.death, getOR(screen.death))
#saveRDS(screen.death.all, file="screen.death.RDS")
#
#
###########
### LTFU ##
###########
#screen.ltfu = mclapply(confounders, function(x) screen(X = discretizedData[, x, drop=FALSE], Y = discretizedData$ltfu, id = discretizedData$patient_id, time = discretizedData$rank, df = 2, family = "binomial"))
#screen.ltfu.rank = screen(X = NULL, Y = discretizedData$ltfu, id = discretizedData$patient_id, time = discretizedData$rank, df = 2, family = "binomial")
#screen.ltfu = rbind(screen.ltfu.rank, do.call("rbind", screen.ltfu))
#screen.ltfu.all = cbind(screen.ltfu, getOR(screen.ltfu))
#saveRDS(screen.ltfu.all, file="screen.ltfu.RDS")
#
#
################
### DEAD.LTFU ##
################
#screen.dead.ltfu = mclapply(confounders, function(x) screen(X = discretizedData[, x, drop=FALSE], Y = discretizedData$dead.ltfu, id = discretizedData$patient_id, time = discretizedData$rank, df = 2, family = "binomial"))
#screen.dead.ltfu.rank = screen(X = NULL, Y = discretizedData$dead.ltfu, id = discretizedData$patient_id, time = discretizedData$rank, df = 2, family = "binomial")
#screen.dead.ltfu = rbind(screen.dead.ltfu.rank, do.call("rbind", screen.dead.ltfu))
#screen.dead.ltfu.all = cbind(screen.dead.ltfu, getOR(screen.dead.ltfu))
#saveRDS(screen.dead.ltfu.all, file="screen.dead.ltfu.RDS")
#
#
###################
### AVAILABILITY ##
###################
#availData = subset(discretizedData, availPrev==0 & dead.ltfu==0)
#screen.avail = mclapply(confounders, function(x) screen(X = availData[, x, drop=FALSE], Y = availData$avail, id = availData$patient_id, time = availData$rank, df=2, family = "binomial"))
#screen.avail.rank = screen(X = NULL, Y = availData$avail, id = availData$patient_id, time = availData$rank, df=2, family = "binomial")
#screen.avail = rbind(screen.avail.rank, do.call("rbind", screen.avail))
#screen.avail.all = cbind(screen.avail, getOR(screen.avail))
#saveRDS(screen.avail.all, file="screen.avail.RDS")
#
#
#################
### ENROLLMENT ##
#################
#enrollData = subset(discretizedData, avail==1 & enrollPrev==0 & dead.ltfu==0)
#screen.enroll = mclapply(confounders, function(x) screen(X = enrollData[, x, drop=FALSE], Y = enrollData$enroll, id = enrollData$patient_id, time = enrollData$rank, df=2, family = "binomial"))
#screen.enroll.rank = screen(X = NULL, Y = enrollData$enroll, id = enrollData$patient_id, time = enrollData$rank, df=2, family = "binomial")
#screen.enroll = rbind(screen.enroll.rank, do.call("rbind", screen.enroll))
#screen.enroll.all = cbind(screen.enroll, getOR(screen.enroll))
#saveRDS(screen.enroll.all, file="screen.enroll.RDS")
#
#
###############
### TRANSFER ##
###############
#transferData = subset(discretizedData, dead.ltfu==0)
#screen.transfer = mclapply(confounders, function(x) screen(X = transferData[, x, drop=FALSE], Y = transferData$transfer, id = transferData$patient_id, time = transferData$rank, df=2, family = "binomial"))
#screen.transfer.rank = screen(X = NULL, Y = transferData$transfer, id = transferData$patient_id, time = transferData$rank, df=2, family = "binomial")
#screen.transfer = rbind(screen.transfer.rank, do.call("rbind", screen.transfer))
#screen.transfer.all = cbind(screen.transfer, getOR(screen.transfer))
#saveRDS(screen.transfer.all, file="screen.transfer.RDS")
#
#
###########
### EOFU ##
###########
## nb. EOFU only needs baseline covariates
#eofu.confounders = list("ageb", "cd4arv.p100", c("clintypeelig.ruralhealthcenter..sub.districthospital", "clintypeelig.ruralhealthcenter.refhospital"), "male", "missvispreEC", "pibasedMR", "tbtx_arv", "urbanelig", c("whoECmax.1.2", "whoECmax.1.3", "whoECmax.1.4"), c("whoatarvstart.1.2", "whoatarvstart.1.3", "whoatarvstart.1.4"))
#eofuData = subset(discretizedData, dead.ltfu==0 & transfer==0)
#screen.eofu = mclapply(eofu.confounders, function(x) screen(X = eofuData[, x, drop=FALSE], Y = eofuData$eofu, id = eofuData$patient_id, time = eofuData$rank, df=2, family = "binomial"))
#screen.eofu.rank = screen(X = NULL, Y = eofuData$eofu, id = eofuData$patient_id, time = eofuData$rank, df=2, family = "binomial")
#screen.eofu = rbind(screen.eofu.rank, do.call("rbind", screen.eofu))
#screen.eofu.tmp = cbind(screen.eofu, getOR(screen.eofu))
#screen.eofu.all = matrix(nrow=nrow(screen.transfer.all), ncol=ncol(screen.transfer.all), dimnames=list(rownames(screen.transfer.all), colnames(screen.transfer.all)))
#screen.eofu.all[rownames(screen.eofu.tmp),] = as.matrix(screen.eofu.tmp)
#screen.eofu.all = data.frame(screen.eofu.all)
#saveRDS(screen.eofu.all, file="screen.eofu.RDS")
#
#
###################
### EXCEL OUTPUT ##
###################
#results = list(screen.death.all, screen.ltfu.all, screen.dead.ltfu.all, screen.avail.all, screen.enroll.all, screen.transfer.all, screen.eofu.all)
#names(results) = c('screen.death.all', 'screen.ltfu.all', 'screen.dead.ltfu.all', 'screen.avail.all', 'screen.enroll.all', 'screen.transfer.all', 'screen.eofu.all')
#screenSummary(results=results, file=paste0("./Results_", .discsize, "day.xlsx"))



###############################################################################
################################# g/Q FORMULAS ################################
###############################################################################
# nb. All covariates but c(male, missvispreEC) passed screen. Included male anyways.
discretizedData = readRDS(paste0(.ddir, "discData_disc", .discsize, "_ltfu", .ltfusize, ".RDS"))
core.vars = c("patient_id", "rank", "dead.ltfu", "avail", "enroll", "transfer", "eofu")
base.vars = c("ageb", "cd4arv.p100", c("clintypeelig.ruralhealthcenter..sub.districthospital", "clintypeelig.ruralhealthcenter.refhospital"), "male", "pibasedMR", "tbtx_arv", "urbanelig", c("whoECmax.1.2", "whoECmax.1.3", "whoECmax.1.4"), c("whoatarvstart.1.2", "whoatarvstart.1.3", "whoatarvstart.1.4"))
time.vars = c("arvadhere", "cd4_nadir.p100", "cd4_zenith.p100", "cd4v_locf.cat.ge500.lt200", "cd4v_locf.cat.ge500.200to350", "cd4v_locf.cat.ge500.350to500", "clintype.ruralhealthcenter..sub.districthospital", "clintype.ruralhealthcenter.refhospital", "daysfromenroll.p180", "onARV", "pregnant_locf.l", "prevSeen", "stage34_locf", "tbtx_locf.l", "urbanclin")
# nb. couldn't use "date" variable d/t positivity violation

##############
## FORMULAS ##
##############
for(i in 1:.final_t){
	if(i==1) {
		#Q-formulas
		Qform.null = "Q.kplus1 ~ avail.0 + enroll.0"
		Qform = paste("Q.kplus1 ~ ", paste(c(base.vars, paste0(c(time.vars, "avail", "enroll"),".",i-1)), collapse=" + "))
		#g-formulas
		gform.null = c(
				paste0("avail.",i-1, " ~ 1"),
				paste0("enroll.",i-1, " ~ 1"),
				paste0("transfer.",i-1, " ~ 1"),
				paste0("eofu.",i-1, " ~ 1")
		)
		gform = c(
				paste0("avail.",i-1, " ~ ", paste(c(base.vars, paste0(time.vars,".",i-1)), collapse=" + ")),
				paste0("enroll.",i-1, " ~ ", paste(c(base.vars, paste0(time.vars,".",i-1)), collapse=" + ")),
				paste0("transfer.",i-1, " ~ ", paste(c(base.vars, paste0(time.vars,".",i-1), paste0(c("avail", "enroll"), ".",i-1)), collapse=" + ")),
				paste0("eofu.",i-1, " ~ ", paste(c(base.vars), collapse=" + "))
		)
	} else {
		#Q-formulas
		Qform.null = c(Qform.null, paste("Q.kplus1 ~ ", paste(c(paste0(c("avail", "enroll"),".",i-1)), collapse=" + ")))
		Qform = c(Qform, paste("Q.kplus1 ~ ", paste(c(base.vars, paste0(c(time.vars, "avail", "enroll"),".",i-1)), collapse=" + ")))
		#g-formulas
		gform.null = c(gform.null,
				paste0("avail.",i-1, " ~ 1"),
				paste0("enroll.",i-1, " ~ 1"),
				paste0("transfer.",i-1, " ~ 1"),
				paste0("eofu.",i-1, " ~ 1")
		)
		gform = c(gform,
				paste0("avail.",i-1, " ~ ", paste(c(base.vars, paste0(time.vars,".",i-1)), collapse=" + ")),
				paste0("enroll.",i-1, " ~ ", paste(c(base.vars, paste0(time.vars,".",i-1)), collapse=" + ")),
				paste0("transfer.",i-1, " ~ ", paste(c(base.vars, paste0(time.vars,".",i-1), paste0(c("avail", "enroll"), ".",i-1)), collapse=" + ")),
				paste0("eofu.",i-1, " ~ ", paste(c(base.vars), collapse=" + "))
		)
	}
	names(gform.null)[(4*(i-1)+1):(4*(i-1)+4)] = names(gform)[(4*(i-1)+1):(4*(i-1)+4)] = paste0(c("avail.", "enroll.", "transfer.", "eofu."), i-1)
	names(Qform.null)[i] = names(Qform)[i] = paste0("arvadhere.", i)
}


#########
# NODES #
#########
Anodes = Lnodes = Cnodes = Ynodes = NULL
for(i in 1:.final_t){
	Lnodes = c(Lnodes, paste0(time.vars,".", i))
	Ynodes = c(Ynodes, paste0("dead.ltfu.",i))
	Anodes = c(Anodes, paste0("avail.",i-1), paste0("enroll.",i-1))
	Cnodes = c(Cnodes, paste0("transfer.",i-1), paste0("eofu.",i-1))
}


#####################
## DETERMINISTIC G ##
#####################
MaintainTreatment.2 = function(data, current.node, nodes) {
	A1nodes = grep("^avail\\.", names(data))
	A2nodes = grep("^enroll\\.", names(data))
	## NON-TX ##
	if (!(current.node %in% nodes$A)) {
		return(NULL)
		## 1st Node ##
	} else if (names(data)[current.node]=="avail.0") {
		return(NULL)
	} else if (names(data)[current.node]=="enroll.0") {
		curr.A1node = max(A1nodes[A1nodes < current.node])
		is.deterministic = data[,curr.A1node] == 0
		is.deterministic[is.na(is.deterministic)] = FALSE
		prob1 = rep(0, sum(is.deterministic, na.rm=T))
		## Subsequent Nodes ##
	} else if (grepl("^avail\\.", names(data)[current.node])) {
		#If already available, then continue availability
		prev.A1node <- max(A1nodes[A1nodes < current.node])
		is.deterministic <- data[,prev.A1node] == 1
		is.deterministic[is.na(is.deterministic)] = FALSE
		prob1 = rep(1, sum(is.deterministic, na.rm=T))
	} else if (grepl("^enroll\\.", names(data)[current.node])) {
		#If not available, then can't enroll
		curr.A1node <- max(A1nodes[A1nodes < current.node])
		is.deterministic.1 = data[,curr.A1node] == 0
		#If already enrolled, then continue enrollment
		prev.A2node <- max(A2nodes[A2nodes < current.node])
		is.deterministic.2 = data[,prev.A2node] == 1
		#Assigns probabilities based on above deterministic nature
		is.deterministic = is.deterministic.1 | is.deterministic.2
		is.deterministic[is.na(is.deterministic)] = FALSE
		prob1 = rep(NA, length(is.deterministic))
		prob1[is.deterministic.1] = 0 
		prob1[is.deterministic.2] = 1
		prob1 = prob1[!is.na(prob1)]
	}
	return(list(is.deterministic=is.deterministic, prob1=prob1))
}



###############################################################################
############################# SUPER LEARNER LIBRARY ###########################
###############################################################################
source(paste0(.fdir, "SL_fits.R"))

###############
## g-library ##
###############
SL.lib.g = c("SL.mean", "SL.time", "SL.glm", "SL.stepAIC", "SL.gam", "SL.gam.3", "SL.gam.4", "SL.gam.5", "SL.bayesglm", "SL.knn.LT", "SL.knn.5", "SL.knn.15", "SL.knn.20", "SL.knn.25", "SL.lasso.LT", "SL.ridge.LT", "SL.nnet.LT.1", "SL.nnet.3", "SL.nnet.4", "SL.nnet.5", "SL.polymars.LT.1", "SL.gbm.LT.1", "SL.gbm.2", "SL.svm.LT.1")


###############
## Q-library ##
###############
SL.lib.Q = c("SL.mean", "SL.glm", "SL.stepAIC", "SL.gam", "SL.gam.3", "SL.gam.4", "SL.gam.5", "SL.bayesglm", "SL.knn.LT", "SL.knn.5", "SL.knn.15", "SL.knn.20", "SL.knn.25", "SL.lasso.LT", "SL.ridge.LT", "SL.polymars.LT.1", "SL.polymars.LT.2", "SL.svm.LT.1", "SL.svm.LT.2", "SL.nnet.LT.1", "SL.nnet.LT.2", "SL.gbm.LT.1", "SL.gbm.LT.2")



###############################################################################
########################## SUPER LEARNER LONG g-FITS ##########################
###############################################################################

##########
## DATA ## 
##########
A1.data = subset(discretizedData, availPrev==0 & dead.ltfu==0, select=c("patient_id", "avail", "rank", base.vars, time.vars))
A2.data = subset(discretizedData, avail==1 & enrollPrev==0 & dead.ltfu==0, select=c("patient_id", "enroll", "rank", base.vars, time.vars))
C1.data = subset(discretizedData, dead.ltfu==0, select=c("patient_id", "transfer", "rank", "avail", "enroll", base.vars, time.vars))
C2.data = subset(discretizedData, dead.ltfu==0 & transfer==0, select=c("patient_id", "eofu", "rank", base.vars))
# Treatments of interest
A1.data.psi = subset(discretizedData, dead.ltfu==0, select=c("patient_id", "avail", "rank", base.vars, time.vars))
A2.data.psi = subset(discretizedData, dead.ltfu==0, select=c("patient_id", "enroll", "rank", base.vars, time.vars))
C1.data.psi_00 = C1.data.psi_10 = C1.data.psi_11 = subset(discretizedData, dead.ltfu==0, select=c("patient_id", "transfer", "rank", "avail", "enroll", base.vars, time.vars))
C1.data.psi_00$avail = 0; C1.data.psi_00$enroll = 0;
C1.data.psi_10$avail = 1; C1.data.psi_10$enroll = 0;
C1.data.psi_11$avail = 1; C1.data.psi_11$enroll = 1;
C2.data.psi = subset(discretizedData, dead.ltfu==0, select=c("patient_id", "eofu", "rank", base.vars))

##############
## GLM fits ##
##############
if(FALSE) {
	## A1 ##	
	gA1.formula = as.formula(paste("avail ~ ", paste(names(A1.data)[4:ncol(A1.data)], collapse=" + "), " + ns(rank, df=2)"))
	gA1.fit = glm(gA1.formula, data=A1.data, family=binomial)
	saveRDS(gA1.fit, file=paste0("./g-fits/glm/disc", .discsize,"/A1.glm.RDS"))
	## A2 ##
	gA2.formula = as.formula(paste("enroll ~ ", paste(names(A2.data)[4:ncol(A2.data)], collapse=" + "), " + ns(rank, df=2)"))
	gA2.fit = glm(gA2.formula, data=A2.data, family=binomial)
	saveRDS(gA2.fit, file=paste0("./g-fits/glm/disc", .discsize,"/A2.glm.RDS"))
	## C1 ##
	gC1.formula = as.formula(paste("transfer ~ ", paste(names(C1.data)[4:ncol(C1.data)], collapse=" + "), " + ns(rank, df=2)"))
	gC1.fit = glm(gC1.formula, data=C1.data, family=binomial)
	saveRDS(gC1.fit, file=paste0("./g-fits/glm/disc", .discsize,"/C1.glm.RDS"))
	## C2 ##
	gC2.formula = as.formula(paste("eofu ~ ", paste(names(C2.data)[4:ncol(C2.data)], collapse=" + "), " + ns(rank, df=2)"))
	gC2.fit = glm(gC2.formula, data=C2.data, family=binomial)
	saveRDS(gC2.fit, file=paste0("./g-fits/glm/disc", .discsize, "/C2.glm.RDS"))

	##############
	## g-matrix ##
	##############
	# n.b. W want P(not enroll), but ltmle package takes P(enroll)

	## psi_00 ##
	gA1_psi_00 = predict(gA1.fit, newdata=A1.data.psi, type="response")
	gA2_psi_00 = 0
	gC1_psi_00 = 1-predict(gC1.fit, newdata=C1.data.psi_00, type="response")
	gC2_psi_00 = 1-predict(gC2.fit, newdata=C2.data.psi, type="response")

	## psi_10 ## 
	gA1_psi_10 = predict(gA1.fit, newdata=A1.data.psi, type="response")
	gA1_psi_10[A1.data.psi$rank>0] = 1
	gA2_psi_10 = predict(gA2.fit, newdata=A2.data.psi, type="response")
	gC1_psi_10 = 1-predict(gC1.fit, newdata=C1.data.psi_10, type="response")
	gC2_psi_10 = 1-predict(gC2.fit, newdata=C2.data.psi, type="response")

	## psi_11 ##
	gA1_psi_11 = predict(gA1.fit, newdata=A1.data.psi, type="response")
	gA1_psi_11[A1.data.psi$rank>0] = 1
	gA2_psi_11 = predict(gA2.fit, newdata=A2.data.psi, type="response")
	gA2_psi_11[A2.data.psi$rank>0] = 1
	gC1_psi_11 = 1-predict(gC1.fit, newdata=C1.data.psi_11, type="response")
	gC2_psi_11 = 1-predict(gC2.fit, newdata=C2.data.psi, type="response")

	## Combines ##
	gA1 = cbind(A1.data.psi[,c("patient_id", "rank")], gA1_psi_00, gA1_psi_10, gA1_psi_11)
	gA2 = cbind(A2.data.psi[,c("patient_id", "rank")], gA2_psi_00, gA2_psi_10, gA2_psi_11)
	gC1 = cbind(C1.data.psi_00[,c("patient_id", "rank")], gC1_psi_00, gC1_psi_10, gC1_psi_11)
	gC2 = cbind(C2.data.psi[,c("patient_id", "rank")], gC2_psi_00, gC2_psi_10, gC2_psi_11)
	merge.data = list(subset(discretizedData, select=c('patient_id','rank','dead.ltfu')), gA1, gA2, gC1, gC2)
	gALL = Reduce(function(x, y) merge(x, y, all=T, by=c("patient_id", "rank")), merge.data, accumulate=F)

	## Accounts for events ##
	gALL$gA1_psi_00[gALL$dead.ltfu==1] = gALL$gA1_psi_10[gALL$dead.ltfu==1] = gALL$gA1_psi_11[gALL$dead.ltfu==1] = 1
	gALL$gA2_psi_00[gALL$dead.ltfu==1] = gALL$gA2_psi_10[gALL$dead.ltfu==1] = gALL$gA2_psi_11[gALL$dead.ltfu==1] = 1
	gALL$gC1_psi_00[gALL$dead.ltfu==1] = gALL$gC1_psi_10[gALL$dead.ltfu==1] = gALL$gC1_psi_11[gALL$dead.ltfu==1] = 1
	gALL$gC2_psi_00[gALL$dead.ltfu==1] = gALL$gC2_psi_10[gALL$dead.ltfu==1] = gALL$gC2_psi_11[gALL$dead.ltfu==1] = 1	
	
	## SAVES ##
	saveRDS(gALL, file=paste0("./g-fits/glm/disc", .discsize,"/gALL.RDS"))
}


#############
## SL fits ##
#############
if(FALSE) {
	## Availability ##
	set.seed(0123)  
	A1.SL = mcSuperLearner(Y=A1.data$avail, X=A1.data[,3:ncol(A1.data)], newX=A1.data.psi[,3:ncol(A1.data.psi)], id=A1.data$patient_id, SL.library=c(SL.lib.g, "SL.glm.g_AW.1", "SL.glm.g_AW.2", "SL.glm.g_AW.3"), method="method.NNloglik.LT", family="binomial", control = list(trimLogit=.001, saveFitLibrary=FALSE), cvControl=list(V=.nfolds), verbose=TRUE)
	saveRDS(A1.SL, paste0("./g-fits/SL/disc", .discsize, "/A1.SL.RDS"))
	#A1.SL = readRDS(paste0("./g-fits/SL/disc", .discsize, "/A1.SL.RDS"))
	
	## Enrollment ##
	set.seed(2345)
	A2.SL = mcSuperLearner(Y=A2.data$enroll, X=A2.data[,3:ncol(A2.data)], newX=A2.data.psi[,3:ncol(A2.data.psi)], id=A2.data$patient_id, SL.library=c(SL.lib.g, "SL.glm.g_AW.1", "SL.glm.g_AW.2", "SL.glm.g_AW.3"), method="method.NNloglik.LT", family="binomial", control=list(trimLogit=0.001, saveFitLibrary=FALSE), cvControl=list(V=.nfolds), verbose=TRUE)
	saveRDS(A2.SL, paste0("./g-fits/SL/disc", .discsize, "/A2.SL.RDS"))
	#A2.SL = readRDS(paste0("./g-fits/SL/disc", .discsize, "/A2.SL.RDS"))
	
	## Transfer (psi_00) ##
	set.seed(3456)
	C1.SL.psi_00 = mcSuperLearner(Y=C1.data$transfer, X=C1.data[,3:ncol(C1.data)], newX=C1.data.psi_00[,3:ncol(C1.data.psi_00)], id=C1.data$patient_id, SL.library=c(SL.lib.g, "SL.glm.g_TW.1", "SL.glm.g_TW.2", "SL.glm.g_TW.3"), method="method.NNloglik.LT", family="binomial", control=list(trimLogit=0.001, saveFitLibrary=FALSE), cvControl=list(V=.nfolds), verbose=TRUE)
	saveRDS(A2.SL, paste0("./g-fits/SL/disc", .discsize, "/C1.SL.psi_00.RDS"))
	#C1.SL.psi_00 = readRDS(paste0("./g-fits/SL/disc", .discsize, "/C1.SL.psi_00.RDS"))
	
	## Transfer (psi_10) ##
	set.seed(3456)
	C1.SL.psi_10 = mcSuperLearner(Y=C1.data$transfer, X=C1.data[,3:ncol(C1.data)], newX=C1.data.psi_10[,3:ncol(C1.data.psi_10)], id=C1.data$patient_id, SL.library=c(SL.lib.g, "SL.glm.g_TW.1", "SL.glm.g_TW.2", "SL.glm.g_TW.3"), method="method.NNloglik.LT", family="binomial", control=list(trimLogit=0.001, saveFitLibrary=FALSE), cvControl=list(V=.nfolds), verbose=TRUE)
	saveRDS(A2.SL, paste0("./g-fits/SL/disc", .discsize, "/C1.SL.psi_10.RDS"))
	#C1.SL.psi_10 = readRDS(paste0("./g-fits/SL/disc", .discsize, "/C1.SL.psi_10.RDS"))
	
	## Transfer (psi_00) ##
	set.seed(3456)
	C1.SL.psi_11 = mcSuperLearner(Y=C1.data$transfer, X=C1.data[,3:ncol(C1.data)], newX=C1.data.psi_11[,3:ncol(C1.data.psi_11)], id=C1.data$patient_id, SL.library=c(SL.lib.g, "SL.glm.g_TW.1", "SL.glm.g_TW.2", "SL.glm.g_TW.3"), method="method.NNloglik.LT", family="binomial", control=list(trimLogit=0.001, saveFitLibrary=FALSE), cvControl=list(V=.nfolds), verbose=TRUE)
	saveRDS(A2.SL, paste0("./g-fits/SL/disc", .discsize, "/C1.SL.psi_11.RDS"))
	#C1.SL.psi_11 = readRDS(paste0("./g-fits/SL/disc", .discsize, "/C1.SL.psi_11.RDS"))
	
	## EOFU ##
	set.seed(5678)
	C2.SL = mcSuperLearner(Y=C2.data$eofu, X=C2.data[,3:ncol(C2.data)], newX=C2.data.psi[,3:ncol(C2.data.psi)], id=C2.data$patient_id, SL.library=c(setdiff(SL.lib.g, "SL.time"), "SL.glm.g_EW.1", "SL.glm.g_EW.2"), method="method.NNloglik.LT", family="binomial", control=list(trimLogit=0.001, saveFitLibrary=FALSE), cvControl=list(V=.nfolds), verbose=TRUE)
	saveRDS(A2.SL, paste0("./g-fits/SL/disc", .discsize, "/C2.SL.RDS"))
	#C2.SL = readRDS(paste0("./g-fits/SL/disc", .discsize, "/C2.SL.RDS"))

	##############
	## g-matrix ##
	##############
	# n.b. W want P(not enroll), but ltmle package takes P(enroll)
	
	## psi_00 ##
	gA1_psi_00 = as.vector(A1.SL$SL.predict)
	gA2_psi_00 = 0
	gC1_psi_00 = 1-as.vector(C1.SL.psi_00$SL.predict)
	gC2_psi_00 = 1-as.vector(C2.SL$SL.predict)
	
	## psi_10 ##
	gA1_psi_10 = as.vector(A1.SL$SL.predict)
	gA1_psi_10[A1.data.psi$rank>0] = 1
	gA2_psi_10 = as.vector(A2.SL$SL.predict)
	gC1_psi_10 = 1-as.vector(C1.SL.psi_10$SL.predict)
	gC2_psi_10 = 1-as.vector(C2.SL$SL.predict)
	
	## psi_11 ##
	gA1_psi_11 = as.vector(A1.SL$SL.predict)
	gA1_psi_11[A1.data.psi$rank>0] = 1
	gA2_psi_11 = as.vector(A2.SL$SL.predict)
	gA2_psi_11[A2.data.psi$rank>0] = 1
	gC1_psi_11 = 1-as.vector(C1.SL.psi_11$SL.predict)
	gC2_psi_11 = 1-as.vector(C2.SL$SL.predict)
	
	## Combines ##
	gA1 = cbind(A1.data.psi[,c("patient_id", "rank")], gA1_psi_00, gA1_psi_10, gA1_psi_11)
	gA2 = cbind(A2.data.psi[,c("patient_id", "rank")], gA2_psi_00, gA2_psi_10, gA2_psi_11)
	gC1 = cbind(C1.data.psi_00[,c("patient_id", "rank")], gC1_psi_00, gC1_psi_10, gC1_psi_11)
	gC2 = cbind(C2.data.psi[,c("patient_id", "rank")], gC2_psi_00, gC2_psi_10, gC2_psi_11)
	merge.data = list(subset(discretizedData, select=c('patient_id','rank','dead.ltfu')), gA1, gA2, gC1, gC2)
	gALL = Reduce(function(x, y) merge(x, y, all=T, by=c("patient_id", "rank")), merge.data, accumulate=F)
	
	# Accounts for events
	gALL$gA1_psi_00[gALL$dead.ltfu==1] = gALL$gA1_psi_10[gALL$dead.ltfu==1] = gALL$gA1_psi_11[gALL$dead.ltfu==1] = 1
	gALL$gA2_psi_00[gALL$dead.ltfu==1] = gALL$gA2_psi_10[gALL$dead.ltfu==1] = gALL$gA2_psi_11[gALL$dead.ltfu==1] = 1
	gALL$gC1_psi_00[gALL$dead.ltfu==1] = gALL$gC1_psi_10[gALL$dead.ltfu==1] = gALL$gC1_psi_11[gALL$dead.ltfu==1] = 1
	gALL$gC2_psi_00[gALL$dead.ltfu==1] = gALL$gC2_psi_10[gALL$dead.ltfu==1] = gALL$gC2_psi_11[gALL$dead.ltfu==1] = 1
	
	## SAVES ##
	saveRDS(gALL, file=paste0("./g-fits/SL/disc", .discsize, "/gALL.RDS"))

}


################################################################################
############################### TRANPOSES TO WIDE ##############################
################################################################################
# Baseline variables
base.data = subset(discretizedData, rank==0, select=c("patient_id", base.vars))		
rownames(base.data) = base.data$patient_id
base.data$patient_id = NULL

# Time-varying varialbes
time.data = subset(discretizedData, select=c("patient_id", "rank", time.vars, "dead.ltfu", "avail", "enroll", "transfer", "eofu"))
wide.data = reshape(time.data, direction="wide", idvar="patient_id", timevar="rank")
rownames(wide.data) = wide.data$patient_id 
wide.data$patient_id = NULL

# Carries forward deaths for all those who die
for(i in 2:max(discretizedData$rank)) {
	wide.data[,paste0("dead.ltfu.", i)] = ifelse(wide.data[,paste0("dead.ltfu.", i-1)]==1, 1, wide.data[,paste0("dead.ltfu.", i)])
}

# Factorizes censoring variables
for(i in 0:max(discretizedData$rank)) {
	wide.data[,paste0("transfer.", i)] = BinaryToCensoring(is.censored=wide.data[,paste0("transfer.", i)])
	wide.data[,paste0("eofu.", i)] = BinaryToCensoring(is.censored=wide.data[,paste0("eofu.", i)])
}

# Separates out variables of interest
ltmle.data = wide.data[,1:which(names(wide.data)==paste0("dead.ltfu.", .final_t))]
#cat("Patients still aligned?"); table(rownames(base.data) == rownames(ltmle.data))
ltmle.data = cbind(base.data, ltmle.data)
ltmle.data$dead.ltfu.0 = NULL


#
################################################################################
################################ PSI NULL MODELS ###############################
################################################################################
#
###########
### TMLE ##
###########
#psi_00.null.tmle <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(0,0),.final_t),
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, 
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform.null, gform=gform.null, gbounds=.gbounds, stratify=TRUE,
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_00.null.tmle, file=paste0("./ltmle-fits/Unadjusted/psi_00.null.tmle-t", .final_t,".RDS"))
#psi_10.null.tmle <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(1,0),.final_t), 
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, 
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform.null, gform=gform.null, gbounds=.gbounds, stratify=TRUE,
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_10.null.tmle, file=paste0("./ltmle-fits/Unadjusted/psi_10.null.tmle-t", .final_t,".RDS"))
#psi_11.null.tmle <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(1,1),.final_t), 
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes, 
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform.null, gform=gform.null, gbounds=.gbounds, stratify=TRUE, 
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_11.null.tmle, file=paste0("./ltmle-fits/Unadjusted/psi_11.null.tmle-t", .final_t,".RDS"))
#
#
############
### GCOMP ##
############
#psi_00.null.gcmp <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(0,0),.final_t),
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform.null, gform=gform.null, gbounds=.gbounds, gcomp=TRUE, stratify=TRUE, 
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_00.null.gcmp, file=paste0("./ltmle-fits/Unadjusted/psi_00.null.gcmp-t", .final_t,".RDS"))
#psi_10.null.gcmp <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(1,0),.final_t),
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform.null, gform=gform.null, gbounds=.gbounds, gcomp=TRUE, stratify=TRUE, 
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_10.null.gcmp, file=paste0("./ltmle-fits/Unadjusted/psi_10.null.gcmp-t", .final_t,".RDS"))
#psi_11.null.gcmp <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(1,1),.final_t),
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform.null, gform=gform.null, gbounds=.gbounds, gcomp=TRUE, stratify=TRUE, 
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_11.null.gcmp, file=paste0("./ltmle-fits/Unadjusted/psi_11.null.gcmp-t", .final_t,".RDS"))
#
#
################################################################################
################################ GLM WIDE MODELS ###############################
################################################################################

##########
# LTMLE ##
##########
#psi_00.glm.tmle <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(0,0),.final_t),
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform, gform=gform, gbounds=.gbounds,
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_00.glm.tmle, file=paste0("./ltmle-fits/GLM_WIDE/psi_00.glm.tmle-t", .final_t,".RDS"))
#psi_10.glm.tmle <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(1,0),.final_t),
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform, gform=gform, gbounds=.gbounds,
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_10.glm.tmle, file=paste0("./ltmle-fits/GLM_WIDE/psi_10.glm.tmle-t", .final_t,".RDS"))
#psi_11.glm.tmle <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(1,1),.final_t),
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform, gform=gform, gbounds=.gbounds,
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_11.glm.tmle, file=paste0("./ltmle-fits/GLM_WIDE/psi_11.glm.tmle-t", .final_t,".RDS"))


##########
# GCOMP ##
##########
#psi_00.glm.gcmp <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(0,0),.final_t),
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform, gform=gform, gbounds=.gbounds, gcomp=TRUE, stratify=TRUE,
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_00.glm.gcmp, file=paste0("./ltmle-fits/GLM_WIDE/psi_00.glm.gcmp-t", .final_t,".RDS"))
#psi_10.glm.gcmp <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(1,0),.final_t),
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform, gform=gform, gbounds=.gbounds, gcomp=TRUE, stratify=TRUE,
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_10.glm.gcmp, file=paste0("./ltmle-fits/GLM_WIDE/psi_10.glm.gcmp-t", .final_t,".RDS"))
#psi_11.glm.gcmp <- ltmle(
#	data=ltmle.data,
#	abar=rep(c(1,1),.final_t),
#	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#	deterministic.g.function=MaintainTreatment.2,
#	Qform=Qform, gform=gform, gbounds=.gbounds, gcomp=TRUE, stratify=TRUE,
#	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
#saveRDS(psi_11.glm.gcmp, file=paste0("./ltmle-fits/GLM_WIDE/psi_11.glm.gcmp-t", .final_t,".RDS"))
#
#
#
################################################################################
################################ GLM LONG MODELS ###############################
################################################################################

##############
## g-MATRIX ##
##############
gALL = readRDS(file=paste0("./g-fits/glm/disc", .discsize,"/gALL.RDS"))

## Reshapes to wide ##
gmatrix.psi_00 = gALL %>%
	subset(rank<.final_t, select=c("patient_id", "rank", "gA1_psi_00", "gA2_psi_00", "gC1_psi_00", "gC2_psi_00")) %>%
	rename(c(gA1_psi_00="A1", gA2_psi_00="A2", gC1_psi_00="C1", gC2_psi_00="C2")) %>%
	reshape(direction="wide", idvar="patient_id", timevar="rank")
gmatrix.psi_10 = gALL %>%
	subset(rank<.final_t, select=c("patient_id", "rank", "gA1_psi_10", "gA2_psi_10", "gC1_psi_10", "gC2_psi_10")) %>%
	rename(c(gA1_psi_10="A1", gA2_psi_10="A2", gC1_psi_10="C1", gC2_psi_10="C2")) %>%
	reshape(direction="wide", idvar="patient_id", timevar="rank")
gmatrix.psi_11 = gALL %>%
	subset(rank<.final_t, select=c("patient_id", "rank", "gA1_psi_11", "gA2_psi_11", "gC1_psi_11", "gC2_psi_11")) %>%
	rename(c(gA1_psi_11="A1", gA2_psi_11="A2", gC1_psi_11="C1", gC2_psi_11="C2")) %>%
	reshape(direction="wide", idvar="patient_id", timevar="rank")
rownames(gmatrix.psi_00) = gmatrix.psi_00$patient_id; gmatrix.psi_00$patient_id=NULL; gmatrix.psi_00 = as.matrix(gmatrix.psi_00)
rownames(gmatrix.psi_10) = gmatrix.psi_10$patient_id; gmatrix.psi_10$patient_id=NULL; gmatrix.psi_10 = as.matrix(gmatrix.psi_10)
rownames(gmatrix.psi_11) = gmatrix.psi_11$patient_id; gmatrix.psi_11$patient_id=NULL; gmatrix.psi_11 = as.matrix(gmatrix.psi_11)


#########
# TMLE ##
#########
psi_00.glm2.tmle <- ltmle(
	data=ltmle.data,
	abar=rep(c(0,0),.final_t),
	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
	deterministic.g.function=MaintainTreatment.2,
	Qform=Qform, gform=gmatrix.psi_00, gbounds=.gbounds,
	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
saveRDS(psi_00.glm2.tmle, file=paste0("./ltmle-fits/GLM_LONG/psi_00.glm.tmle-t", .final_t,".RDS"))
psi_10.glm2.tmle <- ltmle(
	data=ltmle.data,
	abar=rep(c(1,0),.final_t),
	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
	deterministic.g.function=MaintainTreatment.2,
	Qform=Qform, gform=gmatrix.psi_10, gbounds=.gbounds,
	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
saveRDS(psi_10.glm2.tmle, file=paste0("./ltmle-fits/GLM_LONG/psi_10.glm.tmle-t", .final_t,".RDS"))
psi_11.glm2.tmle <- ltmle(
	data=ltmle.data,
	abar=rep(c(1,1),.final_t),
	Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
	deterministic.g.function=MaintainTreatment.2,
	Qform=Qform, gform=gmatrix.psi_11, gbounds=.gbounds,
	estimate.time=FALSE, SL.library=NULL, survivalOutcome=TRUE)
saveRDS(psi_11.glm2.tmle, file=paste0("./ltmle-fits/GLM_LONG/psi_11.glm.tmle-t", .final_t,".RDS"))
#
#
#
#
###############################################################################
############################### SL LONG MODELS ################################
###############################################################################
#
###############
### g-MATRIX ##
###############
#gALL = readRDS(file=paste0("./g-fits/SL/disc", .discsize, "/gALL.RDS"))
#
### Reshapes to wide ##
#gmatrix.psi_00 = gALL %>%
#	subset(rank<.final_t, select=c("patient_id", "rank", "gA1_psi_00", "gA2_psi_00", "gC1_psi_00", "gC2_psi_00")) %>%
#	rename(c(gA1_psi_00="A1", gA2_psi_00="A2", gC1_psi_00="C1", gC2_psi_00="C2")) %>%
#	reshape(direction="wide", idvar="patient_id", timevar="rank")
#gmatrix.psi_10 = gALL %>%
#	subset(rank<.final_t, select=c("patient_id", "rank", "gA1_psi_10", "gA2_psi_10", "gC1_psi_10", "gC2_psi_10")) %>%
#	rename(c(gA1_psi_10="A1", gA2_psi_10="A2", gC1_psi_10="C1", gC2_psi_10="C2")) %>%
#	reshape(direction="wide", idvar="patient_id", timevar="rank")
#gmatrix.psi_11 = gALL %>%
#	subset(rank<.final_t, select=c("patient_id", "rank", "gA1_psi_11", "gA2_psi_11", "gC1_psi_11", "gC2_psi_11")) %>%
#	rename(c(gA1_psi_11="A1", gA2_psi_11="A2", gC1_psi_11="C1", gC2_psi_11="C2")) %>%
#	reshape(direction="wide", idvar="patient_id", timevar="rank")
#rownames(gmatrix.psi_00) = gmatrix.psi_00$patient_id; gmatrix.psi_00$patient_id=NULL; gmatrix.psi_00 = as.matrix(gmatrix.psi_00)
#rownames(gmatrix.psi_10) = gmatrix.psi_10$patient_id; gmatrix.psi_10$patient_id=NULL; gmatrix.psi_10 = as.matrix(gmatrix.psi_10)
#rownames(gmatrix.psi_11) = gmatrix.psi_11$patient_id; gmatrix.psi_11$patient_id=NULL; gmatrix.psi_11 = as.matrix(gmatrix.psi_11)
#
#
###########
### TMLE ##
###########
#psi_00.SL.tmle <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(0,0),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gmatrix.psi_00, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE,
#		SL.library=list(g=NULL, Q=SL.lib.Q))
#saveRDS(psi_00.SL.tmle, file=paste0("./ltmle-fits/SL_LONG/psi_00.SL.tmle-t", .final_t,".RDS"))
#rm(psi_00.SL.tmle); gc()
#psi_10.SL.tmle <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(1,0),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gmatrix.psi_10, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE,
#		SL.library=list(g=NULL, Q=SL.lib.Q))
#saveRDS(psi_10.SL.tmle, file=paste0("./ltmle-fits/SL_LONG/psi_10.SL.tmle-t", .final_t,".RDS"))
#rm(psi_10.SL.tmle); gc()
#psi_11.SL.tmle <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(1,1),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gmatrix.psi_11, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE,
#		SL.library=list(g=NULL, Q=SL.lib.Q))
#saveRDS(psi_11.SL.tmle, file=paste0("./ltmle-fits/SL_LONG/psi_11.SL.tmle-t", .final_t,".RDS"))
#rm(psi_11.SL.tmle); gc()
#
#
###########
### GCMP ##
###########
#psi_00.SL.gcmp <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(0,0),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gmatrix.psi_00, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE, gcomp=TRUE, stratify=TRUE,
#		SL.library=list(g=NULL, Q=SL.lib.Q))
#saveRDS(psi_00.SL.gcmp, file=paste0("./ltmle-fits/SL_LONG/psi_00.SL.gcmp-t", .final_t,".RDS"))
#rm(psi_00.SL.gcmp); gc()
#psi_10.SL.gcmp <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(1,0),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gmatrix.psi_10, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE, gcomp=TRUE, stratify=TRUE,
#		SL.library=list(g=NULL, Q=SL.lib.Q))
#saveRDS(psi_10.SL.gcmp, file=paste0("./ltmle-fits/SL_LONG/psi_10.SL.gcmp-t", .final_t,".RDS"))
#rm(psi_10.SL.gcmp); gc()
#psi_11.SL.gcmp <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(1,1),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gmatrix.psi_11, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE, gcomp=TRUE, stratify=TRUE,
#		SL.library=list(g=NULL, Q=SL.lib.Q))
#saveRDS(psi_11.SL.gcmp, file=paste0("./ltmle-fits/SL_LONG/psi_11.SL.gcmp-t", .final_t,".RDS"))
#rm(psi_11.SL.gcmp); gc()
#
#
################################################################################
################################ SL WIDE MODELS ################################
################################################################################
#
###########
### TMLE ##
###########
#psi_00.SL.tmle <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(0,0),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gform, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE,
#		SL.library=list(g=SL.lib.g, Q=SL.lib.Q))
#saveRDS(psi_00.SL.tmle, file=paste0("./ltmle-fits/SL_WIDE/psi_00.SL.tmle-t", .final_t,".RDS"))
#rm(psi_00.SL); gc()
#psi_10.SL.tmle <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(1,0),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gform, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE,
#		SL.library=list(g=SL.lib.g, Q=SL.lib.Q))
#saveRDS(psi_10.SL.tmle, file=paste0("./ltmle-fits/SL_WIDE/psi_10.SL.tmle-t", .final_t,".RDS"))
#rm(psi_10.SL); gc()
#psi_11.SL.tmle <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(1,1),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gform, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE,
#		SL.library=list(g=SL.lib.g, Q=SL.lib.Q))
#saveRDS(psi_11.SL.tmle, file=paste0("./ltmle-fits/SL_WIDE/psi_11.SL.tmle-t", .final_t,".RDS"))
#rm(psi_11.SL.tmle); gc()
#
#
###########
### GCMP ##
###########
#psi_00.SL.gcmp <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(0,0),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gform, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE, gcomp=TRUE, stratify=TRUE,
#		SL.library=list(g=SL.lib.g, Q=SL.lib.Q))
#saveRDS(psi_00.SL.gcmp, file=paste0("./ltmle-fits/SL_WIDE/psi_00.SL.gcmp-t", .final_t,".RDS"))
#rm(psi_00.SL); gc()
#psi_10.SL.gcmp <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(1,0),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gform, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE, gcomp=TRUE, stratify=TRUE,
#		SL.library=list(g=SL.lib.g, Q=SL.lib.Q))
#saveRDS(psi_10.SL.gcmp, file=paste0("./ltmle-fits/SL_WIDE/psi_10.SL.gcmp-t", .final_t,".RDS"))
#rm(psi_10.SL); gc()
#psi_11.SL.gcmp <- ltmle(
#		data=ltmle.data,
#		abar=rep(c(1,1),.final_t),
#		Anodes=Anodes, Cnodes=Cnodes, Lnodes=Lnodes, Ynodes=Ynodes,
#		deterministic.g.function=MaintainTreatment.2,
#		Qform=Qform, gform=gform, gbounds=.gbounds,
#		estimate.time=FALSE, survivalOutcome=TRUE, gcomp=TRUE, stratify=TRUE,
#		SL.library=list(g=SL.lib.g, Q=SL.lib.Q))
#saveRDS(psi_11.SL.gcmp, file=paste0("./ltmle-fits/SL_WIDE/psi_11.SL.gcmp-t", .final_t,".RDS"))
#rm(psi_11.SL.gcmp); gc()
#
#
#
#
#################################################################################
#################################### RESULTS ####################################
#################################################################################
#
####################
#### EXCEL OUTPUT ##
####################
#estSummary(estimator="Unadjusted", 
#		fileloc = "/Users/tranlm/Dropbox/00-Studies/lrec_shared/results/ltmle-fits",
#		file=paste0("./Results_", .discsize, "day.xlsx"),
#		timepts=4
#)
#estSummary(estimator="GLM_WIDE", 
#		fileloc = "/Users/tranlm/Dropbox/00-Studies/lrec_shared/results/ltmle-fits",
#		file=paste0("./Results_", .discsize, "day.xlsx"),
#		timepts=4
#)
#estSummary(estimator="GLM_LONG", 
#		fileloc = "/Users/tranlm/Dropbox/00-Studies/lrec_shared/results/ltmle-fits",
#		gloc = "/Users/tranlm/Dropbox/00-Studies/lrec_shared/results/g-fits",
#		file=paste0("./Results_", .discsize, "day.xlsx"),
#		timepts=4
#)
#estSummary(estimator="SL_LONG", 
#		fileloc = "/Users/tranlm/Dropbox/00-Studies/lrec_shared/results/ltmle-fits",
#		gloc = "/Users/tranlm/Dropbox/00-Studies/lrec_shared/results/g-fits",
#		file=paste0("./Results_", .discsize, "day.xlsx"),
#		timepts=4
#)
