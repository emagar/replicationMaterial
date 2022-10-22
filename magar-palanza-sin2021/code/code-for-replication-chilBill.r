#####################################
## CODE TO REPLICATE ANALYSIS IN R ##
## Prepared 14-jul-2019            ##
#####################################

# load some required packages
library(lubridate)
library(plyr)

rm(list=ls()) # clean memory
# set your data directory here
datdir <- "/home/eric/Dropbox/data/latAm/chile/replication/data/" 
setwd(datdir)

#####################################################################################
## NO NEED TO RUN SCRIPT data-prep.r IF THE PURPOSE IS PURE REPLICATION OF RESULTS ##
## IN PUBLISHED PAPER. MANIPULATE SCRIPT TO VERIFY/MODIFY OUR CODING CHOICES.      ##
#####################################################################################
# source("../code/data-prep.r")

# load data
load(file = "dataForUrgencyRegressions.RData")       

# explore
ls()
## Objects included: ##
## - bills --- includes the main data for analysis. It is a list of data frames and other lists, each with 6,995 observations sorted by bill (boletin) number. Data frame bills[["info"]] has the dependent and independent variables in the published models. 
## - I --- number of bills for loops
## - allRep --- data frame with a short summary of all committee reports observed. Has the date, the bill number, and the reporting committee. There could be more than one report for bills with multiple committee referrals, and none for unreported bills.  
## - allUrg --- list of data frames summarizing urgencies observed for each bill. Has the urgency type (simple, suma, or discusion inmediata), the date issued, the deadline, whether or not there is a chain of urgency messages, the chamber receiving the message, and the bill number. List element is empty for bills that never became urgent.
## - allVot --- data frame with a short summary of all roll call votes committee reports observed. Has the date, bill number, and the reporting committee. Bills with multiple committee referrals can have more than one report. Unreported bills have none.
## - comPres --- data frame summarizing committee chairs for the period

# recode urgency dummy to control for chamber where it happened
bills$info$nSimpleSen <- bills$info$nSumaSen <- bills$info$nInmedSen <- bills$info$nUrgSen <- bills$info$nSimpleCam <- bills$info$nSumaCam <- bills$info$nInmedCam <- bills$info$nUrgCam <- NA # open slots
sel <- which(bills$info$hasUrg=="yes") # index rows with some urgency
bills$info$nSimpleSen[-sel] <- bills$info$nSumaSen[-sel] <- bills$info$nInmedSen[-sel] <- bills$info$nSimpleCam[-sel] <- bills$info$nSumaCam[-sel] <- bills$info$nInmedCam[-sel] <- 0 # rest have zero urgency
for(i in sel){
    #i <- 6985 # debug
    message(sprintf("loop %s of %s", i, max(sel)))
    tmp <- bills$urg[[i]]
    sel2 <- which(tmp$tramite=="dip"|tmp$tramite=="conf")
    bills$info$nUrgCam[i] <- length(sel2) 
    sel2 <- which( (tmp$tramite=="dip"|tmp$tramite=="conf") & tmp$type=="Simple")
    bills$info$nSimpleCam[i] <- length(sel2) 
    sel2 <- which( (tmp$tramite=="dip"|tmp$tramite=="conf") & tmp$type=="Suma")
    bills$info$nSumaCam[i] <- length(sel2)
    sel2 <- which( (tmp$tramite=="dip"|tmp$tramite=="conf") & tmp$type=="Discusión inmediata")
    bills$info$nInmedCam[i] <- length(sel2) 
    sel2 <- which(tmp$tramite=="sen"|tmp$tramite=="conf")
    bills$info$nUrgSen[i] <- length(sel2) 
    sel2 <- which( (tmp$tramite=="sen"|tmp$tramite=="conf") & tmp$type=="Simple")
    bills$info$nSimpleSen[i] <- length(sel2) 
    sel2 <- which( (tmp$tramite=="sen"|tmp$tramite=="conf") & tmp$type=="Suma")
    bills$info$nSumaSen[i] <- length(sel2)
    sel2 <- which( (tmp$tramite=="sen"|tmp$tramite=="conf") & tmp$type=="Discusión inmediata")
    bills$info$nInmedSen[i] <- length(sel2)
}

# SOME DESCRIPTIVES REPORTED IN PAPER
table(bills$info$dmensaje) # dummy: exec-initiated bills (called mensajes in Chile)
table(bills$info$dpassed[bills$info$dmensaje==1]) # dummy: exec bills passed/not
sum(bills$info$dpassed[bills$info$dmensaje==1]) * 100/ sum(bills$info$dmensaje) # as %
tmp <- bills$info$nSumaCam; tmp[tmp>0] <- 1; table(tmp[bills$info$dmensaje==1]) # <-- exec with at least one 2 wk urgency
table(tmp[bills$info$dmensaje==1])*100 / sum(bills$info$dmensaje) # as %
table(tmp[bills$info$dmensaje==1 & bills$info$dpassed==1]) # <-- exec with at least one 2 wk urgency and passed
table(tmp[bills$info$dmensaje==1 & bills$info$dpassed==1])[2] *100 / table(tmp[bills$info$dmensaje==1])[2] # as % of 2 wk urgent
#
sel <- which(bills$info$dateIn>=dmy("11/3/1998") & bills$info$dateIn<dmy("11/3/2000") & bills$info$dmensaje==1)
tmp <- bills$info[sel,]
nrow(tmp)
sum(tmp$nSumaCam>0)*100 / nrow(tmp)
#
sel <- which(bills$info$dateIn>=dmy("11/3/2000") & bills$info$dateIn<dmy("11/3/2006") & bills$info$dmensaje==1)
tmp <- bills$info[sel,]
nrow(tmp)
sum(tmp$nSumaCam>0)*100 / nrow(tmp)
#
sel <- which(bills$info$dateIn>=dmy("11/3/2006") & bills$info$dateIn<dmy("11/3/2010") & bills$info$dmensaje==1)
tmp <- bills$info[sel,]
nrow(tmp)
sum(tmp$nSumaCam>0)*100 / nrow(tmp)
#
sel <- which(bills$info$dateIn>=dmy("11/3/2010") & bills$info$dateIn<dmy("11/3/2014") & bills$info$dmensaje==1)
tmp <- bills$info[sel,]
nrow(tmp)
sum(tmp$nSumaCam>0)*100 / nrow(tmp)
#
sel <- which(bills$info$dateIn>=dmy("11/3/1998") & bills$info$dateIn<dmy("11/3/2014") & bills$info$dmensaje==1)
tmp <- bills$info[sel,]
nrow(tmp)
sum(tmp$nSumaCam>0)*100 / nrow(tmp)


###############
# SUBSET DATA #
###############
sel <- which(bills$info$dateIn>=dmy("11/3/1998") & bills$info$dateIn<dmy("11/3/2014") & bills$info$dmensaje==1)
dat <- bills$info[sel,]
tmp.importancia <- bills$importancia[sel,]
#
# Add president's maj status in chamber
dat$dmajDip <- dat$dmajSen <- 0
# sen
tmp <- dmy(c("11-03-1990", "22-01-1999", "11-03-2000", "11-01-2002", "27-01-2005", "30-08-2005", "11-03-2006", "11-03-2010") )
sel <- which(dat$dateIn >= tmp[1] & dat$dateIn < tmp[2]); dat$dmajSen[sel] <- 0
sel <- which(dat$dateIn >= tmp[2] & dat$dateIn < tmp[3]); dat$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(dat$dateIn >= tmp[3] & dat$dateIn < tmp[4]); dat$dmajSen[sel] <- 1 
sel <- which(dat$dateIn >= tmp[4] & dat$dateIn < tmp[5]); dat$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(dat$dateIn >= tmp[5] & dat$dateIn < tmp[6]); dat$dmajSen[sel] <- 0
sel <- which(dat$dateIn >= tmp[6] & dat$dateIn < tmp[7]); dat$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(dat$dateIn >= tmp[7] & dat$dateIn < tmp[8]); dat$dmajSen[sel] <- 1
sel <- which(dat$dateIn >= tmp[8] & dat$dateIn < tmp[9]); dat$dmajSen[sel] <- 0
#
# dip: always maj=1 except jan-march 2010, when PRI left Concertación (2010-14 50%, coded 1)
dat$dmajDip <- 1
dat$dmajDip[which(dat$dateIn>=dmy("28-1-2010") & dat$dateIn<dmy("1-3-2010"))] <- 0 # but no bills!
#
# to end of pdtl term
tmp <- dmy(c("11-03-1994", "11-03-2000", "11-03-2006", "11-03-2010", "11-03-2014") )
dat$pterm <- NA
sel <- which(                       dat$dateIn < tmp[1]); dat$pterm[sel] <- round((tmp[1] - dat$dateIn[sel]) * 100 / (365*4), digits = 0)
sel <- which(dat$dateIn >= tmp[1] & dat$dateIn < tmp[2]); dat$pterm[sel] <- round((tmp[2] - dat$dateIn[sel]) * 100 / (365*6), digits = 0)
sel <- which(dat$dateIn >= tmp[2] & dat$dateIn < tmp[3]); dat$pterm[sel] <- round((tmp[3] - dat$dateIn[sel]) * 100 / (365*6), digits = 0)
sel <- which(dat$dateIn >= tmp[3] & dat$dateIn < tmp[4]); dat$pterm[sel] <- round((tmp[4] - dat$dateIn[sel]) * 100 / (365*4), digits = 0)
sel <- which(dat$dateIn >= tmp[4] & dat$dateIn < tmp[5]); dat$pterm[sel] <- round((tmp[5] - dat$dateIn[sel]) * 100 / (365*4), digits = 0)
#
# to end of dip term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
dat$dterm <- NA
sel <- which(                            dat$dateIn < tmp[1]); dat$dterm[sel] <- round((tmp[1] - dat$dateIn[sel]) * 100 / (365*4), digits = 0)
sel <- which(dat$dateIn >= tmp[1] & dat$dateIn < tmp[2]); dat$dterm[sel] <- round((tmp[2] - dat$dateIn[sel]) * 100 / (365*4), digits = 0)
sel <- which(dat$dateIn >= tmp[2] & dat$dateIn < tmp[3]); dat$dterm[sel] <- round((tmp[3] - dat$dateIn[sel]) * 100 / (365*4), digits = 0)
sel <- which(dat$dateIn >= tmp[3] & dat$dateIn < tmp[4]); dat$dterm[sel] <- round((tmp[4] - dat$dateIn[sel]) * 100 / (365*4), digits = 0)
sel <- which(dat$dateIn >= tmp[4] & dat$dateIn < tmp[5]); dat$dterm[sel] <- round((tmp[5] - dat$dateIn[sel]) * 100 / (365*4), digits = 0)
sel <- which(dat$dateIn >= tmp[5] & dat$dateIn < tmp[6]); dat$dterm[sel] <- round((tmp[6] - dat$dateIn[sel]) * 100 / (365*4), digits = 0)
#
# to end of sen term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2006", "11-03-2014") )
dat$sterm <- NA
sel <- which(                          dat$dateIn < tmp[1]); dat$sterm[sel] <- round((tmp[1] - dat$dateIn[sel]) * 100 / (365*8), digits = 0)
sel <- which(dat$dateIn >= tmp[1] & dat$dateIn < tmp[2]); dat$sterm[sel] <- round((tmp[2] - dat$dateIn[sel]) * 100 / (365*8), digits = 0)
sel <- which(dat$dateIn >= tmp[2] & dat$dateIn < tmp[3]); dat$sterm[sel] <- round((tmp[3] - dat$dateIn[sel]) * 100 / (365*8), digits = 0)
sel <- which(dat$dateIn >= tmp[3] & dat$dateIn < tmp[4]); dat$sterm[sel] <- round((tmp[4] - dat$dateIn[sel]) * 100 / (365*8), digits = 0)
#
# to end of leg year
tmp <- dmy(c("10-03-1999", "10-03-2000", "10-03-2001", "10-03-2002", "10-03-2003", "10-03-2004", "10-03-2005", "10-03-2006", "10-03-2007", "10-03-2008", "10-03-2009", "10-03-2010", "10-03-2011", "10-03-2012", "10-03-2013", "10-03-2014") )
dat$legyr <- NA
sel <- which(                           dat$dateIn < tmp[1]);  dat$legyr[sel] <- round((tmp[1]  - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[1]  & dat$dateIn < tmp[2]);  dat$legyr[sel] <- round((tmp[2]  - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[2]  & dat$dateIn < tmp[3]);  dat$legyr[sel] <- round((tmp[3]  - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[3]  & dat$dateIn < tmp[4]);  dat$legyr[sel] <- round((tmp[4]  - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[4]  & dat$dateIn < tmp[5]);  dat$legyr[sel] <- round((tmp[5]  - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[5]  & dat$dateIn < tmp[6]);  dat$legyr[sel] <- round((tmp[6]  - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[6]  & dat$dateIn < tmp[7]);  dat$legyr[sel] <- round((tmp[7]  - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[7]  & dat$dateIn < tmp[8]);  dat$legyr[sel] <- round((tmp[8]  - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[8]  & dat$dateIn < tmp[9]);  dat$legyr[sel] <- round((tmp[9]  - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[9]  & dat$dateIn < tmp[10]); dat$legyr[sel] <- round((tmp[10] - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[10] & dat$dateIn < tmp[11]); dat$legyr[sel] <- round((tmp[11] - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[11] & dat$dateIn < tmp[12]); dat$legyr[sel] <- round((tmp[12] - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[12] & dat$dateIn < tmp[13]); dat$legyr[sel] <- round((tmp[13] - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[13] & dat$dateIn < tmp[14]); dat$legyr[sel] <- round((tmp[14] - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[14] & dat$dateIn < tmp[15]); dat$legyr[sel] <- round((tmp[15] - dat$dateIn[sel]) * 100 / 365, digits = 0)
sel <- which(dat$dateIn >= tmp[15] & dat$dateIn < tmp[16]); dat$legyr[sel] <- round((tmp[16] - dat$dateIn[sel]) * 100 / 365, digits = 0)
#
# for leg year dummies
tmp <- dmy(c("10-03-1999", "10-03-2000", "10-03-2001", "10-03-2002", "10-03-2003", "10-03-2004", "10-03-2005", "10-03-2006", "10-03-2007", "10-03-2008", "10-03-2009", "10-03-2010", "10-03-2011", "10-03-2012", "10-03-2013", "10-03-2014") )
dat$yr <- NA
sel <- which(                           dat$dateIn < tmp[1]);  dat$yr[sel] <- 1999
sel <- which(dat$dateIn >= tmp[1]  & dat$dateIn < tmp[2]);  dat$yr[sel] <- 2000
sel <- which(dat$dateIn >= tmp[2]  & dat$dateIn < tmp[3]);  dat$yr[sel] <- 2001
sel <- which(dat$dateIn >= tmp[3]  & dat$dateIn < tmp[4]);  dat$yr[sel] <- 2002
sel <- which(dat$dateIn >= tmp[4]  & dat$dateIn < tmp[5]);  dat$yr[sel] <- 2003
sel <- which(dat$dateIn >= tmp[5]  & dat$dateIn < tmp[6]);  dat$yr[sel] <- 2004
sel <- which(dat$dateIn >= tmp[6]  & dat$dateIn < tmp[7]);  dat$yr[sel] <- 2005
sel <- which(dat$dateIn >= tmp[7]  & dat$dateIn < tmp[8]);  dat$yr[sel] <- 2006
sel <- which(dat$dateIn >= tmp[8]  & dat$dateIn < tmp[9]);  dat$yr[sel] <- 2007
sel <- which(dat$dateIn >= tmp[9]  & dat$dateIn < tmp[10]); dat$yr[sel] <- 2008
sel <- which(dat$dateIn >= tmp[10] & dat$dateIn < tmp[11]); dat$yr[sel] <- 2009
sel <- which(dat$dateIn >= tmp[11] & dat$dateIn < tmp[12]); dat$yr[sel] <- 2010
sel <- which(dat$dateIn >= tmp[12] & dat$dateIn < tmp[13]); dat$yr[sel] <- 2011
sel <- which(dat$dateIn >= tmp[13] & dat$dateIn < tmp[14]); dat$yr[sel] <- 2012
sel <- which(dat$dateIn >= tmp[14] & dat$dateIn < tmp[15]); dat$yr[sel] <- 2013
sel <- which(dat$dateIn >= tmp[15] & dat$dateIn < tmp[16]); dat$yr[sel] <- 2014
#
# for year 1,2,3,4 dummies
tmp <- dmy(c("10-03-1999", "10-03-2000", "10-03-2001", "10-03-2002", "10-03-2003", "10-03-2004", "10-03-2005", "10-03-2006", "10-03-2007", "10-03-2008", "10-03-2009", "10-03-2010", "10-03-2011", "10-03-2012", "10-03-2013", "10-03-2014") )
dat$yr14 <- NA
sel <- which(                           dat$dateIn < tmp[1]);  dat$yr14[sel] <- 5
sel <- which(dat$dateIn >= tmp[1]  & dat$dateIn < tmp[2]);  dat$yr14[sel] <- 6
sel <- which(dat$dateIn >= tmp[2]  & dat$dateIn < tmp[3]);  dat$yr14[sel] <- 1
sel <- which(dat$dateIn >= tmp[3]  & dat$dateIn < tmp[4]);  dat$yr14[sel] <- 2
sel <- which(dat$dateIn >= tmp[4]  & dat$dateIn < tmp[5]);  dat$yr14[sel] <- 3
sel <- which(dat$dateIn >= tmp[5]  & dat$dateIn < tmp[6]);  dat$yr14[sel] <- 4
sel <- which(dat$dateIn >= tmp[6]  & dat$dateIn < tmp[7]);  dat$yr14[sel] <- 5
sel <- which(dat$dateIn >= tmp[7]  & dat$dateIn < tmp[8]);  dat$yr14[sel] <- 6
sel <- which(dat$dateIn >= tmp[8]  & dat$dateIn < tmp[9]);  dat$yr14[sel] <- 1
sel <- which(dat$dateIn >= tmp[9]  & dat$dateIn < tmp[10]); dat$yr14[sel] <- 2
sel <- which(dat$dateIn >= tmp[10] & dat$dateIn < tmp[11]); dat$yr14[sel] <- 3
sel <- which(dat$dateIn >= tmp[11] & dat$dateIn < tmp[12]); dat$yr14[sel] <- 4
sel <- which(dat$dateIn >= tmp[12] & dat$dateIn < tmp[13]); dat$yr14[sel] <- 1
sel <- which(dat$dateIn >= tmp[13] & dat$dateIn < tmp[14]); dat$yr14[sel] <- 2
sel <- which(dat$dateIn >= tmp[14] & dat$dateIn < tmp[15]); dat$yr14[sel] <- 3
sel <- which(dat$dateIn >= tmp[15] & dat$dateIn < tmp[16]); dat$yr14[sel] <- 4
#
# legislature dummies (periodo)
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
dat$dleg90 <- dat$dleg94 <- dat$dleg98 <- dat$dleg02 <- dat$dleg06 <- dat$dleg10 <- dat$legis <- 0
sel <- which(                          dat$dateIn < tmp[1]); dat$dleg90[sel] <- 1; dat$legis[sel] <- 1990
sel <- which(dat$dateIn >= tmp[1] & dat$dateIn < tmp[2]); dat$dleg94[sel] <- 1; dat$legis[sel] <- 1994
sel <- which(dat$dateIn >= tmp[2] & dat$dateIn < tmp[3]); dat$dleg98[sel] <- 1; dat$legis[sel] <- 1998
sel <- which(dat$dateIn >= tmp[3] & dat$dateIn < tmp[4]); dat$dleg02[sel] <- 1; dat$legis[sel] <- 2002
sel <- which(dat$dateIn >= tmp[4] & dat$dateIn < tmp[5]); dat$dleg06[sel] <- 1; dat$legis[sel] <- 2006
sel <- which(dat$dateIn >= tmp[5] & dat$dateIn < tmp[6]); dat$dleg10[sel] <- 1; dat$legis[sel] <- 2010
#
# bill introduced after ley orgánica relaxed urgency deadlines
dat$dreform2010 <- 0
sel <- which(dat$dateIn >= dmy("1-7-2010")); dat$dreform2010[sel] <- 1
#
# prepare dependent variable (analysis uses dv2 only, rest redundant here)
dat$dv   <- 0; dat$dv[dat$nUrgCam>0] <- 1
dat$dv1  <- 0; dat$dv1[dat$nInmedCam>0] <- 1
dat$dv2  <- 0; dat$dv2[dat$nSumaCam>0] <- 1
dat$dv12 <- 0; dat$dv12[dat$nInmedCam>0 | dat$nSumaCam>0] <- 1
dat$dv3  <- 0; dat$dv3[dat$nSimpleCam>0] <- 1
#
dat$dinSen <- 0; dat$dinSen[dat$init=="sen"] <- 1
#
dat$ptermR     <- c(scale(dat$pterm))     # rescale/center continuous variables to aid identification of multilevel model ## c() removes class problems for predict() below
dat$legyrR     <- c(scale(dat$legyr))     # rescale/center continuous variables to aid identification of multilevel model
dat$legyrR2    <- dat$legyrR^2            # square
dat$netApprovR <- c(scale(dat$netApprov)) # rescale/center continuous variables to aid identification of multilevel model
#
## # recode dsameCoal = 0 if sdamePres = 1
## sel <- which(dat$dsamePty==1 & dat$dsameCoal==1)
## dat$dsameCoal[sel] <- 0
colnames(dat)

##################
# select DV here #
##################
#dat$dv <- dat$dv1 # discusión inmediata only
dat$dv <- dat$dv2 # suma urgencia only ~~~~  USED IN PAPER  ~~~~
#dat$dv <- dat$dv12 # discusión inmediata and urgencia suma

# Describe dv by pdt/yr
dat$pdtyr <- NA
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-1998") & ymd(dat$dateIn) < dmy("11-3-1999")] <- "a. Frei-5"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-1999") & ymd(dat$dateIn) < dmy("11-3-2000")] <- "a. Frei-6"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2000") & ymd(dat$dateIn) < dmy("11-3-2001")] <- "b. Lagos-1"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2001") & ymd(dat$dateIn) < dmy("11-3-2002")] <- "b. Lagos-2"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2002") & ymd(dat$dateIn) < dmy("11-3-2003")] <- "b. Lagos-3"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2003") & ymd(dat$dateIn) < dmy("11-3-2004")] <- "b. Lagos-4"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2004") & ymd(dat$dateIn) < dmy("11-3-2005")] <- "b. Lagos-5"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2005") & ymd(dat$dateIn) < dmy("11-3-2006")] <- "b. Lagos-6"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2006") & ymd(dat$dateIn) < dmy("11-3-2007")] <- "c. Bachelet-1"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2007") & ymd(dat$dateIn) < dmy("11-3-2008")] <- "c. Bachelet-2"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2008") & ymd(dat$dateIn) < dmy("11-3-2009")] <- "c. Bachelet-3"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2009") & ymd(dat$dateIn) < dmy("11-3-2010")] <- "c. Bachelet-4"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2010") & ymd(dat$dateIn) < dmy("11-3-2011")] <- "d. Piñera-1"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2011") & ymd(dat$dateIn) < dmy("11-3-2012")] <- "d. Piñera-2"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2012") & ymd(dat$dateIn) < dmy("11-3-2013")] <- "d. Piñera-3"
dat$pdtyr[ymd(dat$dateIn) >= dmy("11-3-2013") & ymd(dat$dateIn) < dmy("11-3-2014")] <- "d. Piñera-4"
tmp <- table(dat$pdtyr, dat$dv)
tmp <- rbind(tmp, colSums(tmp))
tmp2 <- data.frame(supUrg=tmp[,2]*100 / rowSums(tmp), not=tmp[,1]*100/rowSums(tmp), tot=100, n=rowSums(tmp))
round(tmp2,0)

# Describe dv by president
dat$pdt <- NA
dat$pdt[ymd(dat$dateIn) >= dmy("11-3-1998") & ymd(dat$dateIn) < dmy("11-3-2000")] <- "a. Frei"
dat$pdt[ymd(dat$dateIn) >= dmy("11-3-2000") & ymd(dat$dateIn) < dmy("11-3-2006")] <- "b. Lagos"
dat$pdt[ymd(dat$dateIn) >= dmy("11-3-2006") & ymd(dat$dateIn) < dmy("11-3-2010")] <- "c. Bachelet"
dat$pdt[ymd(dat$dateIn) >= dmy("11-3-2010") & ymd(dat$dateIn) < dmy("11-3-2014")] <- "d. Piñera"
tmp <- table(dat$pdt, dat$dv)
tmp <- rbind(tmp, colSums(tmp))
tmp2 <- data.frame(supUrg=tmp[,2]*100/rowSums(tmp), not=tmp[,1]*100/rowSums(tmp), tot=100, n=rowSums(tmp))
round(tmp2,0)

#############################################
# LOGIT ON WHETHER NOT BILL DECLARED URGENT #
#############################################
# FIT MODELS
fit1e <- glm(dv ~ dsamePty  + dmultiRef +         + drefHda + dmajSen + dinSen + legyrR + legyrR2 + dreform2010 + netApprovR                   , data = dat, family = binomial(link = logit))
fit2e <- glm(dv ~ dsameCoal + dmultiRef           + drefHda + dmajSen + dinSen + legyrR + legyrR2 + dreform2010 + netApprovR                   , data = dat, family = binomial(link = logit))
fit3e <- glm(dv ~ dsameCoal + dmultiRef           + drefHda           + dinSen + legyrR + legyrR2               + netApprovR + as.factor(legis), data = dat, family = binomial(link = logit))
# multilevel version with error terms clustered by legislatura (Gelman Hill p. 302 from eq 12.13)
# For non-convergence warnings in glmer, see script nonConv.r. This is from https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html (and related Stack overflow http://stackoverflow.com/questions/23478792/warning-messages-when-trying-to-run-glmer-in-r). Re-scaling all continuous ivs took care of the problem...
library(lme4)
fit4e <- glmer(dv ~ dsameCoal + dmultiRef           + drefHda         + dinSen + legyrR + legyrR2            + netApprovR + (1|legis), data = dat, family = binomial(link ="logit"))
#
summary(fit1e)
summary(fit2e)
summary(fit3e)
summary(fit4e)
library(arm)
display(fit4e)

# log likelihood
logLik(fit1e)
logLik(fit2e)
logLik(fit3e)
logLik(fit4e)

# n
nobs(fit1e)
nobs(fit2e)
nobs(fit3e)
nobs(fit4e)

# descriptives of variables in model
summary(dat[, c("legyr", "netApprov")])
sd(dat$legyr)
sd(dat$netApprov)
#
table(dat$legis)
round(table(dat$legis)/1467, 3)

## GRAFICA MATRIZ DE CORRELACIONES: COOL!
#tmp <- cor(dat[,-grep("Extend|Shorten|Retire|d2010|nses", colnames(dat))])
tmp <- dat[, grep("samePty|sameCoal|dmultiRef|drefHda|dmajSen|dinSen|ptermR|legyrR|dreform2010|netApprovR|legis", colnames(dat))]
tmp$leg98 <- as.numeric(tmp$legis==1998)
tmp$leg02 <- as.numeric(tmp$legis==2002)
tmp$leg06 <- as.numeric(tmp$legis==2006)
tmp$leg10 <- as.numeric(tmp$legis==2010)
tmp$legis <- NULL
tmp <- cor(tmp)
library('corrplot')   # plots correlation matrix!
corrplot(tmp, color = TRUE) #plot matrix
#corrplot(tmp, method = "circle") #plot matrix
#
# descriptives
# continuous variables
tmp <- data.frame()
tmp <- rbind(tmp, summary(dat$pterm[dat$dmensaje==1]))
tmp <- rbind(tmp, summary(dat$legyr[dat$dmensaje==1]))
tmp <- rbind(tmp, summary(dat$netApprov[dat$dmensaje==1]))
tmp <- cbind(c("Presidential term", "Year remaining", "Pres.~approval"), tmp)
colnames(tmp) <- c("Var.", "Min.","Q1","Med.","Mean","Q3","Max.")
tmp$sd <- c(sd(dat$pterm[dat$dmensaje==1]), sd(dat$legyr[dat$dmensaje==1]), sd(dat$netApprov[dat$dmensaje==1]))
tmp

tmp <- data.frame()
tmp <- rbind(tmp, table(dat$dv[dat$dmensaje==1]))
tmp <- rbind(tmp, round(table(dat$dv)/nrow(dat), 3))
tmp <- rbind(tmp, table(dat$dsamePty[dat$dmensaje==1]))
tmp <- rbind(tmp, round(table(dat$dsamePty)/nrow(dat), 3))
tmp <- rbind(tmp, table(dat$dsameCoal[dat$dmensaje==1]))
tmp <- rbind(tmp, round(table(dat$dsameCoal)/nrow(dat), 3))
tmp <- rbind(tmp, table(dat$dmultiRef[dat$dmensaje==1]))
tmp <- rbind(tmp, round(table(dat$dmultiRef)/nrow(dat), 3))
tmp <- rbind(tmp, table(dat$drefHda[dat$dmensaje==1]))
tmp <- rbind(tmp, round(table(dat$drefHda)/nrow(dat), 3))
tmp <- rbind(tmp, table(dat$dmajSen[dat$dmensaje==1]))
tmp <- rbind(tmp, round(table(dat$dmajSen)/nrow(dat), 3))
tmp <- rbind(tmp, table(dat$dinSen[dat$dmensaje==1]))
tmp <- rbind(tmp, round(table(dat$dinSen)/nrow(dat), 3))
tmp <- rbind(tmp, table(dat$dreform2010[dat$dmensaje==1]))
tmp <- rbind(tmp, round(table(dat$dreform2010)/nrow(dat), 3))
tmp$tot <- rowSums(tmp)
tmp <- cbind(c("dv", "dv", "dsamePty", "dsamePty", "dsameCoal", "dsameCoal", "dmultiRef", "dmultiRef", "drefHda", "drefHda", "dmajSen", "dmajSen", "dinSen", "dinSen", "dreform2010", "dreform2010"), tmp)
colnames(tmp) <- c("var","zero","one","sum")
tmp
#
cor(dat$dsamePty, dat$dmultiRef)

# LR tests of overall model fit (vs. intercept-only model, see http://www.ats.ucla.edu/stat/r/dae/logit.htm)
with(fit1e, null.deviance - deviance)
with(fit1e, df.null - df.residual)
with(fit1e, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#
with(fit2e, null.deviance - deviance)
with(fit2e, df.null - df.residual)
with(fit2e, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#
with(fit3e, null.deviance - deviance)
with(fit3e, df.null - df.residual)
with(fit3e, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#
# LR test not reliable for mixed model, bootsrapping preferable but not straightforward, see http://glmm.wikidot.com/faq
#
# percent correctly predicted
sel <- which(dat$dmensaje==1)
pred1 <- predict(fit1e, type = "response"); pred1[pred1>=.5] <- 1; pred1[pred1<.5] <- 0
table(dat$dv[sel] - pred1) / nrow(dat) # pct correctly predicted
pred2 <- predict(fit2e, type = "response"); pred2[pred2>=.5] <- 1; pred2[pred2<.5] <- 0
table(dat$dv[sel] - pred2) / nrow(dat) # pct correctly predicted
pred3 <- predict(fit3e, type = "response"); pred3[pred3>=.5] <- 1; pred3[pred3<.5] <- 0
table(dat$dv[sel] - pred3) / nrow(dat) # pct correctly predicted
pred4 <- predict(fit4e, type = "response"); pred4[pred4>=.5] <- 1; pred4[pred4<.5] <- 0
table(dat$dv[sel] - pred4) / nrow(dat) # pct correctly predicted

# predict probabilities for specific scenarios
tmp <- data.frame(
    Intercept = 1,
    dsameCoal = median(dat$dsamePty),
    dmultiRef = median(dat$dmultiRef), 
    drefHda      = 1, #median(dat$drefHda),
    dmajSen      = median(dat$dmajSen),
    dinSen       = median(dat$dinSen),
    legyrR       = median(dat$legyrR),
    legyrR2      = (median(dat$legyrR))^2,
    dreform2010  = median(dat$dreform2010),
    netApprovR   = median(dat$netApprovR)
    )
predict(fit2, tmp, type = "response") 
exp(sum(coefficients(fit2)*tmp)) / (1 + exp(sum(coefficients(fit2)*tmp))) # probability urg=1


# export to latex
library(stargazer)
stargazer(fit1e, fit2e, fit3e, fit4e, title="Regression results", align=TRUE, report = ('vc*p')#,
 ##          covariate.labels=
 ## c("Copartisan comm.~chair",
 ##   "Coalition comm.~chair",
 ##   "Multiple referrals",
 ##   "Hacienda referral",
 ##   "Senate majority",
 ##   "Introduced Senate",
 ##   "Year remaining",
 ##   "Relax deadlines",
 ##   "Pres.~approval",
 ##   "2002-06 Leg.",
 ##   "2006-10 Leg.",
 ##   "2010-14 Leg.",
 ##   "Constant")
          )

## library(apsrtable)
## apsrtable(fit1, fit2)

############################
# Average marginal effects #
############################
library(margins)
mar3 <- margins(fit3e)
mar3 <- summary(mar3)
tmp <-  c(5,  2,3,1,8, 9,10,6,7,4); mar3 <- mar3[order(tmp),] # sort rows so coefs appear in same order as in table   *w/o dmocion*
mar3
tmp <- c("Coal. comm. chair",
         "Multiple referrals",
         "Hacienda referral",
         "Pres. approval",
         "Introd. in Senate",
         "Year remaining",
         "Sq. year remaining",
         "2002-06",
         "2006-10",
         "2010-14")
#library(ggplot2) -- for ggplot see https://www.r-bloggers.com/probitlogit-marginal-effects-in-r/
gr <- "../graphs/"
## #pdf (file = paste(gr, "avgMgEffects.pdf", sep = ""), width = 7, height = 5)
## par(mar=c(4,2,2,2)+0.1) # drop title space and left space
## plot(x=c(-.35,.15),
##      y=-c(1,nrow(mar3)),
##      type="n", axes = FALSE,
##      xlab = "Average marginal effect",
##      ylab = "")
## abline(v=seq(-.25, .15, .05), col = "gray70")
## abline(v=0, lty=2)
## axis(1, at = seq(-.25, .15, .05), labels = FALSE)
## axis(1, at = seq(-.2, .1, .1))
## for (i in c(-1:-nrow(mar3))){
##     points(y=i, x=mar3$AME[-i], pch=20, cex=1.5, col = "black")
##     lines(y=rep(i, 2), x=c(mar3$lower[-i],mar3$upper[-i]), lwd = 2, col = "black")
## }
## #mar3$factor
## text(x=-.375, y=-1:-nrow(mar3), labels=tmp, pos=4)
## #dev.off()
#
#pdf (file = paste(gr, "avgMgEffects.pdf", sep = ""), width = 7, height = 5)
par(mar=c(4,2,2,2)+0.1) # drop title space and left space
plot(x=c(-.4,.35),
     y=-c(1,nrow(mar3)),
     type="n", axes = FALSE,
     xlab = "Average marginal effect",
     ylab = "")
abline(v=seq(-.2, .35, .05), col = "gray70")
abline(v=0, lty=2)
abline(h=seq(-1,-nrow(mar3),-1), col = "gray70")
axis(1, at = seq(-.2, .35, .05), labels = FALSE)
axis(1, at = seq(-.2, .3, .1))
for (i in c(-1:-nrow(mar3))){
    points(y=i, x=mar3$AME[-i], pch=20, cex=1.5, col = "black")
    lines(y=rep(i, 2), x=c(mar3$lower[-i],mar3$upper[-i]), lwd = 2, col = "black")
}
#mar3$factor
polygon(x= c(-.45,-.22,-.22,-.45), y=c(-11,-11,0,0), col = "white", border = "white")
text(x=-.425, y=-1:-nrow(mar3), labels=tmp, pos=4)
#dev.off()

###############################################################
# For appendix: models 2 and 3 for bill subsets by issue area #
###############################################################
#
# model2e for subsets of bills 
dat$domn <- as.numeric(gsub(pattern = "0*([0-9]+)[-].*", replacement = "\\1", dat$dom, perl = TRUE))
table(dat$dom, dat$dsameCoal) # too few zeroes in many subsets
table(dat$materiaCat, dat$dsameCoal ) # too few zeroes in many subsets
#
# merge some categories
dat$matCat <- dat$materiaCat
dat$matCat[dat$matCat=="Cobre" | dat$matCat=="Recaudación"] <- "Cobre y recaudación"
dat$matCat[dat$matCat=="Convenios intl" | dat$matCat=="RR.EE."] <- "RR.EE. convenios"
table(dat$matCat, dat$dsameCoal )
#
table(dat$matCat, dat$dsameCoal)
#
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dmajSen + dinSen + legyrR + legyrR2 + dreform2010 + netApprovR, data = dat, subset = (domn==1|domn==12|domn==21), family = binomial(link = logit));tmp->fit2e011221
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dmajSen + dinSen + legyrR + legyrR2 + dreform2010 + netApprovR, data = dat, subset = (domn==3|domn==5|domn==8)  , family = binomial(link = logit));tmp->fit2e030508
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dmajSen + dinSen + legyrR + legyrR2 + dreform2010 + netApprovR, data = dat, subset = domn==10                   , family = binomial(link = logit));tmp->fit2e10
#
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dmajSen + dinSen + legyrR + legyrR2 + dreform2010 + netApprovR, data = dat, subset = (matCat=="Agricultura/medio ambiente"), family = binomial(link = logit));tmp->fit2eAgri
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dmajSen + dinSen + legyrR + legyrR2 + dreform2010 + netApprovR, data = dat, subset = (matCat=="Comercio"), family = binomial(link = logit));tmp->fit2eComer
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dmajSen + dinSen + legyrR + legyrR2 + dreform2010 + netApprovR, data = dat, subset = (matCat=="RR.EE. convenios"), family = binomial(link = logit));tmp->fit2eRREE
#
# model3e for subsets of bills 
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dinSen + legyrR + legyrR2 + netApprovR + as.factor(legis), data = dat, subset = (domn==1|domn==12|domn==21), family = binomial(link = logit));tmp->fit3e011221
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dinSen + legyrR + legyrR2 + netApprovR + as.factor(legis), data = dat, subset = (domn==3|domn==5|domn==8)  , family = binomial(link = logit));tmp->fit3e030508
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dinSen + legyrR + legyrR2 + netApprovR + as.factor(legis), data = dat, subset = (domn==10)                   , family = binomial(link = logit));tmp->fit3e10
#
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dinSen + legyrR + legyrR2 + netApprovR + as.factor(legis), data = dat, subset = (matCat=="Agricultura/medio ambiente"), family = binomial(link = logit));tmp->fit3eAgri
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dinSen + legyrR + legyrR2 + netApprovR + as.factor(legis), data = dat, subset = (matCat=="Comercio"), family = binomial(link = logit));tmp->fit3eComer
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dinSen + legyrR + legyrR2 + netApprovR + as.factor(legis), data = dat, subset = (matCat=="RR.EE. convenios"), family = binomial(link = logit));tmp->fit3eRREE
#summary(tmp)
#
##### coef(tmp)["dsameCoal"]
##### summary(tmp)$coefficients[2,1]
##### summary(tmp)$coefficients[2,2]
#
#################################################
# Average marginal effects for subsets of bills #
#################################################
library(margins)
res <- data.frame()
#
mar2 <- margins(fit2e)
mar2 <- summary(mar2)
res <- rbind(res, mar2[mar2$factor=="dsameCoal",])
#
mar2 <- margins(fit2e011221)
mar2 <- summary(mar2)
res <- rbind(res, mar2[mar2$factor=="dsameCoal",])
#
mar2 <- margins(fit2e030508)
mar2 <- summary(mar2)
res <- rbind(res, mar2[mar2$factor=="dsameCoal",])
#
mar2 <- margins(fit2e10)
mar2 <- summary(mar2)
res <- rbind(res, mar2[mar2$factor=="dsameCoal",])
#
mar2 <- margins(fit2eAgri)
mar2 <- summary(mar2)
res <- rbind(res, mar2[mar2$factor=="dsameCoal",])
#
mar2 <- margins(fit2eComer)
mar2 <- summary(mar2)
res <- rbind(res, mar2[mar2$factor=="dsameCoal",])
#
mar2 <- margins(fit2eRREE)
mar2 <- summary(mar2)
res <- rbind(res, mar2[mar2$factor=="dsameCoal",])
#
mar3 <- margins(fit3e)
mar3 <- summary(mar3)
res <- rbind(res, mar3[mar3$factor=="dsameCoal",])
#
mar3 <- margins(fit3e011221)
mar3 <- summary(mar3)
res <- rbind(res, mar3[mar3$factor=="dsameCoal",])
#
mar3 <- margins(fit3e030508)
mar3 <- summary(mar3)
res <- rbind(res, mar3[mar3$factor=="dsameCoal",])
#
mar3 <- margins(fit3e10)
mar3 <- summary(mar3)
res <- rbind(res, mar3[mar3$factor=="dsameCoal",])
#
mar3 <- margins(fit3eAgri)
mar3 <- summary(mar3)
res <- rbind(res, mar3[mar3$factor=="dsameCoal",])
#
mar3 <- margins(fit3eComer)
mar3 <- summary(mar3)
res <- rbind(res, mar3[mar3$factor=="dsameCoal",])
#
mar3 <- margins(fit3eRREE)
mar3 <- summary(mar3)
res <- rbind(res, mar3[mar3$factor=="dsameCoal",])
#
subset <- c("all2","bol-agric-mmaa2","bol-eco-cobre2","bol-rree2","mat-agric-mmaa2","mat-exports2","mat-rree-convenio2",
            "all3","bol-agric-mmaa3","bol-eco-cobre3","bol-rree3","mat-agric-mmaa3","mat-exports3","mat-rree-convenio3")
subset2 <- c("All bills","Agric. (bol.)","Mining-taxes (bol.)","Foreign aff. (bol.)","Agric. (mat.)","Exports (mat.)","Foreign aff. (mat.)",
            "All bills","Agric. (bol.)","Mining-taxes (bol.)","Foreign aff. (bol.)","Agric. (mat.)","Exports (mat.)","Foreign aff. (mat.)")
n <- c(nobs(fit2e), nobs(fit2e011221), nobs(fit2e020508), nobs(fit2e10), nobs(fit2eAgri), nobs(fit2eComer), nobs(fit2eRREE),
       nobs(fit3e), nobs(fit3e011221), nobs(fit3e030508), nobs(fit3e10), nobs(fit3eAgri), nobs(fit3eComer), nobs(fit3eRREE))
#
res <- as.data.frame(res)
res[,1] <- subset2
res$n <- n
res[,2:7] <- round(res[,2:7],3)
colnames(res)[1] <- "subset"
res
#

##########################################################
## model 3 using vale's importancia for bachelet piñera ##
##########################################################
table(dat$materiaCat[ymd(dat$dateIn)>=dmy("11-3-2006")], tmp.importancia$importancia[ymd(dat$dateIn)>=dmy("11-3-2006")], useNA = "always")
table(tmp.importancia$importancia[ymd(dat$dateIn)>=dmy("11-3-2006")], useNA = "always")
#
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dinSen + legyrR + legyrR2 + netApprovR + as.factor(legis), data = dat, subset = (tmp.importancia$importancia==1), family = binomial(link = logit));tmp->fit3eLow
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dinSen + legyrR + legyrR2 + netApprovR + as.factor(legis), data = dat, subset = (tmp.importancia$importancia==2), family = binomial(link = logit));tmp->fit3eMid
tmp <- glm(dv ~ dsameCoal + dmultiRef + drefHda + dinSen + legyrR + legyrR2 + netApprovR + as.factor(legis), data = dat, subset = (tmp.importancia$importancia!=1), family = binomial(link = logit));tmp->fit3eMidHi
#
summary(fit3eLow)
summary(fit3eMid)
summary(fit3eMidHi)
x

gr <- "../graphs/"
#pdf (file = paste(gr, "avgMgEffects-subsets.pdf", sep = ""), width = 7, height = 5)
par(mar=c(4,2,2,2)+0.1) # drop title space and left space
plot(x=c(-1,1.3),
     y=-c(1,nrow(res)),
     type="n", axes = FALSE,
     xlab = "Average marginal effect",
     ylab = "")
abline(v=seq(-.6, 1.1, .1), col = "gray70")
abline(v=0, lty=2)
abline(h=seq(-1,-nrow(res),-1), col = "gray70")
axis(1, at = round(seq(-.6, 1, .2),1))
for (i in c(-1:-nrow(res))){
    points(y=i, x=res$AME[-i], pch=20, cex=1.5, col = "black")
    lines(y=rep(i, 2), x=c(res$lower[-i],res$upper[-i]), lwd = 2, col = "black")
}
#mar3$factor
polygon(x= c(-1.1,-.62,-.62,-1.1), y=c(-15,-15,0,0), col = "white", border = "white")
text(x=-1.1, y=-1:-nrow(res), labels=subset2, pos=4)
polygon(x= c(1.12,1.4,1.4,1.12), y=c(-15,-15,0,0), col = "white", border = "white")
text(x=1.1, y=-1:-nrow(res), labels=paste("N =",n), pos=4, cex = .8)
text(x=.7, y=-1.5, labels="Model 2", pos=4, font = 2)
text(x=.7, y=-8.5, labels="Model 3", pos=4, font = 2)
abline(h=-7.5)
#dev.off()


sel <- which(dat$yr==2006)
summary(dat$netApprovR[sel])

##########################
# simulations start here #
##########################
# std error version
sims3 <- with(dat,
              data.frame(dsameCoal=c(0,1),
                         dmultiRef=0,
                         drefHda=0,
#                         dmajSen=0,
                         dinSen=0,
                         legyrR=seq(from=(min(legyrR)-.05), to=(max(legyrR)+.05), length.out = 100),
                         legyrR2=seq(from=(min(legyrR)-.05), to=(max(legyrR)+.05), length.out = 100)^2,
#                         dreform2010=1,
                         netApprovR= 0.33, # <- mean during legis 2006 median(netApprovR),
#                         yr14 = 3,
#                         yr = 2003
                         legis = 2006
                         )
              )
sims3$pr <- predict(fit3e, newdata = sims3, type = "response")
sims3 <- cbind(sims3, predict(fit3e, newdata = sims3, type="link", se=TRUE))
sims3 <- within(sims3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit)) ## use 1.645 for 90 percent
  UL <- plogis(fit + (1.96 * se.fit)) ## use 1.645 for 90 percent
})
sims3$legyr <- seq(from=1, to=0, length.out = 100) # for plot
head(sims3)
library(ggplot2)
gr <- "../graphs/"
#pdf (file = paste(gr, "predictedPr.pdf", sep = ""), width = 7, height = 4)
ggplot(sims3, aes(x = legyr, y = PredictedProb)) +
    geom_ribbon(aes(ymin = LL, ymax = UL, fill = factor(dsameCoal)), alpha = .2) +
    geom_line(aes(colour = factor(dsameCoal)), size=1) +
    labs(fill = "Coalition chair", colour = "Coalition chair",
         x = "Legislative year remaining (in months)",
         y = "Predicted probability") +
    scale_x_continuous(breaks=seq(from=0, to=1, length.out=7), labels=seq(from=12, to=0, by=-2))
#dev.off()

# std error version
sims2 <- with(dat,
              data.frame(dsameCoal=c(0,1),
                         dmultiRef=0,
                         drefHda=0,
                         dmajSen=0,
                         dinSen=0,
                         legyrR=seq(from=(min(legyrR)-.05), to=(max(legyrR)+.05), length.out = 100),
                         legyrR2=seq(from=(min(legyrR)-.05), to=(max(legyrR)+.05), length.out = 100)^2,
                         dreform2010=0,
                         netApprovR=0.33, # median(netApprovR),
#                         yr14 = 3,
                         legis = 2006
                         )
              )
sims2$pr <- predict(fit2e, newdata = sims2, type = "response")
sims2 <- cbind(sims2, predict(fit2e, newdata = sims2, type="link", se=TRUE))
sims2 <- within(sims2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
sims2$legyr <- seq(from=1, to=0, length.out = 100) # for plot
head(sims2)
library(ggplot2)
gr <- "../graphs/"
#pdf (file = paste(gr, "predictedPr.pdf", sep = ""), width = 7, height = 4)
ggplot(sims2, aes(x = legyr, y = PredictedProb)) +
    geom_ribbon(aes(ymin = LL, ymax = UL, fill = factor(dsameCoal)), alpha = .2) +
    geom_line(aes(colour = factor(dsameCoal)), size=1) +
    labs(fill = "Coalition chair", colour = "Coalition chair",
         x = "Legislative year remaining (in months)",
         y = "Predicted probability") +
    scale_x_continuous(breaks=seq(from=0, to=1, length.out=7), labels=seq(from=12, to=0, by=-2))
#dev.off()


# for glmer the method above won't work, but method below does
# see https://stats.stackexchange.com/questions/147836/prediction-interval-for-lmer-mixed-effects-model-in-r
antilogit <- function(X){ exp(X) / (exp(X)+1) }
sims4 <- with(dat,
              data.frame(dsameCoal=c(0,1),
                         dmultiRef=0,
                         drefHda=1,
#                         dmajSen=1,
                         dinSen=0,
                         legyrR=seq(from=(min(legyrR)-.05), to=(max(legyrR)+.05), length.out = 100),
                         legyrR2=seq(from=(min(legyrR)-.05), to=(max(legyrR)+.05), length.out = 100)^2,
#                         dreform2010=0,
                         netApprovR=median(netApprovR),
                         legis = 2006
                         )
              )
library(merTools)
tmp <- predictInterval(fit4e, newdata = sims4, n.sims = 999)
sims4 <- cbind(sims4, antilogit(tmp))
head(sims4)
sims4$PredictedProb <- sims4$fit
#pdf (file = paste(gr, "predictedPr.pdf", sep = ""), width = 7, height = 4)
ggplot(sims4, aes(x = legyrR, y = PredictedProb)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = factor(dsameCoal)), alpha = .2) +
  geom_line(aes(colour = factor(dsameCoal)), size=1)
#dev.off()



# simulations: bayesian method
##################################
##################################
##  JAGS ESTIMATION OF MODEL 2  ##
##################################
##################################
library(R2jags)
###########
## MODEL ##
###########
logitModel <- function() {
    ### OLS regression model
    for (n in 1:N){                ## loop over observations
        depvar[n] ~ dbern(p[n]);   
        logit(p[n]) <- inprod(beta[],X[n,]);  ## FLEXIBLE SPECIFICATION FOR VARYING N OF REGRESSORS, PREPARE depvar AND X IN R
    }
    ############################
    ## NON-INFORMATIVE PRIORS ##
    ############################
    for (k in 1:K){                ## loop over regressors
        beta[k] ~ dnorm(0, .0001);
    }
}
#
########################################
### EXTRA DATA PREP FOR JAGS MODEL 2 ###
########################################
depvar <- dat$dv12
N <- length(depvar)
X <- data.frame(cons=rep(1, N), dsamePty=dat$dsamePty, dmultiRef=dat$dmultiRef, dmocionAllOpp=dat$dmocionAllOpp, dmocionMix=dat$dmocionMix, dmocionAllPdt=dat$dmocionAllPdt, drefHda=dat$drefHda, dmajSen=dat$dmajSen, dinSen=dat$dinSen, legyrR=dat$legyrR, dreform2010=dat$dreform2010, netApprovR=dat$netApprovR)
# labels to interpret parameters
var.labels <- colnames(X)
K <- length(var.labels)
X <- as.matrix(X)
### Data, initial values, and parameter vector for jags
dl.data <- list("N", "K", "depvar", "X")
dl.inits <- function (){
    list (
    #beta=rnorm(K),
    beta=summary(fit2)$coefficients[,1] # use glm's estimates
    )
    }
dl.parameters <- c("beta")
#dm.parameters <- c("beta", "sigma", "depvar.hat")
## test ride
tmp <- jags (data=dl.data, inits=dl.inits, dl.parameters,
             model.file=logitModel, n.chains=3,
             n.iter=100, n.thin=10
             )
## estimate
fit2jags <- jags (data=dl.data, inits=dl.inits, dl.parameters,
                  model.file=logitModel, n.chains=3,
                  n.iter=50000, n.thin=100,
                  )
#
tmp <- fit2jags
fit2jags <- update(fit2jags, 10000) # continue updating to produce 10000 new draws per chain
#traceplot(fit2jags) # visually check posterior parameter convergence
#
fit2jags$var.labels <- var.labels # add object to interpret coefficients

# sims bayesian
antilogit <- function(X){ exp(X) / (exp(X)+1) }
## pr(urgent)
coefs <- fit2jags$BUGSoutput$sims.matrix; coefs <- coefs[,-grep("deviance", colnames(fit2jags$BUGSoutput$sims.matrix))]
scenario <- c(
    1, #cons=1,
    0, #dsamePty <- c(0,1),
    0, #dmultiRef <- c(0,1),
    0, #dmocionAllOpp=0,
    0, #dmocionMix=0,
    0, #dmocionAllPdt=0,
    1, #drefHda=1,
    1, #dmajSen=1,
    0, #dinSen=0,
    0, #legyrR= <- range min max,
    0, #dreform2010=0,
    median(dat$netApprov) #netApprovR=median
)
names(scenario) <- c("cons", "dsamePty", "dmultiRef", "dmocionAllOpp", "dmocionMix", "dmocionAllPdt", "drefHda", "dmajSen", "dinSen", "legyrR", "dreform2010", "netApprovR")
#
n <- nrow(coefs)
sc <- matrix(rep(scenario, n), nrow = n, byrow = TRUE)
sc <- as.data.frame(sc)
colnames(sc) <- c("cons", "dsamePty", "dmultiRef", "dmocionAllOpp", "dmocionMix", "dmocionAllPdt", "drefHda", "dmajSen", "dinSen", "legyrR", "dreform2010", "netApprovR")
# change dsamePty by alternating 0,1
sc$dsamePty <- rep ( 0:1, n/2)
sc$legyrR <- seq(from=(min(dat$legyrR)-.05), to=(max(dat$legyrR)+.05), length.out = n)
tmp <- as.integer(1:n/1000); tmp[tmp==30] <- 0 # create 30 values with n/30=1000 for legyr
tmp <- tmp/29
legyr <- tmp # for plot, will plug to sc later
sc$legyrR <- legyr * ( max(dat$legyrR) - min(dat$legyrR) + .10 ) + (min(dat$legyrR) - .05) # 30 normalized values to predict
sc <- as.matrix(sc)
#
tmp <- fit2jags$BUGSoutput$summary[grep("beta", rownames(fit2jags$BUGSoutput$summary)),1] # coef point pred (mean posterior)
pointPred <- sc %*% diag(tmp) # right side achieves multiplication of matrix columns by vector
pointPred <- antilogit(rowSums(pointPred)) # will plug this in sc later
#
pred <- sc * coefs
pred <- antilogit(rowSums(pred)) # will plug this in sc later
#
sc <- as.data.frame(sc); colnames(sc) <- c("cons", "dsamePty", "dmultiRef", "dmocionAllOpp", "dmocionMix", "dmocionAllPdt", "drefHda", "dmajSen", "dinSen", "legyrR", "dreform2010", "netApprovR")
sc$legyr <- legyr; rm(legyr)
sc$pred <- pred; rm(pred)
sc$pointPred <- pointPred; rm(pointPred)
#
# así se hace en R un by yr mo: egen tmp=sum(invested) de stata
sc$ll <- ave(sc$pred, as.factor(sc$legyr*290 + sc$dsamePty), FUN=function(x) quantile(x, probs=.025), na.rm=TRUE)
sc$ul <- ave(sc$pred, as.factor(sc$legyr*290 + sc$dsamePty), FUN=function(x) quantile(x, probs=.975), na.rm=TRUE)
#
sc <- sc[-duplicated(as.factor(sc$legyr*290 + sc$dsamePty))==FALSE,]
#
library(ggplot2)
#pdf (file = paste(gr, "predictedPr.pdf", sep = ""), width = 7, height = 4)
ggplot(sc, aes(x = legyr, y = pointPred)) +
  geom_ribbon(aes(ymin = ll, ymax = ul, fill = factor(dsamePty)), alpha = .2) +
  geom_line(aes(colour = factor(dsamePty)), size=1)
#dev.off()

########################
# simulations end here #
########################


