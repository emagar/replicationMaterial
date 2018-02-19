## CODE TO REPLICATE RESULTS REPORTED IN 
##
## Eric Magar. 2012. ``Gubernatorial coattails in Mexican Congressional Elections'',
## The Journal of Politics, vol. vv, num. n, pp. ss--ee. 
## 
## Analysis performed in R (http://cran.r-project.org/).
##
## Instructions:
##
## (1) Create a working directory in your disk. Save this file along with data files
##     dipfed.out, triennia.out, annual.out, and dfprallgob.out in that directory.
##
## (2) Use the first command below to set working directory from step 1. 
##
## (3) Codebook
##
## VARNAME        NAME IN ARTICLE         DESCRIPTION
## pansh          Dvote                   pan's federal deputy vote share
## prish          Dvote                   pri's federal deputy vote share
## prdsh          Dvote                   prd's federal deputy vote share
## panshres       Dvote                   pan's federal deputy residual vote share
## prishres       Dvote                   pri's federal deputy residual vote share
## prdshres       Dvote                   prd's federal deputy residual vote share
## pan_prev       RecentDvote             pan's lagged...
## pri_prev       RecentDvote             pri's lagged...
## prd_prev       RecentDvote             prd's lagged...
## dconcgowopr    GovOnlyConcurs          Gubernatorial only concurs with federal deputies
## dconcprwogo    PresOnlyConcurs         Presidential only concurs with federal deputies
## dconcgoandpr   Gov&PresConcur          Both concur with federal deputies
## pangowopr1     Gvote|GovOnlyConcurs    pan's share in concurrent gubernat. race in midterm
## prigowopr1     Gvote|GovOnlyConcurs    pri's share in concurrent gubernat. race in midterm
## prdgowopr1     Gvote|GovOnlyConcurs    prd's share in concurrent gubernat. race in midterm
## pangowopr      Gvote|GovOnlyConcurs    pan's residual share in concurrent gubernat. race in midterm
## prigowopr      Gvote|GovOnlyConcurs    pri's residual share in concurrent gubernat. race in midterm
## prdgowopr      Gvote|GovOnlyConcurs    prd's residual share in concurrent gubernat. race in midterm
## pangoandpr1    Gvote|Gov&PresConcur    pan's share in concurrent gubernat. race in presidential
## prigoandpr1    Gvote|Gov&PresConcur    pri's share in concurrent gubernat. race in presidential
## prdgoandpr1    Gvote|Gov&PresConcur    prd's share in concurrent gubernat. race in presidential
## pangoandpr     Gvote|Gov&PresConcur    pan's residual share in concurrent gubernat. race in presidential
## prigoandpr     Gvote|Gov&PresConcur    pri's residual share in concurrent gubernat. race in presidential
## prdgoandpr     Gvote|Gov&PresConcur    prd's residual share in concurrent gubernat. race in presidential
## panprwogo1     Pvote|PresOnlyConcurs   pan's share in presidential race when governor not elected
## priprwogo1     Pvote|PresOnlyConcurs   pri's share in presidential race when governor not elected
## prdprwogo1     Pvote|PresOnlyConcurs   prd's share in presidential race when governor not elected
## panprandgo1    Pvote|Gov&PresConcur    pan's share in presidential race when governor also elected
## priprandgo1    Pvote|Gov&PresConcur    pri's share in presidential race when governor also elected
## prdprandgo1    Pvote|Gov&PresConcur    prd's share in presidential race when governor also elected
## panprwogo      Pvote|PresOnlyConcurs   pan's residual share in presidential race when governor not elected
## priprwogo      Pvote|PresOnlyConcurs   pri's residual share in presidential race when governor not elected
## prdprwogo      Pvote|PresOnlyConcurs   prd's residual share in presidential race when governor not elected
## panprandgo     Pvote|Gov&PresConcur    pan's residual share in presidential race when governor also elected
## priprandgo     Pvote|Gov&PresConcur    pri's residual share in presidential race when governor also elected
## prdprandgo     Pvote|Gov&PresConcur    prd's residual share in presidential race when governor also elected
## dincgopan      IncumbentGovernor       state has pan governor when federal election held
## dincgopri      IncumbentGovernor       state has pri governor when federal election held
## dincgoprd      IncumbentGovernor       state has prd governor when federal election held
## dincprpan      IncumbentPresident      pan president when federal election held
## dincprpri      IncumbentPresident      pri president when federal election held
## panxgspbar     Economy                 GSP growth conditional on pan incumbency
## prixgspbar     Economy                 GSP growth conditional on pri incumbency
## prdxgspbar     Economy                 GSP growth conditional on prd incumbency
## dcoapan        PartyCoalesced          pan allied to other party(ies) in state's federal deputy races
## dcoapri        PartyCoalesced          pri allied to other party(ies) in state's federal deputy races
## dcoaprd        PartyCoalesced          prd allied to other party(ies) in state's federal deputy races
## yr             ---                     federal election year
## edon           ---                     state number
## edo            ---                     state name
## p1-p12         ---                     deputy votes by party (before apportioning coalition votes)
## p1lab-p12lab   ---                     party labels
## goyr           ---                     year of last/current gubernatorial election
## p1g-p12g       ---                     last/current gubernatorial votes by party (before apportioning coalition votes)
## p1glab-p12glab ---                     party labels
## p1g-p10g       ---                     last/current presidential votes by party (before apportioning coalition votes)
## p1glab-p10glab ---                     party labels
## ptot           ---                     state population

## SET YOUR WORKING DIRECTORY HERE
workdir <- c("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update")
#workdir <- c("C:/Documents and Settings/emagarm/My documents/data/youNameIt")
setwd(workdir)
#DROP##setwd("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/elecs/MXelsCalendGovt/coattails/09update")
#DROP##setwd("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/elecs/MXelsCalendGovt/coattails/09update")

## LOADS REQUIRED PACKAGES
library("arm")
library ("MCMCpack")
library("pcse")

## CLEAN MEMORY
rm(list = ls()) 

## SAVES GENERAL MODEL FOR WINBUGS ESTIMATIONS
cat("
model {
  for (n in 1:N){                ## loop over observations
      depvar[n] ~ dnorm(depvar.hat[n] , tau);   
      depvar.hat[n] <- inprod(beta[],X[n,]);  ## FLEXIBLE SPECIFICATION FOR VARYING 
                                              ## NUMBER OF REGRESSORS, PREPARE X IN R
                }
  ############################
  ## NON-INFORMATIVE PRIORS ##
  ############################
  for (k in 1:K){
    beta[k] ~ dnorm(0, .0001);
    }
  tau <- pow(sigma, -2);
  sigma ~ dunif(0,100);
}
", file="linearModel.txt")

## READS MAIN DATA
alldat <- read.csv("dipfed.out", header=TRUE)
##
## GENERATE NEW VARIABLES
alldat$dconcprwogo <- alldat$dconcpr * (1- alldat$dconcgo) ## DUMMY PRES CONCURS BUT NOT GOV
alldat$panprandgo1 <- alldat$panpr1 * alldat$dconcgoandpr  ## PRESVOT | PRES & GOV CONCUR
alldat$panprwogo1  <- alldat$panpr1 * alldat$dconcprwogo   ## PRESVOT | PRES ONLY CONCURS
alldat$priprandgo1 <- alldat$pripr1 * alldat$dconcgoandpr
alldat$priprwogo1  <- alldat$pripr1 * alldat$dconcprwogo
alldat$prdprandgo1 <- alldat$prdpr1 * alldat$dconcgoandpr
alldat$prdprwogo1  <- alldat$prdpr1 * alldat$dconcprwogo
alldat$panprandgo  <- alldat$panpr * alldat$dconcgoandpr  ## PRESRESID | PRES & GOV CONCUR
alldat$panprwogo   <- alldat$panpr * alldat$dconcprwogo   ## PRESRESID | PRES ONLY CONCURS
alldat$priprandgo  <- alldat$pripr * alldat$dconcgoandpr
alldat$priprwogo   <- alldat$pripr * alldat$dconcprwogo
alldat$prdprandgo  <- alldat$prdpr * alldat$dconcgoandpr
alldat$prdprwogo   <- alldat$prdpr * alldat$dconcprwogo

#####################################
####  1997-2009 period analysis  ####
#####################################
##
usedat <- alldat[alldat$yr>=1997,]
##
## PREPARES DATA FRAMES FOR SHARE REGRESSIONS
panshr <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                pansh=usedat$pansh, pan_prev=usedat$pan_prev, dconcpr=usedat$dconcpr,
                panpr1=usedat$panpr1, dconcgo=usedat$dconcgo, pangoandpr1=usedat$pangoandpr1,
                pangowopr1=usedat$pangowopr1, dincgopan=usedat$dincgopan, dincprpan=usedat$dincprpan,
                panxgspbar=usedat$panxgspbar, dcoapan=usedat$dcoapan,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, panprandgo1=usedat$panprandgo1, panprwogo1=usedat$panprwogo1)
prishr <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                prish=usedat$prish, pri_prev=usedat$pri_prev, dconcpr=usedat$dconcpr,
                pripr1=usedat$pripr1, dconcgo=usedat$dconcgo, prigoandpr1=usedat$prigoandpr1,
                prigowopr1=usedat$prigowopr1, dincgopri=usedat$dincgopri, dincprpri=usedat$dincprpri,
                prixgspbar=usedat$prixgspbar, dcoapri=usedat$dcoapri,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, priprandgo1=usedat$priprandgo1, priprwogo1=usedat$priprwogo1)
prdshr <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                prdsh=usedat$prdsh, prd_prev=usedat$prd_prev, dconcpr=usedat$dconcpr,
                prdpr1=usedat$prdpr1, dconcgo=usedat$dconcgo, prdgoandpr1=usedat$prdgoandpr1,
                prdgowopr1=usedat$prdgowopr1, dincgoprd=usedat$dincgoprd,
                prdxgspbar=usedat$prdxgspbar, dcoaprd=usedat$dcoaprd,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, prdprandgo1=usedat$prdprandgo1, prdprwogo1=usedat$prdprwogo1)

###############################################################
### SHARES PAN                                              ###
###############################################################
##
tmp <- panshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)]))) ## DATA FOR WINBUGS
#
panshest97   <-   lm (depvar ~ X$pan_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$panprwogo1 + X$panprandgo1
                         + X$pangowopr1 + X$pangoandpr1
                         + X$dincgopan + X$dincprpan + X$panxgspbar + X$dcoapan)
##
## REPORT MODEL 1
mod <- panshest97
##
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

## Tests of beta3+beta6 and beta4+beta8
cv <- vcovPC(mod, groupN=tmp$edon, groupT=tmp$yr) ## EXTRACT VAR-COV MATRIX FOR TESTS
b3 <- as.numeric(mod.pcse$b[3]); b6 <- as.numeric(mod.pcse$b[6])
v3 <- cv[3,3]; v6 <- cv[6,6]; c36 <- cv[3,6]; se36 <- sqrt(v3 + v6 + 2*c36)
test <- (b3+b6)/se36; test
dt(test, df=mod.pcse$df) ## one-tailed
b4 <- as.numeric(mod.pcse$b[4]); b8 <- as.numeric(mod.pcse$b[8])
v4 <- cv[4,4]; v8 <- cv[8,8]; c48 <- cv[4,8]; se48 <- sqrt(v4 + v8 + 2*c48)
test <- (b4+b8)/se48; test
dt(test, df=mod.pcse$df) ## one-tailed
b3 <- as.numeric(mod.pcse$b[3]); b4 <- as.numeric(mod.pcse$b[4])
v3 <- cv[3,3]; v4 <- cv[4,4]; c34 <- cv[3,4]; se34 <- sqrt(v3 + v4 + 2*c34)
test <- (b3+b4)/se34; test
dt(test, df=mod.pcse$df) ## one-tailed
## Test of beta6-beta8
b6 <- as.numeric(mod.pcse$b[6]); b8 <- as.numeric(mod.pcse$b[8])
v6 <- cv[6,6]; v8 <- cv[8,8]; c68 <- cv[6,8]; se68 <- sqrt(v6 + v8 + 2*c68)
test <- (b6-b8)/se68; test
dt(test, df=mod.pcse$df) ## one-tailed

## NON AUTOREGRESSIVE MODEL
mod     <-   lm (depvar ~               X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$panprwogo1 + X$panprandgo1
                         + X$pangowopr1 + X$pangoandpr1
                         + X$dincgopan + X$dincprpan + X$panxgspbar + X$dcoapan)
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

# BAYESIAN ESTIMATION OF MODEL 1
tmp <- panshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- as.matrix(cbind (cons, tmp$pan_prev, tmp$dconcprwogo, tmp$dconcgowopr,
                      tmp$dconcgoandpr, tmp$panprwogo1, tmp$panprandgo1,
                      tmp$pangowopr1, tmp$pangoandpr1, tmp$dincgopan,
                      tmp$dincprpan, tmp$panxgspbar, tmp$dcoapan))
varspansh <- c("cons", "pan_prev", "dconcprwogo", "dconcgowopr", "dconcgoandpr", "panprwogo1", "panprandgo1",
               "pangowopr1", "pangoandpr1", "dincgopan", "dincprpan", "panxgspbar", "dcoapan")
colnames(X) <- varspansh
K <- ncol(X)
#
shr.data <- list ("depvar", "X", "N", "K")
shr.inits <- function (){
    list (
    beta=rnorm(K),
    sigma=runif(1)
    )
    }
shr.parameters <- c("beta", "sigma")
#
#test ride to see program works
tmp <- bugs (shr.data, shr.inits, shr.parameters,
                "linearModel.txt", n.chains=3,
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",  ## SET LOCATION OF BUGS IN YOUR DISK
                program = c("WinBUGS"))
#
#longer run
panshBest97 <- bugs (shr.data, shr.inits, shr.parameters,
                "linearModel.txt", n.chains=4,
                n.iter=5000, n.thin=10, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",  ## SET LOCATION OF BUGS IN YOUR DISK
                program = c("WinBUGS"))

plot(panshBest97)
print(panshBest97)

###############################################################
### SHARES PRI                                              ###
###############################################################
#
tmp <- prishr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprish <- colnames(X)
#
prishest97 <-   lm (depvar ~ X$pri_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$priprwogo1 + X$priprandgo1
                         + X$prigowopr1 + X$prigoandpr1
                         + X$dincgopri + X$dincprpri + X$prixgspbar + X$dcoapri)
#
## REPORT MODEL 1
mod <- prishest97
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

## Tests of beta3+beta6 and beta4+beta8
cv <- vcovPC(mod, groupN=tmp$edon, groupT=tmp$yr) ## EXTRACT VAR-COV MATRIX FOR TESTS
b3 <- as.numeric(mod.pcse$b[3]); b6 <- as.numeric(mod.pcse$b[6])
v3 <- cv[3,3]; v6 <- cv[6,6]; c36 <- cv[3,6]; se36 <- sqrt(v3 + v6 + 2*c36)
test <- (b3+b6)/se36; test
dt(test, df=mod.pcse$df) ## one-tailed
b4 <- as.numeric(mod.pcse$b[4]); b8 <- as.numeric(mod.pcse$b[8])
v4 <- cv[4,4]; v8 <- cv[8,8]; c48 <- cv[4,8]; se48 <- sqrt(v4 + v8 + 2*c48)
test <- (b4+b8)/se48; test
dt(test, df=mod.pcse$df) ## one-tailed
b3 <- as.numeric(mod.pcse$b[3]); b4 <- as.numeric(mod.pcse$b[4])
v3 <- cv[3,3]; v4 <- cv[4,4]; c34 <- cv[3,4]; se34 <- sqrt(v3 + v4 + 2*c34)
test <- (b3+b4)/se34; test
dt(test, df=mod.pcse$df) ## one-tailed
## Test of beta6-beta8
b6 <- as.numeric(mod.pcse$b[6]); b8 <- as.numeric(mod.pcse$b[8])
v6 <- cv[6,6]; v8 <- cv[8,8]; c68 <- cv[6,8]; se68 <- sqrt(v6 + v8 + 2*c68)
test <- (b6-b8)/se68; test
dt(test, df=mod.pcse$df) ## one-tailed

## NON AUTOREGRESSIVE MODEL
mod     <-   lm (depvar ~              X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$priprwogo1 + X$priprandgo1
                         + X$prigowopr1 + X$prigoandpr1
                         + X$dincgopri + X$dincprpri + X$prixgspbar + X$dcoapri)
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

# BAYESIAN ESTIMATION OF MODEL 1
tmp <- prishr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- as.matrix(cbind (cons, tmp$pri_prev, tmp$dconcprwogo, tmp$dconcgowopr,
                      tmp$dconcgoandpr, tmp$priprwogo1, tmp$priprandgo1,
                      tmp$prigowopr1, tmp$prigoandpr1, tmp$dincgopri,
                      tmp$dincprpri, tmp$prixgspbar, tmp$dcoapri))
varsprish <- c("cons", "pri_prev", "dconcprwogo", "dconcgowopr", "dconcgoandpr", "priprwogo1", "priprandgo1",
               "prigowopr1", "prigoandpr1", "dincgopri", "dincprpri", "prixgspbar", "dcoapri")
colnames(X) <- varsprish
K <- ncol(X)
#
shr.data <- list ("depvar", "X", "N", "K")
shr.inits <- function (){
    list (
    beta=rnorm(K),
    sigma=runif(1)
    )
    }
shr.parameters <- c("beta", "sigma")
#
##test ride to see program works
tmp <- bugs (shr.data, shr.inits, shr.parameters,
                "linearModel.txt", n.chains=3,
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))
#
#longer run
prishBest97 <- bugs (shr.data, shr.inits, shr.parameters,
                "linearModel.txt", n.chains=4,
                n.iter=5000, n.thin=10, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(prishBest97)
print(prishBest97)

###############################################################
### SHARES PRD                                              ###
###############################################################
#
tmp <- prdshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprdsh <- colnames(X)
#
##Estimate model in R
prdshest97 <-   lm (depvar ~ X$prd_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$prdprwogo1 + X$prdprandgo1
                         + X$prdgowopr1 + X$prdgoandpr1
                         + X$dincgoprd + X$prdxgspbar)
#
## REPORT MODEL 1
mod <- prdshest97
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

## Tests of beta3+beta6 and beta4+beta8
cv <- vcovPC(mod, groupN=tmp$edon, groupT=tmp$yr) ## EXTRACT VAR-COV MATRIX FOR TESTS
b3 <- as.numeric(mod.pcse$b[3]); b6 <- as.numeric(mod.pcse$b[6])
v3 <- cv[3,3]; v6 <- cv[6,6]; c36 <- cv[3,6]; se36 <- sqrt(v3 + v6 + 2*c36)
test <- (b3+b6)/se36; test
dt(test, df=mod.pcse$df) ## one-tailed
b4 <- as.numeric(mod.pcse$b[4]); b8 <- as.numeric(mod.pcse$b[8])
v4 <- cv[4,4]; v8 <- cv[8,8]; c48 <- cv[4,8]; se48 <- sqrt(v4 + v8 + 2*c48)
test <- (b4+b8)/se48; test
dt(test, df=mod.pcse$df) ## one-tailed
b3 <- as.numeric(mod.pcse$b[3]); b4 <- as.numeric(mod.pcse$b[4])
v3 <- cv[3,3]; v4 <- cv[4,4]; c34 <- cv[3,4]; se34 <- sqrt(v3 + v4 + 2*c34)
test <- (b3+b4)/se34; test
dt(test, df=mod.pcse$df) ## one-tailed
## Test of beta6-beta8
b6 <- as.numeric(mod.pcse$b[6]); b8 <- as.numeric(mod.pcse$b[8])
v6 <- cv[6,6]; v8 <- cv[8,8]; c68 <- cv[6,8]; se68 <- sqrt(v6 + v8 + 2*c68)
test <- (b6-b8)/se68; test
dt(test, df=mod.pcse$df) ## one-tailed

## NON AUTOREGRESSIVE MODEL
mod     <-   lm (depvar ~              X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$prdprwogo1 + X$prdprandgo1
                         + X$prdgowopr1 + X$prdgoandpr1
                         + X$dincgoprd + X$prdxgspbar)
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

## BAYESIAN ESTIMATION OF MODEL 1
tmp <- prdshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- as.matrix(cbind (cons, tmp$prd_prev, tmp$dconcprwogo, tmp$dconcgowopr,
                      tmp$dconcgoandpr, tmp$prdprwogo1, tmp$prdprandgo1,
                      tmp$prdgowopr1, tmp$prdgoandpr1, tmp$dincgoprd,
                      tmp$prdxgspbar))
varsprdsh <- c("cons", "prd_prev", "dconcprwogo", "dconcgowopr", "dconcgoandpr", "prdprwogo1", "prdprandgo1",
               "prdgowopr1", "prdgoandpr1", "dincgoprd", "prdxgspbar")
colnames(X) <- varsprdsh
K <- ncol(X)
#
shr.data <- list ("depvar", "X", "N", "K")
shr.inits <- function (){
    list (
    beta=rnorm(K),
    sigma=runif(1)
    )
    }
shr.parameters <- c("beta", "sigma")
#
##test ride to see program works
tmp <- bugs (shr.data, shr.inits, shr.parameters,
                "linearModel.txt", n.chains=3,
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))
#
#longer run
prdshBest97 <- bugs (shr.data, shr.inits, shr.parameters,
                "linearModel.txt", n.chains=4,
                n.iter=5000, n.thin=10, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(prdshBest97)
print(prdshBest97)

##################################################################
##   REPORT RESULTS OF ALL THREE BAYESIAN ESTIMATIONS: 95% CIs  ##
##################################################################
tmp<-rep(NA,13)
attach.bugs(panshBest97)
panshCI <- data.frame(var=tmp,q025=tmp,q25=tmp,q50=tmp,q75=tmp,q975=tmp)
panshCI[,1]<-varspansh
for (r in 1:13){
    panshCI[r,2] <- quantile (beta[,r], 0.025, names=F)
    panshCI[r,3] <- quantile (beta[,r], 0.25, names=F)
    panshCI[r,4] <- quantile (beta[,r], 0.5, names=F)
    panshCI[r,5] <- quantile (beta[,r], 0.75, names=F)
    panshCI[r,6] <- quantile (beta[,r], 0.975, names=F)
    }
attach.bugs(prishBest97)
prishCI <- data.frame(var=tmp,q025=tmp,q25=tmp,q50=tmp,q75=tmp,q975=tmp)
prishCI[,1]<-varsprish
for (r in 1:13){
    prishCI[r,2] <- quantile (beta[,r], 0.025, names=F)
    prishCI[r,3] <- quantile (beta[,r], 0.25, names=F)
    prishCI[r,4] <- quantile (beta[,r], 0.5, names=F)
    prishCI[r,5] <- quantile (beta[,r], 0.75, names=F)
    prishCI[r,6] <- quantile (beta[,r], 0.975, names=F)
    }
tmp<-rep(NA,11)
attach.bugs(prdshBest97)
prdshCI <- data.frame(var=tmp,q025=tmp,q25=tmp,q50=tmp,q75=tmp,q975=tmp)
prdshCI[,1]<-varsprdsh
for (r in 1:11){
    prdshCI[r,2] <- quantile (beta[,r], 0.025, names=F)
    prdshCI[r,3] <- quantile (beta[,r], 0.25, names=F)
    prdshCI[r,4] <- quantile (beta[,r], 0.5, names=F)
    prdshCI[r,5] <- quantile (beta[,r], 0.75, names=F)
    prdshCI[r,6] <- quantile (beta[,r], 0.975, names=F)
    }

## SCENARIOS FOR SIMULATIONS
#          cons       pan_prev         dconcprwogo dconcgowopr dconcgoandpr panprwogo1 panprandgo1
# pangowopr1 pangoandpr1 dincgopan dincprpan     panxgspbar           dcoapan
scenpan <- c(1,median(panshr$pan_prev),      0,          1,         0,          0,          0,
      NA,        0,          0,       0,     median(panshr$panxgspbar),  0)
scenpri <- c(1,median(prishr$pri_prev),0,1,0,0,0,NA,0,0,0,median(prishr$prixgspbar),0)
scenprd <- c(1,median(prdshr$prd_prev),0,1,0,0,0,NA,0,0,median(prdshr$prdxgspbar))
#
## FOR NET EFFECTS
netpan <- c(0,0,0,1,0,0,0,NA,0,0,0,0,0)
netpri <- c(0,0,0,1,0,0,0,NA,0,0,0,0,0)
netprd <- c(0,0,0,1,0,0,0,NA,0,0,0)
godat <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                    dconcgo=usedat$dconcgo, gopansh=usedat$gopansh, goprish=usedat$goprish, goprdsh=usedat$goprdsh)
jitter <- 1-runif(length(godat$dconcgo))/50

## SCENARIOS WITH CI AND THRESHOLD
attach.bugs(panshBest97)
scenarios <- scenpan
nsims<-dim(beta)[1]
tmp<-rep(NA,nsims); tmp4<-tmp; tmp8<-tmp; expsh<-tmp;
govsh <- (0:100)/100
expsh <- matrix(NA, nrow=1000, ncol=101)
expshnoc <- matrix(NA, nrow=1000, ncol=101)
for (c in 1:101){
    expsh[,c] <- beta[,1]*scenarios[1]+beta[,2]*scenarios[2]+beta[,4]*scenarios[4]+beta[,8]*govsh[c]+beta[,12]*scenarios[12]
    expshnoc[,c] <- beta[,1]*scenarios[1]+beta[,2]*scenarios[2]+beta[,12]*scenarios[12]
    }
tmpa<-median(beta[,1])*scenarios[1]+median(beta[,2])*scenarios[2]+median(beta[,4])*scenarios[4]+median(beta[,12])*scenarios[12]
tmpb<-median(beta[,8])
tmpanoc<-median(beta[,1])*scenarios[1]+median(beta[,2])*scenarios[2]+median(beta[,12])*scenarios[12]
lohi <- matrix(NA, nrow=4, ncol=101)
for (c in 1:101){
    lohi[1,c] <- quantile (expsh[,c], 0.025, names=F)
    lohi[2,c] <- quantile (expsh[,c], 0.75, names=F)
    lohi[3,c] <- quantile (expsh[,c], 0.25, names=F)
    lohi[4,c] <- quantile (expsh[,c], 0.975, names=F)
    }
plot(c(0,1),c(0,1), xlab="Gubernatorial vote share", ylab="Expected federal deputy vote share",
     type="n", axes= FALSE, main="PAN")
axis(1, (0:10)/10, labels= FALSE); axis(1, (0:5)/5)
axis(2, (0:10)/10, labels= FALSE); axis(2, (0:5)/5)
axis(3, (0:10)/10, labels= FALSE)
axis(4, (0:10)/10, labels= FALSE)
#lines(c(0,1),c(0,1),lty=3, col="grey") ## 45°-line
lines( govsh, lohi[1,] )
lines( govsh, lohi[2,], lwd=1.25 )
lines( govsh, lohi[3,], lwd=1.25 )
lines( govsh, lohi[4,] )
abline(h=tmpanoc, lty=3)
text(.85,tmpanoc-.02, "non-concurrence baseline", cex=.75)
abline(tmpa,tmpb, lwd=1.75)
lines(c(.17,.17), c(0,1), lty=2, col="grey")
lines(c(.31,.31), c(0,1), lty=2, col="grey")
text(.24,.03, "threshold", cex=.75)
points (godat$gopansh[godat$dconcgo==0], jitter[godat$dconcgo==0], pch=20, cex=.75, col="grey")
points (godat$gopansh[godat$dconcgo==1], jitter[godat$dconcgo==1], pch=20, cex=.75)

## SCENARIOS WITH CI AND THRESHOLD
attach.bugs(prishBest97)
scenarios <- scenpri
nsims<-dim(beta)[1]
tmp<-rep(NA,nsims); tmp4<-tmp; tmp8<-tmp; expsh<-tmp;
govsh <- (0:100)/100
expsh <- matrix(NA, nrow=1000, ncol=101)
expshnoc <- matrix(NA, nrow=1000, ncol=101)
for (c in 1:101){
    expsh[,c] <- beta[,1]*scenarios[1]+beta[,2]*scenarios[2]+beta[,4]*scenarios[4]+beta[,8]*govsh[c]+beta[,12]*scenarios[12]
    expshnoc[,c] <- beta[,1]*scenarios[1]+beta[,2]*scenarios[2]+beta[,12]*scenarios[12]
    }
tmpa<-median(beta[,1])*scenarios[1]+median(beta[,2])*scenarios[2]+median(beta[,4])*scenarios[4]+median(beta[,12])*scenarios[12]
tmpb<-median(beta[,8])
tmpanoc<-median(beta[,1])*scenarios[1]+median(beta[,2])*scenarios[2]+median(beta[,12])*scenarios[12]
lohi <- matrix(NA, nrow=4, ncol=101)
for (c in 1:101){
    lohi[1,c] <- quantile (expsh[,c], 0.025, names=F)
    lohi[2,c] <- quantile (expsh[,c], 0.75, names=F)
    lohi[3,c] <- quantile (expsh[,c], 0.25, names=F)
    lohi[4,c] <- quantile (expsh[,c], 0.975, names=F)
    }
plot(c(0,1),c(0,1), xlab="Gubernatorial vote share", ylab="Expected federal deputy vote share",
     type="n", axes= FALSE, main="PRI")
axis(1, (0:10)/10, labels= FALSE); axis(1, (0:5)/5)
axis(2, (0:10)/10, labels= FALSE); axis(2, (0:5)/5)
axis(3, (0:10)/10, labels= FALSE)
axis(4, (0:10)/10, labels= FALSE)
#lines(c(0,1),c(0,1),lty=3, col="grey") ## 45°-line
lines( govsh, lohi[1,] )
lines( govsh, lohi[2,], lwd=1.25 )
lines( govsh, lohi[3,], lwd=1.25 )
lines( govsh, lohi[4,] )
abline(h=tmpanoc, lty=3)
text(.85,tmpanoc-.02, "non-concurrence baseline", cex=.75)
abline(tmpa,tmpb, lwd=1.75)
lines(c(.4,.4), c(0,1), lty=2, col="grey")
lines(c(.56,.56), c(0,1), lty=2, col="grey")
text(.48,.03, "threshold", cex=.75)
points (godat$goprish[godat$dconcgo==0], jitter[godat$dconcgo==0], pch=20, cex=.75, col="grey")
points (godat$goprish[godat$dconcgo==1], jitter[godat$dconcgo==1], pch=20, cex=.75)

## SCENARIOS WITH CI AND THRESHOLD
attach.bugs(prdshBest97)
scenarios <- scenprd
nsims<-dim(beta)[1]
tmp<-rep(NA,nsims); tmp4<-tmp; tmp8<-tmp; expsh<-tmp;
govsh <- (0:100)/100
expsh <- matrix(NA, nrow=1000, ncol=101)
expshnoc <- matrix(NA, nrow=1000, ncol=101)
for (c in 1:101){
    expsh[,c] <- beta[,1]*scenarios[1]+beta[,2]*scenarios[2]+beta[,4]*scenarios[4]+beta[,8]*govsh[c]+beta[,11]*scenarios[11]
    expshnoc[,c] <- beta[,1]*scenarios[1]+beta[,2]*scenarios[2]+beta[,11]*scenarios[11]
    }
tmpa<-median(beta[,1])*scenarios[1]+median(beta[,2])*scenarios[2]+median(beta[,4])*scenarios[4]+median(beta[,11])*scenarios[11]
tmpb<-median(beta[,8])
tmpanoc<-median(beta[,1])*scenarios[1]+median(beta[,2])*scenarios[2]+median(beta[,11])*scenarios[11]
lohi <- matrix(NA, nrow=4, ncol=101)
for (c in 1:101){
    lohi[1,c] <- quantile (expsh[,c], 0.025, names=F)
    lohi[2,c] <- quantile (expsh[,c], 0.75, names=F)
    lohi[3,c] <- quantile (expsh[,c], 0.25, names=F)
    lohi[4,c] <- quantile (expsh[,c], 0.975, names=F)
    }
plot(c(0,1),c(0,1), xlab="Gubernatorial vote share", ylab="Expected federal deputy vote share",
     type="n", axes= FALSE, main="PRD")
axis(1, (0:10)/10, labels= FALSE); axis(1, (0:5)/5)
axis(2, (0:10)/10, labels= FALSE); axis(2, (0:5)/5)
axis(3, (0:10)/10, labels= FALSE)
axis(4, (0:10)/10, labels= FALSE)
#lines(c(0,1),c(0,1),lty=3, col="grey") ## 45°-line
lines( govsh, lohi[1,] )
lines( govsh, lohi[2,], lwd=1.25 )
lines( govsh, lohi[3,], lwd=1.25 )
lines( govsh, lohi[4,] )
abline(h=tmpanoc, lty=3)
text(.85,tmpanoc-.02, "non-concurrence baseline", cex=.75)
abline(tmpa,tmpb, lwd=1.75)
lines(c(.1,.1), c(0,1), lty=2, col="grey")
lines(c(.19,.19), c(0,1), lty=2, col="grey")
text(.145,.03, "threshold", cex=.75)
points (godat$goprdsh[godat$dconcgo==0], jitter[godat$dconcgo==0], pch=20, cex=.75, col="grey")
points (godat$goprdsh[godat$dconcgo==1], jitter[godat$dconcgo==1], pch=20, cex=.75)

## THE CASE OF NUEVO LEON 1997
scenpannl91 <- c(1,.24,0,0,1,0,.33,0,0,-.043,0)
scenpannl97 <- c(1,.31,0,0,1,0,.46,0,0,-.027,0)
#
#attach.bugs(panshBest)
attach.bugs(panshBest97)
nsims<-dim(beta)[1]
scenarios <- scenpannl91
govsh <- rep(.33, times=nsims)
tmp<-rep(NA,nsims); tmp1<-tmp; tmp2<-tmp; tmp3<-tmp; tmp4<-tmp; tmp5<-tmp; tmp6<-tmp; tmp7<-tmp; tmp8<-tmp; tmp9<-tmp; tmp10<-tmp; tmp11<-tmp;
expsh<-tmp; expshlinea<-tmp; expshlineb<-tmp
for (s in 1:nsims){
    tmp1[s] <- scenarios[1]*beta[s,1]
    tmp2[s] <- scenarios[2]*beta[s,2]
    tmp3[s] <- scenarios[3]*beta[s,3]
    tmp4[s] <- scenarios[4]*beta[s,4]
    tmp5[s] <- scenarios[5]*beta[s,5]
    tmp6[s] <- scenarios[6]*beta[s,6]
    tmp7[s] <- govsh[s]*beta[s,7] ## govsh
    tmp8[s] <- scenarios[8]*beta[s,8]
    tmp9[s] <- scenarios[9]*beta[s,9]
    tmp10[s] <- scenarios[10]*beta[s,10]
    tmp11[s] <- scenarios[11]*beta[s,11]
    expsh[s] <- tmp1[s]+tmp2[s]+tmp3[s]+tmp4[s]+tmp5[s]+tmp6[s]+tmp7[s]+tmp8[s]+tmp9[s]+tmp10[s]+tmp11[s]
    expshlinea[s] <- tmp1[s]+tmp2[s]+tmp3[s]+tmp4[s]+tmp5[s]+tmp6[s]+tmp8[s]+tmp9[s]+tmp10[s]+tmp11[s]
    expshlineb[s] <- beta[s,7]
    }
tmpa<-median(tmp1)+median(tmp2)+median(tmp3)+median(tmp4)+median(tmp5)+median(tmp6)+median(tmp8)+median(tmp9)+median(tmp10)+median(tmp11)
tmpb<-median(beta[,7])
rm(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,tmp11)
expsh91<-expsh
scenarios <- scenpannl97
govsh <- rep(.46, times=nsims)
tmp<-rep(NA,nsims); tmp1<-tmp; tmp2<-tmp; tmp3<-tmp; tmp4<-tmp; tmp5<-tmp; tmp6<-tmp; tmp7<-tmp; tmp8<-tmp; tmp9<-tmp; tmp10<-tmp; tmp11<-tmp;
expsh<-tmp; expshlinea<-tmp; expshlineb<-tmp
for (s in 1:nsims){
    tmp1[s] <- scenarios[1]*beta[s,1]
    tmp2[s] <- scenarios[2]*beta[s,2]
    tmp3[s] <- scenarios[3]*beta[s,3]
    tmp4[s] <- scenarios[4]*beta[s,4]
    tmp5[s] <- scenarios[5]*beta[s,5]
    tmp6[s] <- scenarios[6]*beta[s,6]
    tmp7[s] <- govsh[s]*beta[s,7] ## govsh
    tmp8[s] <- scenarios[8]*beta[s,8]
    tmp9[s] <- scenarios[9]*beta[s,9]
    tmp10[s] <- scenarios[10]*beta[s,10]
    tmp11[s] <- scenarios[11]*beta[s,11]
    expsh[s] <- tmp1[s]+tmp2[s]+tmp3[s]+tmp4[s]+tmp5[s]+tmp6[s]+tmp7[s]+tmp8[s]+tmp9[s]+tmp10[s]+tmp11[s]
    expshlinea[s] <- tmp1[s]+tmp2[s]+tmp3[s]+tmp4[s]+tmp5[s]+tmp6[s]+tmp8[s]+tmp9[s]+tmp10[s]+tmp11[s]
    expshlineb[s] <- beta[s,7]
    }
tmpa<-median(tmp1)+median(tmp2)+median(tmp3)+median(tmp4)+median(tmp5)+median(tmp6)+median(tmp8)+median(tmp9)+median(tmp10)+median(tmp11)
tmpb<-median(beta[,7])
rm(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,tmp11)
expsh97<-expsh
#
summary(expsh91)
quantile (expsh91, 0.025, names=F)
quantile (expsh91, 0.975, names=F)
summary(expsh97)
quantile (expsh97, 0.025, names=F)
quantile (expsh97, 0.975, names=F)

###############################################################
###  USES ANOTHER GSP SPECIFICATION FOR ALL THREE PARTIES   ###
###############################################################
tmp1 <- sign(usedat$panxgspbar)
tmp2 <- sign(usedat$prixgspbar)
tmp3 <- sign(usedat$prdxgspbar)
gspbar <- tmp1*tmp2*tmp3*abs(usedat$panxgspbar)
digxdippan=usedat$dincgopan*usedat$dincprpan
diggsppan=usedat$dincgopan*gspbar
dipgsppan=usedat$dincprpan*gspbar
digdipgsppan=usedat$dincgopan*usedat$dincprpan*gspbar
digxdippri=usedat$dincgopri*usedat$dincprpri
diggsppri=usedat$dincgopri*gspbar
dipgsppri=usedat$dincprpri*gspbar
digdipgsppri=usedat$dincgopri*usedat$dincprpri*gspbar
diggspprd=usedat$dincgoprd*gspbar
#
tmp <- panshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
panshestgsp     <-   lm (depvar ~ X$pan_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$panprwogo1 + X$panprandgo1
                         + X$pangowopr1 + X$pangoandpr1
                         + X$dincgopan + X$dincprpan
                         + gspbar + digxdippan + diggsppan + dipgsppan + digdipgsppan
                         + X$dcoapan)
pan.pcsegsp <- pcse(panshestgsp, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
tmp <- prishr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
prishestgsp <-   lm (depvar ~ X$pri_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$priprwogo1 + X$priprandgo1
                         + X$prigowopr1 + X$prigoandpr1
                         + X$dincgopri + X$dincprpri
                         + gspbar + digxdippri + diggsppri + dipgsppri + digdipgsppri
                         + X$dcoapri)
pri.pcsegsp <- pcse(prishestgsp, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
tmp <- prdshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
prdshestgsp <-   lm (depvar ~ X$prd_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$prdprwogo1 + X$prdprandgo1
                         + X$prdgowopr1 + X$prdgoandpr1
                         + X$dincgoprd
                         + gspbar + diggspprd
#                         + X$dcoaprd)
                         )
##prdshestyr <-   lm (depvar ~ X$prd_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
##                         + X$prdprwogo1 + X$prdprandgo1
##                         + X$prdgowopr1 + X$prdgoandpr1
##                         + X$dincgoprd + X$prdxgspbar + X$dcoaprd
##                         + d1982+d1985+d1988+d1991+d1994+d1997+d2000+d2003+d2006+d2009 )
prd.pcsegsp <- pcse(prdshestgsp, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
##prd.pcseyr <- pcse(prdshestyr, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
#
display(panshestgsp)
summary(pan.pcsegsp)
display(prishestgsp)
summary(pri.pcsegsp)
display(prdshestgsp)
summary(prd.pcsegsp)
#
rm(tmp1, tmp2, tmp3, gspbar,
   digxdippan, diggsppan,   dipgsppan,   digdipgsppan,
   digxdippri,  diggsppri,   dipgsppri,  digdipgsppri,
   diggspprd,
   panshestgsp, prishestgsp, prdshestgsp, pan.pcsegsp, pri.pcsegsp, prd.pcsegsp)


######################################################
## COMPARE MODELS WITH AND WITHOUT COALITION DUMMY  ##
######################################################
### SHARES PAN                                              ###
tmp <- panshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
mod1     <-   lm (depvar ~ X$pan_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$panprwogo1 + X$panprandgo1
                         + X$pangowopr1 + X$pangoandpr1
                         + X$dincgopan + X$dincprpan + X$panxgspbar + X$dcoapan)
mod2     <-   lm (depvar ~ X$pan_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$panprwogo1 + X$panprandgo1
                         + X$pangowopr1 + X$pangoandpr1
                         + X$dincgopan + X$dincprpan + X$panxgspbar)
display(mod1)
display(mod2)
### SHARES PRI                                              ###
tmp <- prishr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprish <- colnames(X)
mod1 <-   lm (depvar ~ X$pri_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$priprwogo1 + X$priprandgo1
                         + X$prigowopr1 + X$prigoandpr1
                         + X$dincgopri + X$dincprpri + X$prixgspbar + X$dcoapri)
mod2 <-   lm (depvar ~ X$pri_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$priprwogo1 + X$priprandgo1
                         + X$prigowopr1 + X$prigoandpr1
                         + X$dincgopri + X$dincprpri + X$prixgspbar)
display(mod1)
display(mod2)
### SHARES PRD                                              ###
tmp <- prdshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprdsh <- colnames(X)
mod1 <-   lm (depvar ~ X$prd_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$prdprwogo1 + X$prdprandgo1
                         + X$prdgowopr1 + X$prdgoandpr1
                         + X$dincgoprd + X$prdxgspbar + X$dcoaprd)
mod2 <-   lm (depvar ~ X$prd_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$prdprwogo1 + X$prdprandgo1
                         + X$prdgowopr1 + X$prdgoandpr1
                         + X$dincgoprd + X$prdxgspbar)
display(mod1)
display(mod2)
# NEED TO DO THE PCSE FOR PRD (SOME CHANGES IN COEFS) TO VERIFY SIGNIFICANCE
mod2.pcse <- pcse(mod2, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod2.pcse)



#####################################
####  1979-2009 period analysis  ####
#####################################
##
usedat <- alldat                      
##
## DRAW ELEMENTS FOR FIGURE 1
##
## IMPORT TRIENNIAL AGGREGATES
tri <- read.csv("triennia.out", header=TRUE)
tri$pansh <- tri$pan/tri$efec; tri$prish<-tri$pri/tri$efec; tri$prdsh<-tri$prd/tri$efec
tri$gopansh <- tri$gopan/tri$goefec; tri$goprish<-tri$gopri/tri$goefec; tri$goprdsh<-tri$goprd/tri$goefec
tri$prpansh <- tri$prpan/tri$prefec; tri$prprish<-tri$prpri/tri$prefec; tri$prprdsh<-tri$prprd/tri$prefec
##
## IMPORT ANNUAL AGGREGATES
anu <- read.csv("annual.out", header=TRUE)
anu$gopansh <- anu$gopan/anu$goefec; anu$goprish<-anu$gopri/anu$goefec; anu$goprdsh<-anu$goprd/anu$goefec
anu$pansh <- rep(NA, times=length(anu$gopansh)); anu$prish <- rep(NA, times=length(anu$gopansh)); anu$prdsh <- rep(NA, times=length(anu$gopansh))
for (t in c(1979,1982,1985,1988,1991,1994,1997,2000,2003,2006,2009)){
    anu$pansh[anu$yr==t] <- tri$pansh[tri$yr==t]
    anu$prish[anu$yr==t] <- tri$prish[tri$yr==t]
    anu$prdsh[anu$yr==t] <- tri$prdsh[tri$yr==t]
    }
##
plot(1:31, c(rep(0,30),1),axes=FALSE,xlab="Year",ylab="Vote share", type="n", main="PAN")
axis(1, 1:31, labels=FALSE);
axis(1, (0:10)*3+1, labels=c("1979","82","85","88","91","94","97","2000","03","06","09"))
axis(2, (0:10)/10, labels=FALSE); axis(2, (0:5)/5)
abline(h=.5, lty=3)
abline(v=18.5, lty=2, col="grey")
text(19.2,.8, labels="Full democracy attained", cex=.8,  srt=90)
points(1:31, anu$gopansh, pch="g", col="grey")
points((0:10)*3+1, anu$gopansh[(0:10)*3+1], pch="g")
points((0:10)*3+1, tri$prpansh, pch="p")
lines((0:10)*3+1, anu$pansh[(0:10)*3+1])
#points((0:10)*3+1, anu$pansh[(0:10)*3+1], pch=19)
#
plot(1:31, c(rep(0,30),1),axes=FALSE,xlab="Year",ylab="Vote share", type="n", main="PRI")
axis(1, 1:31, labels=FALSE);
axis(1, (0:10)*3+1, labels=c("1979","82","85","88","91","94","97","2000","03","06","09"))
axis(2, (0:10)/10, labels=FALSE); axis(2, (0:5)/5)
abline(h=.5, lty=3)
abline(v=18.5, lty=2, col="grey")
text(19.2,.8, labels="Full democracy attained", cex=.8, srt=90)
points(1:31, anu$goprish, pch="g", col="grey")
points((0:10)*3+1, anu$goprish[(0:10)*3+1], pch="g")
points((0:10)*3+1, tri$prprish, pch="p")
lines((0:10)*3+1, anu$prish[(0:10)*3+1])
#points((0:10)*3+1, anu$prish[(0:10)*3+1], pch=19)
#
plot(1:31, c(rep(0,30),1),axes=FALSE,xlab="Year",ylab="Vote share", type="n", main="PRD")
axis(1, 1:31, labels=FALSE);
axis(1, (0:10)*3+1, labels=c("1979","82","85","88","91","94","97","2000","03","06","09"))
axis(2, (0:10)/10, labels=FALSE); axis(2, (0:5)/5)
abline(h=.5, lty=3)
abline(v=18.5, lty=2, col="grey")
text(19.2,.8, labels="Full democracy attained", cex=.8, srt=90)
points(1:31, anu$goprdsh, pch="g", col="grey")
points((0:10)*3+1, anu$goprdsh[(0:10)*3+1], pch="g")
points((0:10)*3+1, tri$prprdsh, pch="p")
lines((0:10)*3+1, anu$prdsh[(0:10)*3+1])
#points((0:10)*3+1, anu$prdsh[(0:10)*3+1], pch=19)
#
allgob <- read.csv("dfprallgob.out", header=TRUE)
allgob$gopan[allgob$gopri==0] <- NA
allgob$goprd[allgob$gopri==0] <- NA
allgob$goefec[allgob$gopri==0] <- NA
allgob$gopri[allgob$gopri==0] <- NA
allgob$prpan[allgob$prpri==0] <- NA
allgob$prprd[allgob$prpri==0] <- NA
allgob$prefec[allgob$prpri==0] <- NA
allgob$prpri[allgob$prpri==0] <- NA
allgob$pansh <- allgob$pan/allgob$efec
allgob$prish <- allgob$pri/allgob$efec
allgob$prdsh <- allgob$prd/allgob$efec
allgob$gopansh <- allgob$gopan/allgob$goefec
allgob$goprish <- allgob$gopri/allgob$goefec
allgob$goprdsh <- allgob$goprd/allgob$goefec
allgob$prpansh <- allgob$prpan/allgob$prefec
allgob$prprish <- allgob$prpri/allgob$prefec
allgob$prprdsh <- allgob$prprd/allgob$prefec
##
## DESCRIPTIVE STATISTICS
## 1979--2009
summary(allgob$pansh)
length(allgob$pansh[is.na(allgob$pansh)==FALSE])
summary(allgob$prish)
length(allgob$prish[is.na(allgob$prish)==FALSE])
summary(allgob$prdsh)
length(allgob$prdsh[is.na(allgob$prdsh)==FALSE])
summary(allgob$prpansh)
length(allgob$prpansh[is.na(allgob$prpansh)==FALSE])
summary(allgob$prprish)
length(allgob$prprish[is.na(allgob$prprish)==FALSE])
summary(allgob$prprdsh)
length(allgob$prprdsh[is.na(allgob$prprdsh)==FALSE])
summary(allgob$gopansh)
length(allgob$gopansh[is.na(allgob$gopansh)==FALSE])
summary(allgob$goprish)
length(allgob$goprish[is.na(allgob$goprish)==FALSE])
summary(allgob$goprdsh)
length(allgob$goprdsh[is.na(allgob$goprdsh)==FALSE])
summary(allgob$gopansh[alldat$dconcgo==1])
length(allgob$gopansh[is.na(allgob$gopansh)==FALSE & alldat$dconcgo==1])
summary(allgob$goprish[alldat$dconcgo==1])
length(allgob$goprish[is.na(allgob$goprish)==FALSE & alldat$dconcgo==1])
summary(allgob$goprdsh[alldat$dconcgo==1])
length(allgob$goprdsh[is.na(allgob$goprdsh)==FALSE & alldat$dconcgo==1])
summary(alldat$panxgspbar)
length(alldat$panxgspbar)
summary(alldat$prixgspbar)
length(alldat$prixgspbar)
summary(alldat$prdxgspbar)
length(alldat$prdxgspbar)
table(alldat$dconcprwogo)
table(alldat$dconcgowopr)
table(alldat$dconcgoandpr)
neither <- 1 - alldat$dconcprwogo - alldat$dconcgowopr - alldat$dconcgoandpr; table(neither); rm(neither)
table(alldat$dincgopan)
table(alldat$dincgopri)
table(alldat$dincgoprd)
table(alldat$dincprpan)
table(alldat$dincprpri)
table(alldat$dcoapan)
table(alldat$dcoapri)
table(alldat$dcoaprd)
##
## 1997--2009
summary(allgob$pansh[allgob$yr>=1997])
length(allgob$pansh[is.na(allgob$pansh)==FALSE & allgob$yr>=1997])
summary(allgob$prish[allgob$yr>=1997])
length(allgob$prish[is.na(allgob$prish)==FALSE & allgob$yr>=1997])
summary(allgob$prdsh[allgob$yr>=1997])
length(allgob$prdsh[is.na(allgob$prdsh)==FALSE & allgob$yr>=1997])
summary(allgob$prpansh[allgob$yr>=1997])
length(allgob$prpansh[is.na(allgob$prpansh)==FALSE & allgob$yr>=1997])
summary(allgob$prprish[allgob$yr>=1997])
length(allgob$prprish[is.na(allgob$prprish)==FALSE & allgob$yr>=1997])
summary(allgob$prprdsh[allgob$yr>=1997])
length(allgob$prprdsh[is.na(allgob$prprdsh)==FALSE & allgob$yr>=1997])
summary(allgob$gopansh[allgob$yr>=1997])
length(allgob$gopansh[is.na(allgob$gopansh)==FALSE & allgob$yr>=1997])
summary(allgob$goprish[allgob$yr>=1997])
length(allgob$goprish[is.na(allgob$goprish)==FALSE & allgob$yr>=1997])
summary(allgob$goprdsh[allgob$yr>=1997])
length(allgob$goprdsh[is.na(allgob$goprdsh)==FALSE & allgob$yr>=1997])
summary(allgob$gopansh[alldat$dconcgo==1 & allgob$yr>=1997])
length(allgob$gopansh[is.na(allgob$gopansh)==FALSE & alldat$dconcgo==1 & allgob$yr>=1997])
summary(allgob$goprish[alldat$dconcgo==1 & allgob$yr>=1997])
length(allgob$goprish[is.na(allgob$goprish)==FALSE & alldat$dconcgo==1 & allgob$yr>=1997])
summary(allgob$goprdsh[alldat$dconcgo==1 & allgob$yr>=1997])
length(allgob$goprdsh[is.na(allgob$goprdsh)==FALSE & alldat$dconcgo==1 & allgob$yr>=1997])
summary(alldat$panxgspbar[allgob$yr>=1997])
length(alldat$panxgspbar[allgob$yr>=1997])
summary(alldat$prixgspbar[allgob$yr>=1997])
length(alldat$prixgspbar[allgob$yr>=1997])
summary(alldat$prdxgspbar[allgob$yr>=1997])
length(alldat$prdxgspbar[allgob$yr>=1997])
table(alldat$dconcprwogo[allgob$yr>=1997])
table(alldat$dconcgowopr[allgob$yr>=1997])
table(alldat$dconcgoandpr[allgob$yr>=1997])
neither <- 1 - alldat$dconcprwogo[allgob$yr>=1997] - alldat$dconcgowopr[allgob$yr>=1997] - alldat$dconcgoandpr[allgob$yr>=1997]; table(neither); rm(neither)
table(alldat$dincgopan[allgob$yr>=1997])
table(alldat$dincgopri[allgob$yr>=1997])
table(alldat$dincgoprd[allgob$yr>=1997])
table(alldat$dincprpan[allgob$yr>=1997])
table(alldat$dincprpri[allgob$yr>=1997])
table(alldat$dcoapan[allgob$yr>=1997])
table(alldat$dcoapri[allgob$yr>=1997])
table(alldat$dcoaprd[allgob$yr>=1997])
##
rm(anu, tri, t, allgob)

## PREPARES DATA FRAMES FOR SHARE REGRESSIONS
panshr <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                pansh=usedat$pansh, pan_prev=usedat$pan_prev, dconcpr=usedat$dconcpr, 
                panpr1=usedat$panpr1, dconcgo=usedat$dconcgo, pangoandpr1=usedat$pangoandpr1, 
                pangowopr1=usedat$pangowopr1, dincgopan=usedat$dincgopan, dincprpan=usedat$dincprpan, 
                panxgspbar=usedat$panxgspbar, dcoapan=usedat$dcoapan,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, panprandgo1=usedat$panprandgo1, panprwogo1=usedat$panprwogo1)
prishr <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot, 
                prish=usedat$prish, pri_prev=usedat$pri_prev, dconcpr=usedat$dconcpr, 
                pripr1=usedat$pripr1, dconcgo=usedat$dconcgo, prigoandpr1=usedat$prigoandpr1, 
                prigowopr1=usedat$prigowopr1, dincgopri=usedat$dincgopri, dincprpri=usedat$dincprpri, 
                prixgspbar=usedat$prixgspbar, dcoapri=usedat$dcoapri,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, priprandgo1=usedat$priprandgo1, priprwogo1=usedat$priprwogo1)
prdshr <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                prdsh=usedat$prdsh, prd_prev=usedat$prd_prev, dconcpr=usedat$dconcpr, 
                prdpr1=usedat$prdpr1, dconcgo=usedat$dconcgo, prdgoandpr1=usedat$prdgoandpr1, 
                prdgowopr1=usedat$prdgowopr1, dincgoprd=usedat$dincgoprd,  
                prdxgspbar=usedat$prdxgspbar, dcoaprd=usedat$dcoaprd,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, prdprandgo1=usedat$prdprandgo1, prdprwogo1=usedat$prdprwogo1)

###############################################################
### SHARES PAN                                              ###
###############################################################
##
tmp <- panshr
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)]))) ## DATA FOR WINBUGS
#
panshest     <-   lm (depvar ~ X$pan_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr 
                         + X$panprwogo1 + X$panprandgo1 
                         + X$pangowopr1 + X$pangoandpr1 
                         + X$dincgopan + X$dincprpan + X$panxgspbar + X$dcoapan)
panshnogoest <-   lm (depvar ~ X$pan_prev + X$dconcpr 
                         + X$panpr1
                         + X$dincgopan + X$dincprpan + X$panxgspbar + X$dcoapan)
##
## REPORT MODEL 2
mod <- panshest
##
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

## REPORT MODEL 3
display(panshnogoest)
panshnogoest.pcse <- pcse(panshnogoest, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(panshnogoest.pcse)

## Tests of beta3+beta6 and beta4+beta8
cv <- vcovPC(mod, groupN=tmp$edon, groupT=tmp$yr) ## EXTRACT VAR-COV MATRIX FOR TESTS
b3 <- as.numeric(mod.pcse$b[3]); b6 <- as.numeric(mod.pcse$b[6])
v3 <- cv[3,3]; v6 <- cv[6,6]; c36 <- cv[3,6]; se36 <- sqrt(v3 + v6 + 2*c36)
test <- (b3+b6)/se36; test
dt(test, df=mod.pcse$df) ## one-tailed
b4 <- as.numeric(mod.pcse$b[4]); b8 <- as.numeric(mod.pcse$b[8])
v4 <- cv[4,4]; v8 <- cv[8,8]; c48 <- cv[4,8]; se48 <- sqrt(v4 + v8 + 2*c48)
test <- (b4+b8)/se48; test
dt(test, df=mod.pcse$df) ## one-tailed
b3 <- as.numeric(mod.pcse$b[3]); b4 <- as.numeric(mod.pcse$b[4])
v3 <- cv[3,3]; v4 <- cv[4,4]; c34 <- cv[3,4]; se34 <- sqrt(v3 + v4 + 2*c34)
test <- (b3+b4)/se34; test
dt(test, df=mod.pcse$df) ## one-tailed
## Test of beta6-beta8
b6 <- as.numeric(mod.pcse$b[6]); b8 <- as.numeric(mod.pcse$b[8])
v6 <- cv[6,6]; v8 <- cv[8,8]; c68 <- cv[6,8]; se68 <- sqrt(v6 + v8 + 2*c68)
test <- (b6-b8)/se68; test
dt(test, df=mod.pcse$df) ## one-tailed

## ESTIMATE MODELS SEPARATELY FOR EACH CONCURRENCE REGIME
tmp1 <- X[X$dconcprwogo==1,]  ## Pres conc only
tmp2 <- X[X$dconcgowopr==1,]  ## Gov  conc only
tmp3 <- X[X$dconcgoandpr==1,] ## Gov & Pres conc
tmp4 <- X[X$dconcprwogo==0 & X$dconcgowopr==0 & X$dconcgoandpr==0,] ## Non-concurrent
tmpdv1 <- usedat$pansh[X$dconcprwogo==1]
tmpdv2 <- usedat$pansh[X$dconcgowopr==1]
tmpdv3 <- usedat$pansh[X$dconcgoandpr==1]
tmpdv4 <- usedat$pansh[X$dconcprwogo==0 & X$dconcgowopr==0 & X$dconcgoandpr==0]
tmp11 <- lm (tmpdv1 ~ tmp1$pan_prev + tmp1$panpr1                                      + tmp1$dincgopan + tmp1$dincprpan + tmp1$panxgspbar + tmp1$dcoapan)
tmp22 <- lm (tmpdv2 ~ tmp2$pan_prev                                  + tmp2$pangowopr1 + tmp2$dincgopan + tmp2$dincprpan + tmp2$panxgspbar + tmp2$dcoapan)
tmp33 <- lm (tmpdv3 ~ tmp3$pan_prev + tmp3$panpr1 + tmp3$pangoandpr1                   + tmp3$dincgopan + tmp3$dincprpan + tmp3$panxgspbar + tmp3$dcoapan)
tmp44 <- lm (tmpdv4 ~ tmp4$pan_prev                                                    + tmp4$dincgopan + tmp4$dincprpan + tmp4$panxgspbar + tmp4$dcoapan)
#
## PRES CONC ONLY
summary(tmp11)
## GOV  CONC ONLY 
summary(tmp22)
## GOV & PRES CONC
summary(tmp33)
## NON-CONCURRENT
summary(tmp44)
#
## NON AUTOREGRESSIVE MODEL
mod     <-   lm (depvar ~               X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr 
                         + X$panprwogo1 + X$panprandgo1 
                         + X$pangowopr1 + X$pangoandpr1 
                         + X$dincgopan + X$dincprpan + X$panxgspbar + X$dcoapan)
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

# BAYESIAN ESTIMATION OF MODEL 2
tmp <- panshr
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- as.matrix(cbind (cons, tmp$pan_prev, tmp$dconcprwogo, tmp$dconcgowopr, 
                      tmp$dconcgoandpr, tmp$panprwogo1, tmp$panprandgo1,
                      tmp$pangowopr1, tmp$pangoandpr1, tmp$dincgopan, 
                      tmp$dincprpan, tmp$panxgspbar, tmp$dcoapan))
varspansh <- c("cons", "pan_prev", "dconcprwogo", "dconcgowopr", "dconcgoandpr", "panprwogo1", "panprandgo1",
               "pangowopr1", "pangoandpr1", "dincgopan", "dincprpan", "panxgspbar", "dcoapan")
colnames(X) <- varspansh
K <- ncol(X)
#
shr.data <- list ("depvar", "X", "N", "K")
shr.inits <- function (){
    list (
    beta=rnorm(K),
    sigma=runif(1)
    )
    }
shr.parameters <- c("beta", "sigma")
#
#test ride to see program works
tmp <- bugs (shr.data, shr.inits, shr.parameters, 
                "linearModel.txt", n.chains=3, 
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",  ## SET LOCATION OF BUGS IN YOUR DISK
                program = c("WinBUGS"))
#
#longer run
panshBest <- bugs (shr.data, shr.inits, shr.parameters, 
                "linearModel.txt", n.chains=4, 
                n.iter=5000, n.thin=10, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(panshBest)
print(panshBest)

###############################################################
### SHARES PRI                                              ###
###############################################################
#
tmp <- prishr
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprish <- colnames(X)
#
prishest   <-   lm (depvar ~ X$pri_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr 
                         + X$priprwogo1 + X$priprandgo1 
                         + X$prigowopr1 + X$prigoandpr1 
                         + X$dincgopri + X$dincprpri + X$prixgspbar + X$dcoapri)
prishnogoest <- lm (depvar ~ X$pri_prev + X$dconcpr 
                         + X$pripr1 
                         + X$dincgopri + X$dincprpri + X$prixgspbar + X$dcoapri)
#
## REPORT MODEL 2
mod <- prishest
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

## REPORT MODEL 3
display(prishnogoest)
prishnogoest.pcse <- pcse(prishnogoest, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(prishnogoest.pcse)

## Tests of beta3+beta6 and beta4+beta8
cv <- vcovPC(mod, groupN=tmp$edon, groupT=tmp$yr) ## EXTRACT VAR-COV MATRIX FOR TESTS
b3 <- as.numeric(mod.pcse$b[3]); b6 <- as.numeric(mod.pcse$b[6])
v3 <- cv[3,3]; v6 <- cv[6,6]; c36 <- cv[3,6]; se36 <- sqrt(v3 + v6 + 2*c36)
test <- (b3+b6)/se36; test
dt(test, df=mod.pcse$df) ## one-tailed
b4 <- as.numeric(mod.pcse$b[4]); b8 <- as.numeric(mod.pcse$b[8])
v4 <- cv[4,4]; v8 <- cv[8,8]; c48 <- cv[4,8]; se48 <- sqrt(v4 + v8 + 2*c48)
test <- (b4+b8)/se48; test
dt(test, df=mod.pcse$df) ## one-tailed
b3 <- as.numeric(mod.pcse$b[3]); b4 <- as.numeric(mod.pcse$b[4])
v3 <- cv[3,3]; v4 <- cv[4,4]; c34 <- cv[3,4]; se34 <- sqrt(v3 + v4 + 2*c34)
test <- (b3+b4)/se34; test
dt(test, df=mod.pcse$df) ## one-tailed
## Test of beta6-beta8
b6 <- as.numeric(mod.pcse$b[6]); b8 <- as.numeric(mod.pcse$b[8])
v6 <- cv[6,6]; v8 <- cv[8,8]; c68 <- cv[6,8]; se68 <- sqrt(v6 + v8 + 2*c68)
test <- (b6-b8)/se68; test
dt(test, df=mod.pcse$df) ## one-tailed

## ESTIMATE MODELS SEPARATELY FOR EACH CONCURRENCE REGIME
tmp1 <- X[X$dconcprwogo==1,]  ## Pres conc only
tmp2 <- X[X$dconcgowopr==1,]  ## Gov  conc only
tmp3 <- X[X$dconcgoandpr==1,] ## Gov & Pres conc
tmp4 <- X[X$dconcprwogo==0 & X$dconcgowopr==0 & X$dconcgoandpr==0,] ## Non-concurrent
tmpdv1 <- usedat$prish[X$dconcprwogo==1]
tmpdv2 <- usedat$prish[X$dconcgowopr==1]
tmpdv3 <- usedat$prish[X$dconcgoandpr==1]
tmpdv4 <- usedat$prish[X$dconcprwogo==0 & X$dconcgowopr==0 & X$dconcgoandpr==0]
tmp11 <- lm (tmpdv1 ~ tmp1$pri_prev + tmp1$pripr1                                      + tmp1$dincgopri + tmp1$dincprpri + tmp1$prixgspbar + tmp1$dcoapri)
tmp22 <- lm (tmpdv2 ~ tmp2$pri_prev                                  + tmp2$prigowopr1 + tmp2$dincgopri + tmp2$dincprpri + tmp2$prixgspbar + tmp2$dcoapri)
tmp33 <- lm (tmpdv3 ~ tmp3$pri_prev + tmp3$pripr1 + tmp3$prigoandpr1                   + tmp3$dincgopri + tmp3$dincprpri + tmp3$prixgspbar + tmp3$dcoapri)
tmp44 <- lm (tmpdv4 ~ tmp4$pri_prev                                                    + tmp4$dincgopri + tmp4$dincprpri + tmp4$prixgspbar + tmp4$dcoapri)
#
## PRES CONC ONLY
summary(tmp11)
## GOV  CONC ONLY 
summary(tmp22)
## GOV & PRES CONC
summary(tmp33)
## NON-CONCURRENT
summary(tmp44)
#
## NON AUTOREGRESSIVE MODEL
mod     <-   lm (depvar ~              X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr 
                         + X$priprwogo1 + X$priprandgo1 
                         + X$prigowopr1 + X$prigoandpr1 
                         + X$dincgopri + X$dincprpri + X$prixgspbar + X$dcoapri)
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

# BAYESIAN ESTIMATION OF MODEL 2
tmp <- prishr
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- as.matrix(cbind (cons, tmp$pri_prev, tmp$dconcprwogo, tmp$dconcgowopr, 
                      tmp$dconcgoandpr, tmp$priprwogo1, tmp$priprandgo1,
                      tmp$prigowopr1, tmp$prigoandpr1, tmp$dincgopri, 
                      tmp$dincprpri, tmp$prixgspbar, tmp$dcoapri))
varsprish <- c("cons", "pri_prev", "dconcprwogo", "dconcgowopr", "dconcgoandpr", "priprwogo1", "priprandgo1",
               "prigowopr1", "prigoandpr1", "dincgopri", "dincprpri", "prixgspbar", "dcoapri")
colnames(X) <- varsprish
K <- ncol(X)
#
shr.data <- list ("depvar", "X", "N", "K")
shr.inits <- function (){
    list (
    beta=rnorm(K),
    sigma=runif(1)
    )
    }
shr.parameters <- c("beta", "sigma")
#
##test ride to see program works
tmp <- bugs (shr.data, shr.inits, shr.parameters, 
                "linearModel.txt", n.chains=3, 
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))
#
#longer run
prishBest97 <- bugs (shr.data, shr.inits, shr.parameters, 
                "linearModel.txt", n.chains=4, 
                n.iter=5000, n.thin=10, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(prishBest97)
print(prishBest97)

###############################################################
### SHARES PRD                                              ###
###############################################################
#
tmp <- prdshr
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprdsh <- colnames(X)
#
##Estimate model in R
prdshest     <- lm (depvar ~ X$prd_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr 
                         + X$prdprwogo1 + X$prdprandgo1 
                         + X$prdgowopr1 + X$prdgoandpr1 
                         + X$dincgoprd + X$prdxgspbar + X$dcoaprd)
prdshnogoest <- lm (depvar ~ X$prd_prev + X$dconcpr 
                         + X$prdpr1
                         + X$dincgoprd + X$prdxgspbar + X$dcoaprd)
#
## REPORT MODEL 2
mod <- prdshest
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

## REPORT MODEL 3
display(prdshnogoest)
prdshnogoest.pcse <- pcse(prdshnogoest, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(prdshnogoest.pcse)

## Tests of beta3+beta6 and beta4+beta8
cv <- vcovPC(mod, groupN=tmp$edon, groupT=tmp$yr) ## EXTRACT VAR-COV MATRIX FOR TESTS
b3 <- as.numeric(mod.pcse$b[3]); b6 <- as.numeric(mod.pcse$b[6])
v3 <- cv[3,3]; v6 <- cv[6,6]; c36 <- cv[3,6]; se36 <- sqrt(v3 + v6 + 2*c36)
test <- (b3+b6)/se36; test
dt(test, df=mod.pcse$df) ## one-tailed
b4 <- as.numeric(mod.pcse$b[4]); b8 <- as.numeric(mod.pcse$b[8])
v4 <- cv[4,4]; v8 <- cv[8,8]; c48 <- cv[4,8]; se48 <- sqrt(v4 + v8 + 2*c48)
test <- (b4+b8)/se48; test
dt(test, df=mod.pcse$df) ## one-tailed
b3 <- as.numeric(mod.pcse$b[3]); b4 <- as.numeric(mod.pcse$b[4])
v3 <- cv[3,3]; v4 <- cv[4,4]; c34 <- cv[3,4]; se34 <- sqrt(v3 + v4 + 2*c34)
test <- (b3+b4)/se34; test
dt(test, df=mod.pcse$df) ## one-tailed
## Test of beta6-beta8
b6 <- as.numeric(mod.pcse$b[6]); b8 <- as.numeric(mod.pcse$b[8])
v6 <- cv[6,6]; v8 <- cv[8,8]; c68 <- cv[6,8]; se68 <- sqrt(v6 + v8 + 2*c68)
test <- (b6-b8)/se68; test
dt(test, df=mod.pcse$df) ## one-tailed

## ESTIMATE MODELS SEPARATELY FOR EACH CONCURRENCE REGIME
tmp1 <- X[X$dconcprwogo==1,]  ## Pres conc only
tmp2 <- X[X$dconcgowopr==1,]  ## Gov  conc only
tmp3 <- X[X$dconcgoandpr==1,] ## Gov & Pres conc
tmp4 <- X[X$dconcprwogo==0 & X$dconcgowopr==0 & X$dconcgoandpr==0,] ## Non-concurrent
tmpdv1 <- usedat$prdsh[X$dconcprwogo==1]
tmpdv2 <- usedat$prdsh[X$dconcgowopr==1]
tmpdv3 <- usedat$prdsh[X$dconcgoandpr==1]
tmpdv4 <- usedat$prdsh[X$dconcprwogo==0 & X$dconcgowopr==0 & X$dconcgoandpr==0]
tmp11 <- lm (tmpdv1 ~ tmp1$prd_prev + tmp1$prdpr1                                      + tmp1$dincgoprd + tmp1$prdxgspbar + tmp1$dcoaprd)
tmp22 <- lm (tmpdv2 ~ tmp2$prd_prev                                  + tmp2$prdgowopr1 + tmp2$dincgoprd + tmp2$prdxgspbar + tmp2$dcoaprd)
tmp33 <- lm (tmpdv3 ~ tmp3$prd_prev + tmp3$prdpr1 + tmp3$prdgoandpr1                   + tmp3$dincgoprd + tmp3$prdxgspbar + tmp3$dcoaprd)
tmp44 <- lm (tmpdv4 ~ tmp4$prd_prev                                                    + tmp4$dincgoprd + tmp4$prdxgspbar + tmp4$dcoaprd)
#
## PRES CONC ONLY
summary(tmp11)
## GOV  CONC ONLY 
summary(tmp22)
## GOV & PRES CONC
summary(tmp33)
## NON-CONCURRENT
summary(tmp44)
#
## NON AUTOREGRESSIVE MODEL
mod     <-   lm (depvar ~              X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr 
                         + X$prdprwogo1 + X$prdprandgo1 
                         + X$prdgowopr1 + X$prdgoandpr1 
                         + X$dincgoprd + X$prdxgspbar)
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

# BAYESIAN ESTIMATION
tmp <- prdshr
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- as.matrix(cbind (cons, tmp$prd_prev, tmp$dconcprwogo, tmp$dconcgowopr, 
                      tmp$dconcgoandpr, tmp$prdprwogo1, tmp$prdprandgo1,
                      tmp$prdgowopr1, tmp$prdgoandpr1, tmp$dincgoprd, 
                      tmp$prdxgspbar))
varsprdsh <- c("cons", "prd_prev", "dconcprwogo", "dconcgowopr", "dconcgoandpr", "prdprwogo1", "prdprandgo1",
               "prdgowopr1", "prdgoandpr1", "dincgoprd", "prdxgspbar")
colnames(X) <- varsprdsh
K <- ncol(X)
#
shr.data <- list ("depvar", "X", "N", "K")
shr.inits <- function (){
    list (
    beta=rnorm(K),
    sigma=runif(1)
    )
    }
shr.parameters <- c("beta", "sigma")
#
##test ride to see program works
tmp <- bugs (shr.data, shr.inits, shr.parameters, 
                "linearModel.txt", n.chains=3, 
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))
#
#longer run
#prdshBest <- bugs (shr.data, shr.inits, shr.parameters, 
prdshBest97 <- bugs (shr.data, shr.inits, shr.parameters, 
                "linearModel.txt", n.chains=4, 
                n.iter=5000, n.thin=10, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(prdshBest97)
print(prdshBest97)

## REPORT RESULTS OF BAYESIAN ESTIMATION FOR ALL THREE PARTIES: 95% CIs
tmp<-rep(NA,13)
attach.bugs(panshBest97)
panshCI <- data.frame(var=tmp,q025=tmp,q25=tmp,q50=tmp,q75=tmp,q975=tmp)
panshCI[,1]<-varspansh
for (r in 1:13){
    panshCI[r,2] <- quantile (beta[,r], 0.025, names=F)
    panshCI[r,3] <- quantile (beta[,r], 0.25, names=F)
    panshCI[r,4] <- quantile (beta[,r], 0.5, names=F)
    panshCI[r,5] <- quantile (beta[,r], 0.75, names=F)
    panshCI[r,6] <- quantile (beta[,r], 0.975, names=F)
    }
attach.bugs(prishBest97)
prishCI <- data.frame(var=tmp,q025=tmp,q25=tmp,q50=tmp,q75=tmp,q975=tmp)
prishCI[,1]<-varsprish
for (r in 1:13){
    prishCI[r,2] <- quantile (beta[,r], 0.025, names=F)
    prishCI[r,3] <- quantile (beta[,r], 0.25, names=F)
    prishCI[r,4] <- quantile (beta[,r], 0.5, names=F)
    prishCI[r,5] <- quantile (beta[,r], 0.75, names=F)
    prishCI[r,6] <- quantile (beta[,r], 0.975, names=F)
    }
tmp<-rep(NA,11)
attach.bugs(prdshBest97)
prdshCI <- data.frame(var=tmp,q025=tmp,q25=tmp,q50=tmp,q75=tmp,q975=tmp)
prdshCI[,1]<-varsprdsh
for (r in 1:11){
    prdshCI[r,2] <- quantile (beta[,r], 0.025, names=F)
    prdshCI[r,3] <- quantile (beta[,r], 0.25, names=F)
    prdshCI[r,4] <- quantile (beta[,r], 0.5, names=F)
    prdshCI[r,5] <- quantile (beta[,r], 0.75, names=F)
    prdshCI[r,6] <- quantile (beta[,r], 0.975, names=F)
    }

###############################################################
###   USES ANOTHER GSP SPECIFICATION FOR ALL THREE PARTIES  ###
###############################################################
tmp1 <- sign(usedat$panxgspbar)
tmp2 <- sign(usedat$prixgspbar)
tmp3 <- sign(usedat$prdxgspbar)
gspbar <- tmp1*tmp2*tmp3*abs(usedat$panxgspbar)
digxdippan=usedat$dincgopan*usedat$dincprpan
diggsppan=usedat$dincgopan*gspbar
dipgsppan=usedat$dincprpan*gspbar
digdipgsppan=usedat$dincgopan*usedat$dincprpan*gspbar
digxdippri=usedat$dincgopri*usedat$dincprpri
diggsppri=usedat$dincgopri*gspbar
dipgsppri=usedat$dincprpri*gspbar
digdipgsppri=usedat$dincgopri*usedat$dincprpri*gspbar
diggspprd=usedat$dincgoprd*gspbar
tmp <- panshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
panshestgsp     <-   lm (depvar ~ X$pan_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$panprwogo1 + X$panprandgo1
                         + X$pangowopr1 + X$pangoandpr1
                         + X$dincgopan + X$dincprpan 
                         + gspbar + digxdippan + diggsppan + dipgsppan + digdipgsppan
                         + X$dcoapan)
pan.pcsegsp <- pcse(panshestgsp, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
tmp <- prishr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
prishestgsp <-   lm (depvar ~ X$pri_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$priprwogo1 + X$priprandgo1
                         + X$prigowopr1 + X$prigoandpr1
                         + X$dincgopri + X$dincprpri 
                         + gspbar + digxdippri + diggsppri + dipgsppri + digdipgsppri
                         + X$dcoapri)
pri.pcsegsp <- pcse(prishestgsp, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
tmp <- prdshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
prdshestgsp <-   lm (depvar ~ X$prd_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$prdprwogo1 + X$prdprandgo1
                         + X$prdgowopr1 + X$prdgoandpr1
                         + X$dincgoprd 
                         + gspbar + diggspprd 
                         + X$dcoaprd)
prd.pcsegsp <- pcse(prdshestgsp, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
#
display(panshestgsp)
summary(pan.pcsegsp)
display(prishestgsp)
summary(pri.pcsegsp)
display(prdshestgsp)
summary(prd.pcsegsp)
#
rm(tmp1, tmp2, tmp3, gspbar, 
   digxdippan, diggsppan,   dipgsppan,   digdipgsppan, 
   digxdippri,  diggsppri,   dipgsppri,  digdipgsppri, 
   diggspprd,   
   panshestgsp, prishestgsp, prdshestgsp, pan.pcsegsp, pri.pcsegsp, prd.pcsegsp) 


####################################################################################
####        CORRELATION BETWEEN GOV AND PRES VOTES, SENSITIVITY ANALYSIS         ###
####################################################################################
#
tmp1 <- data.frame(gov=usedat$gopansh, pres=usedat$panpr1)
tmp2 <- data.frame(dconcgo=usedat$dconcgo, dconcpr=usedat$dconcpr, yr=usedat$yr)
tmp3 <- tmp1[tmp2$dconcgo==1 & tmp2$dconcpr==1,]
tmp4 <- tmp1[tmp2$dconcgo==1 & tmp2$dconcpr==1 & tmp2$yr>1996,]
#
## PAN 1979--2009 both concur
cor(tmp3)
## PAN 1997--2009 both concur
cor(tmp4)
#
tmp1 <- data.frame(gov=usedat$goprish, pres=usedat$pripr1)
tmp2 <- data.frame(dconcgo=usedat$dconcgo, dconcpr=usedat$dconcpr, yr=usedat$yr)
tmp3 <- tmp1[tmp2$dconcgo==1 & tmp2$dconcpr==1,]
tmp4 <- tmp1[tmp2$dconcgo==1 & tmp2$dconcpr==1 & tmp2$yr>1996,]
#
## PRI 1979--2009 both concur
cor(tmp3)
## PRI 1997--2009 both concur
cor(tmp4)
#
tmp1 <- data.frame(gov=usedat$goprdsh, pres=usedat$prdpr1)
tmp2 <- data.frame(dconcgo=usedat$dconcgo, dconcpr=usedat$dconcpr, yr=usedat$yr)
tmp3 <- tmp1[tmp2$dconcgo==1 & tmp2$dconcpr==1,]
tmp4 <- tmp1[tmp2$dconcgo==1 & tmp2$dconcpr==1 & tmp2$yr>1996,]
#
## PRD 1979--2009 both concur
cor(tmp3)
## PRD 1997--2009 both concur
cor(tmp4)
rm(tmp1,tmp2,tmp3,tmp4)
#
## SENSITIVITY ANALYSIS: DROPS ONE OBS AT A TIME TO REPORT % CHANGE IN COEFS
tmp <- panshr
#tmp <- panshr[panshr$yr>1996,]
edoab <- tmp$edo
require("car")
edoab <- recode(edoab, 
       "'Aguascalientes'=       'ags';
        'Baja California'=      'bcn';  
        'Baja California Sur'=  'bcs';  
        'Campeche'=             'cam';  
        'Coahuila'=             'coa';  
        'Colima'=               'col';  
        'Chiapas'=              'cps';  
        'Chihuahua'=            'cua';  
        'Distrito Federal'=     'df';
        'Durango'=              'dgo';  
        'Guanajuato'=           'gua';  
        'Guerrero'=             'gue';  
        'Hidalgo'=              'hgo';  
        'Jalisco'=              'jal';  
        'Mexico'=               'mex';  
        'Michoacan'=            'mic';  
        'Morelos'=              'mor';  
        'Nayarit'=              'nay';  
        'Nuevo Leon'=           'nl';
        'Oaxaca'=               'oax';  
        'Puebla'=               'pue';  
        'Queretaro'=            'que';  
        'Quintana Roo'=         'qui';  
        'San Luis Potosi'=      'san';  
        'Sinaloa'=              'sin';  
        'Sonora'=               'son';  
        'Tabasco'=              'tab';  
        'Tamaulipas'=           'tam';  
        'Tlaxcala'=             'tla';  
        'Veracruz'=             'ver';  
        'Yucatan'=              'yuc';  
        'Zacatecas'=            'zac' ")    
N <- nrow(tmp)
X <- data.frame(as.matrix(cbind (tmp[,6:ncol(tmp)])))
mod     <-   lm (pansh ~ pan_prev + dconcgowopr + dconcprwogo + dconcgoandpr 
                         + pangowopr1 + pangoandpr1 
                         + panprwogo1 + panprandgo1 
                         + dincgopan + dincprpan + panxgspbar + dcoapan,
                         data=X)
base <- mod$coefficients
threshold <- .1
chgpan <- data.frame( matrix(NA, nrow=N, ncol=length(mod$coefficients)) )
colnames(chgpan) <- names(mod$coefficients); colnames(chgpan)[1] <- "Intercept"
for (i in 1:N){
    mod     <-   lm (pansh ~ pan_prev + dconcgowopr + dconcprwogo + dconcgoandpr 
                         + pangowopr1 + pangoandpr1 
                         + panprwogo1 + panprandgo1 
                         + dincgopan + dincprpan + panxgspbar + dcoapan,
                         data=X[-i,])
#    chgpan[i,] <- (mod$coefficients - base)*100/base
    chgpan[i,] <- mod$coefficients
    } 
vnam <- c("Intercept", "RecentDvote", "GovOnlyConcurs", "PresOnlyConcurs", "Gov&PresConcur", 
             "Gvote|GovOnlyConcurs", "Gvote|Gov&PresConcur", "Pvote|PresOnlyConcurs", "Pvote|Gov&PresConcur", 
             "IncumbentGovernor", "IncumbentPresident", "Economy", "PartyCoalesced")
pt <- "PAN"
tmp1 <- chgpan$Intercept; j <- 1
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$pan_prev; j <- 2
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$dconcgowopr; j <- 3
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$dconcprwogo; j <- 4
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$dconcgoandpr; j <- 5
plot(tmp1, main=paste(pt, vnam[j])) 
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$pangowopr1; j <- 6
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$pangoandpr1; j <- 7
plot(tmp1, main=paste(pt, vnam[j])) 
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$panprwogo1; j <- 8
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$panprandgo1; j <- 9
plot(tmp1, main=paste(pt, vnam[j])) 
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$dincgopan; j <- 10
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$dincprpan; j <- 11
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$panxgspbar; j <- 12
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpan$dcoapan; j <- 13
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp <- prishr
#tmp <- prishr[prishr$yr>1996,]
require("car")
N <- nrow(tmp)
X <- data.frame(as.matrix(cbind (tmp[,6:ncol(tmp)])))
mod     <-   lm (prish ~ pri_prev + dconcgowopr + dconcprwogo + dconcgoandpr 
                         + prigowopr1 + prigoandpr1 
                         + priprwogo1 + priprandgo1 
                         + dincgopri + dincprpri + prixgspbar + dcoapri,
                         data=X)
base <- mod$coefficients
chgpri <- data.frame( matrix(NA, nrow=N, ncol=length(mod$coefficients)) )
colnames(chgpri) <- names(mod$coefficients); colnames(chgpri)[1] <- "Intercept"
for (i in 1:N){
    mod     <-   lm (prish ~ pri_prev + dconcgowopr + dconcprwogo + dconcgoandpr 
                         + prigowopr1 + prigoandpr1 
                         + priprwogo1 + priprandgo1 
                         + dincgopri + dincprpri + prixgspbar + dcoapri,
                         data=X[-i,])
#    chgpri[i,] <- (mod$coefficients - base)*100/base
    chgpri[i,] <- mod$coefficients
    } 
vnam <- c("Intercept", "RecentDvote", "GovOnlyConcurs", "PresOnlyConcurs", "Gov&PresConcur", 
             "Gvote|GovOnlyConcurs", "Gvote|Gov&PresConcur", "Pvote|PresOnlyConcurs", "Pvote|Gov&PresConcur", 
             "IncumbentGovernor", "IncumbentPresident", "Economy", "PartyCoalesced")
pt <- "PRI"
tmp1 <- chgpri$Intercept; j <- 1
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$pri_prev; j <- 2
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$dconcgowopr; j <- 3
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$dconcprwogo; j <- 4
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$dconcgoandpr; j <- 5
plot(tmp1, main=paste(pt, vnam[j])) 
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$prigowopr1; j <- 6
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$prigoandpr1; j <- 7
plot(tmp1, main=paste(pt, vnam[j])) 
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$priprwogo1; j <- 8
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$priprandgo1; j <- 9
plot(tmp1, main=paste(pt, vnam[j])) 
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$dincgopri; j <- 10
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$dincprpri; j <- 11
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$prixgspbar; j <- 12
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgpri$dcoapri; j <- 13
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp <- prdshr
#tmp <- prdshr[prdshr$yr>1996,]
require("car")
N <- nrow(tmp)
X <- data.frame(as.matrix(cbind (tmp[,6:ncol(tmp)])))
mod     <-   lm (prdsh ~ prd_prev + dconcgowopr + dconcprwogo + dconcgoandpr 
                         + prdgowopr1 + prdgoandpr1 
                         + prdprwogo1 + prdprandgo1 
                         + dincgoprd +             prdxgspbar + dcoaprd,
                         data=X)
base <- mod$coefficients
chgprd <- data.frame( matrix(NA, nrow=N, ncol=length(mod$coefficients)) )
colnames(chgprd) <- names(mod$coefficients); colnames(chgprd)[1] <- "Intercept"
for (i in 1:N){
    mod     <-   lm (prdsh ~ prd_prev + dconcgowopr + dconcprwogo + dconcgoandpr 
                         + prdgowopr1 + prdgoandpr1 
                         + prdprwogo1 + prdprandgo1 
                         + dincgoprd +             prdxgspbar + dcoaprd,
                         data=X[-i,])
#    chgprd[i,] <- (mod$coefficients - base)*100/base
    chgprd[i,] <- mod$coefficients
    } 
vnam <- c("Intercept", "RecentDvote", "GovOnlyConcurs", "PresOnlyConcurs", "Gov&PresConcur", 
             "Gvote|GovOnlyConcurs", "Gvote|Gov&PresConcur", "Pvote|PresOnlyConcurs", "Pvote|Gov&PresConcur", 
             "IncumbentGovernor", "Economy", "PartyCoalesced")
pt <- "PRD"
tmp1 <- chgprd$Intercept; j <- 1
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$prd_prev; j <- 2
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$dconcgowopr; j <- 3
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$dconcprwogo; j <- 4
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$dconcgoandpr; j <- 5
plot(tmp1, main=paste(pt, vnam[j])) 
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$prdgowopr1; j <- 6
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$prdgoandpr1; j <- 7
plot(tmp1, main=paste(pt, vnam[j])) 
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$prdprwogo1; j <- 8
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$prdprandgo1; j <- 9
plot(tmp1, main=paste(pt, vnam[j])) 
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$dincgoprd; j <- 10
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$prdxgspbar; j <- 11
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)
#
tmp1 <- chgprd$dcoaprd; j <- 12
plot(tmp1, main=paste(pt, vnam[j]))
abline(h=base[j], col="blue")
for (i in 1:N){
    if ( tmp1[i] < base[j] - abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    if ( tmp1[i] > base[j] + abs(base[j]*threshold) )  {  text( i,tmp1[i], paste(edoab[i], tmp$yr[i], sep=""), cex=.65, pos=4 )  };
    abline(h=base[j]*.95); abline(h=base[j]*1.05); abline(h=base[j]*.9, col="red"); abline(h=base[j]*1.1, col="red")
    }
setwd("d:/01/Dropbox/data/elecs/MXelsCalendGovt/coattails/09update/graphs/sensitivity") ## WHERE YOU WANT GRAPHS SAVED
savePlot(filename = paste(pt, j, sep=""), type = "pdf")
setwd(workdir)


######################################################
## COMPARE MODELS WITH AND WITHOUT COALITION DUMMY  ##
######################################################
### SHARES PAN                                              ###
tmp <- panshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
mod1     <-   lm (depvar ~ X$pan_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$panprwogo1 + X$panprandgo1
                         + X$pangowopr1 + X$pangoandpr1
                         + X$dincgopan + X$dincprpan + X$panxgspbar + X$dcoapan)
mod2     <-   lm (depvar ~ X$pan_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$panprwogo1 + X$panprandgo1
                         + X$pangowopr1 + X$pangoandpr1
                         + X$dincgopan + X$dincprpan + X$panxgspbar)
display(mod1)
display(mod2)
### SHARES PRI                                              ###
tmp <- prishr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprish <- colnames(X)
mod1 <-   lm (depvar ~ X$pri_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$priprwogo1 + X$priprandgo1
                         + X$prigowopr1 + X$prigoandpr1
                         + X$dincgopri + X$dincprpri + X$prixgspbar + X$dcoapri)
mod2 <-   lm (depvar ~ X$pri_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$priprwogo1 + X$priprandgo1
                         + X$prigowopr1 + X$prigoandpr1
                         + X$dincgopri + X$dincprpri + X$prixgspbar)
display(mod1)
display(mod2)
### SHARES PRD                                              ###
tmp <- prdshr
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprdsh <- colnames(X)
mod1 <-   lm (depvar ~ X$prd_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$prdprwogo1 + X$prdprandgo1
                         + X$prdgowopr1 + X$prdgoandpr1
                         + X$dincgoprd + X$prdxgspbar + X$dcoaprd)
mod2 <-   lm (depvar ~ X$prd_prev + X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$prdprwogo1 + X$prdprandgo1
                         + X$prdgowopr1 + X$prdgoandpr1
                         + X$dincgoprd + X$prdxgspbar)
display(mod1)
display(mod2)
# Need to do the pcse for prd (some changes in coefs) to verify significance
mod2.pcse <- pcse(mod2, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod2.pcse) 


####################################################################################
### RUN MODEL FOR 1979-1985 PERIOD ONLY (DROPS INCUMBENCY AND COALITION DUMMIES) ###
####################################################################################
usedat <- alldat[alldat$yr<=1985,]
panshr <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                pansh=usedat$pansh, pan_prev=usedat$pan_prev, dconcpr=usedat$dconcpr, 
                panpr1=usedat$panpr1, dconcgo=usedat$dconcgo, pangoandpr1=usedat$pangoandpr1, 
                pangowopr1=usedat$pangowopr1, dincgopan=usedat$dincgopan, dincprpan=usedat$dincprpan, 
                panxgspbar=usedat$panxgspbar, dcoapan=usedat$dcoapan,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, panprandgo1=usedat$panprandgo1, panprwogo1=usedat$panprwogo1)
prishr <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot, 
                prish=usedat$prish, pri_prev=usedat$pri_prev, dconcpr=usedat$dconcpr, 
                pripr1=usedat$pripr1, dconcgo=usedat$dconcgo, prigoandpr1=usedat$prigoandpr1, 
                prigowopr1=usedat$prigowopr1, dincgopri=usedat$dincgopri, dincprpri=usedat$dincprpri, 
                prixgspbar=usedat$prixgspbar, dcoapri=usedat$dcoapri,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, priprandgo1=usedat$priprandgo1, priprwogo1=usedat$priprwogo1)
prdshr <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                prdsh=usedat$prdsh, prd_prev=usedat$prd_prev, dconcpr=usedat$dconcpr, 
                prdpr1=usedat$prdpr1, dconcgo=usedat$dconcgo, prdgoandpr1=usedat$prdgoandpr1, 
                prdgowopr1=usedat$prdgowopr1, dincgoprd=usedat$dincgoprd,  
                prdxgspbar=usedat$prdxgspbar, dcoaprd=usedat$dcoaprd,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, prdprandgo1=usedat$prdprandgo1, prdprwogo1=usedat$prdprwogo1)
#
tmp <- panshr
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
panshest     <-   lm (depvar ~ X$pan_prev + X$dconcpr + X$dconcgo
                         + X$panpr1 
                         + X$pangowopr1 + X$pangoandpr1 
                         + X$panxgspbar)
mod <- panshest
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)
#
tmp <- prishr
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprish <- colnames(X)
prishest <-   lm (depvar ~ X$pri_prev + X$dconcpr + X$dconcgo
                         + X$pripr1 
                         + X$prigowopr1 + X$prigoandpr1 
                         + X$prixgspbar)
mod <- prishest
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)
#
## PRD MODEL CANT BE RUN REALLY, TOO FEW GUBERNATORIAL CANDIDATES
tmp <- prdshr
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprdsh <- colnames(X)
prdshest <-   lm (depvar ~ X$prd_prev + X$dconcpr + X$dconcgo
                         + X$prdpr1 
                         + X$prdgowopr1 + X$prdgoandpr1 
                         + X$prdxgspbar)
mod <- prdshest
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)




#########################################################
#########################################################
###          DATA FOR RESIDUAL REGRESSIONS            ###
#########################################################
#########################################################
usedat <- alldat                      
##
panresid <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                panshres=usedat$panshres, dconcpr=usedat$dconcpr,
                panpr=usedat$panpr, dconcgo=usedat$dconcgo, pangoandpr=usedat$pangoandpr,
                pangowopr=usedat$pangowopr, dincgopan=usedat$dincgopan, dincprpan=usedat$dincprpan,
                panxgspbar=usedat$panxgspbar, dcoapan=usedat$dcoapan,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, panprandgo=usedat$panprandgo, panprwogo=usedat$panprwogo)
priresid <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                prishres=usedat$prishres, dconcpr=usedat$dconcpr,
                pripr=usedat$pripr, dconcgo=usedat$dconcgo, prigoandpr=usedat$prigoandpr,
                prigowopr=usedat$prigowopr, dincgopri=usedat$dincgopri, dincprpri=usedat$dincprpri,
                prixgspbar=usedat$prixgspbar, dcoapri=usedat$dcoapri,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, priprandgo=usedat$priprandgo, priprwogo=usedat$priprwogo)
prdresid <- data.frame(edon=usedat$edon, edo=usedat$edo, yr=usedat$yr, goyr=usedat$goyr, ptot=usedat$ptot,
                prdshres=usedat$prdshres, dconcpr=usedat$dconcpr,
                prdpr=usedat$prdpr, dconcgo=usedat$dconcgo, prdgoandpr=usedat$prdgoandpr,
                prdgowopr=usedat$prdgowopr, dincgoprd=usedat$dincgoprd, 
                prdxgspbar=usedat$prdxgspbar, dcoaprd=usedat$dcoaprd,
                dconcgowopr=usedat$dconcgowopr, dconcgoandpr=usedat$dconcgoandpr,
                dconcprwogo=usedat$dconcprwogo, prdprandgo=usedat$prdprandgo, prdprwogo=usedat$prdprwogo)
#
###############################################################
### RESIDUALS PAN                                           ###
###############################################################
#
tmp <- panresid
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varspanres <- colnames(X)
panresest     <-   lm (depvar ~ X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$panprwogo + X$panprandgo
                         + X$pangowopr + X$pangoandpr
                         + X$dincgopan + X$dincprpan + X$panxgspbar + X$dcoapan)
#
## REPORT MODEL 4
mod <- panresest
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

## Tests of beta3+beta6 and beta4+beta8
cv <- vcovPC(mod, groupN=tmp$edon, groupT=tmp$yr) ## EXTRACT VAR-COV MATRIX FOR TESTS
b2 <- as.numeric(mod.pcse$b[2]); b5 <- as.numeric(mod.pcse$b[5])
v2 <- cv[2,2]; v5 <- cv[5,5]; c25 <- cv[2,5]; se25 <- sqrt(v2 + v5 + 2*c25)
test <- (b2+b5)/se25; test
dt(test, df=mod.pcse$df) ## one-tailed
b3 <- as.numeric(mod.pcse$b[3]); b7 <- as.numeric(mod.pcse$b[7])
v3 <- cv[3,3]; v7 <- cv[7,7]; c37 <- cv[3,7]; se37 <- sqrt(v3 + v7 + 2*c37)
test <- (b3+b7)/se37; test
dt(test, df=mod.pcse$df) ## one-tailed


## BAYESIAN ESTIMATION
tmp <- panresid
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- as.matrix(cbind (cons, tmp$dconcprwogo, tmp$dconcgowopr, tmp$dconcgoandpr, 
                      tmp$panprwogo, tmp$panprandgo,
                      tmp$pangowopr, tmp$pangoandpr, 
                      tmp$dincgopan, tmp$dincprpan, tmp$panxgspbar, tmp$dcoapan))
varspansh <- c("cons", "dconcprwogo", "dconcgowopr", "dconcgoandpr", "panprwogo", "panprandgo",
               "pangowopr", "pangoandpr", "dincgopan", "dincprpan", "panxgspbar", "dcoapan")
colnames(X) <- varspansh
K <- ncol(X)
#
res.data <- list ("depvar", "X", "N", "K")
res.inits <- function (){
    list (
    beta=rnorm(K),
    sigma=runif(1)
    )
    }
res.parameters <- c("beta", "sigma")
#
#test ride to see program works
tmp <- bugs (res.data, res.inits, res.parameters, 
                "linearModel.txt", n.chains=3, 
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))
#longer run
panresBest <- bugs (res.data, res.inits, res.parameters, 
                "linearModel.txt", n.chains=4, 
                n.iter=5000, n.thin=10, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(panresBest)
print(panresBest)

###############################################################
### RESIDUALS PRI                                           ###
###############################################################

tmp <- priresid
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprires <- colnames(X)
priresest     <-   lm (depvar ~ X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$priprwogo + X$priprandgo
                         + X$prigowopr + X$prigoandpr
                         + X$dincgopri + X$dincprpri + X$prixgspbar + X$dcoapri)
#
## REPORT MODEL 4
mod <- priresest
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

## Tests of beta3+beta6 and beta4+beta8
cv <- vcovPC(mod, groupN=tmp$edon, groupT=tmp$yr) ## EXTRACT VAR-COV MATRIX FOR TESTS
b2 <- as.numeric(mod.pcse$b[2]); b5 <- as.numeric(mod.pcse$b[5])
v2 <- cv[2,2]; v5 <- cv[5,5]; c25 <- cv[2,5]; se25 <- sqrt(v2 + v5 + 2*c25)
test <- (b2+b5)/se25; test
dt(test, df=mod.pcse$df) ## one-tailed
b3 <- as.numeric(mod.pcse$b[3]); b7 <- as.numeric(mod.pcse$b[7])
v3 <- cv[3,3]; v7 <- cv[7,7]; c37 <- cv[3,7]; se37 <- sqrt(v3 + v7 + 2*c37)
test <- (b3+b7)/se37; test
dt(test, df=mod.pcse$df) ## one-tailed

## BAYESIAN ESTIMATION
tmp <- priresid
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- as.matrix(cbind (cons, tmp$dconcprwogo, tmp$dconcgowopr, tmp$dconcgoandpr, 
                      tmp$priprwogo, tmp$priprandgo,
                      tmp$prigowopr, tmp$prigoandpr, 
                      tmp$dincgopri, tmp$dincprpri, tmp$prixgspbar, tmp$dcoapri))
varsprish <- c("cons", "dconcprwogo", "dconcgowopr", "dconcgoandpr", "priprwogo", "priprandgo",
               "prigowopr", "prigoandpr", "dincgopri", "dincprpri", "prixgspbar", "dcoapri")
colnames(X) <- varsprish
K <- ncol(X)
#
res.data <- list ("depvar", "X", "N", "K")
res.inits <- function (){
    list (
    beta=rnorm(K),
    sigma=runif(1)
    )
    }
res.parameters <- c("beta", "sigma")
##
#test ride to see program works
tmp <- bugs (res.data, res.inits, res.parameters, 
                "linearModel.txt", n.chains=3, 
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))
##
#longer run
priresBest <- bugs (res.data, res.inits, res.parameters, 
                "linearModel.txt", n.chains=4, 
                n.iter=5000, n.thin=10, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(priresBest)
print(priresBest)

###############################################################
### RESIDUALS PRD                                           ###
###############################################################

tmp <- prdresid
N <- nrow(tmp) 
cons <- rep(1, N)
depvar <- tmp[,6]
X <- data.frame(as.matrix(cbind (cons, tmp[,7:ncol(tmp)])))
K <- ncol(X)
varsprdres <- colnames(X)
prdresest     <-   lm (depvar ~ X$dconcprwogo + X$dconcgowopr + X$dconcgoandpr
                         + X$prdprwogo + X$prdprandgo
                         + X$prdgowopr + X$prdgoandpr
                         + X$dincgoprd + X$prdxgspbar + X$dcoaprd)
#
## REPORT MODEL 4
mod <- prdresest
display(mod)
mod.pcse <- pcse(mod, groupN=tmp$edon, groupT=tmp$yr) ## PCSEs
summary(mod.pcse)

## Tests of beta3+beta6 and beta4+beta8
cv <- vcovPC(mod, groupN=tmp$edon, groupT=tmp$yr) ## EXTRACT VAR-COV MATRIX FOR TESTS
b2 <- as.numeric(mod.pcse$b[2]); b5 <- as.numeric(mod.pcse$b[5])
v2 <- cv[2,2]; v5 <- cv[5,5]; c25 <- cv[2,5]; se25 <- sqrt(v2 + v5 + 2*c25)
test <- (b2+b5)/se25; test
dt(test, df=mod.pcse$df) ## one-tailed
b3 <- as.numeric(mod.pcse$b[3]); b7 <- as.numeric(mod.pcse$b[7])
v3 <- cv[3,3]; v7 <- cv[7,7]; c37 <- cv[3,7]; se37 <- sqrt(v3 + v7 + 2*c37)
test <- (b3+b7)/se37; test
dt(test, df=mod.pcse$df) ## one-tailed

## BAYESIAN ESTIMATION
tmp <- prdresid
N <- nrow(tmp)
cons <- rep(1, N)
depvar <- tmp[,6]
X <- as.matrix(cbind (cons, tmp$dconcprwogo, tmp$dconcgowopr, tmp$dconcgoandpr, 
                      tmp$prdprwogo, tmp$prdprandgo,
                      tmp$prdgowopr, tmp$prdgoandpr, 
                      tmp$dincgoprd, tmp$dincprprd, tmp$prdxgspbar, tmp$dcoaprd))
varsprdsh <- c("cons", "dconcprwogo", "dconcgowopr", "dconcgoandpr", "prdprwogo", "prdprandgo",
               "prdgowopr", "prdgoandpr", "dincgoprd", "dincprprd", "prdxgspbar", "dcoaprd")
colnames(X) <- varsprdsh
K <- ncol(X)
#
res.data <- list ("depvar", "X", "N", "K")
res.inits <- function (){
    list (
    beta=rnorm(K),
    sigma=runif(1)
    )
    }
res.parameters <- c("beta", "sigma")

#test ride to see program works
tmp <- bugs (res.data, res.inits, res.parameters, 
                "linearModel.txt", n.chains=3, 
                n.iter=20, n.thin=2, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))
##
#longer run
prdresBest <- bugs (res.data, res.inits, res.parameters, 
                "linearModel.txt", n.chains=4, 
                n.iter=5000, n.thin=10, debug=F,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
                program = c("WinBUGS"))

plot(prdresBest)
print(prdresBest)

## IMPORTS RAW VOTES (BEFORE APPORTIONING COALITION VOTE SHARES
rawvote <- read.csv("rawvote.out", header=TRUE)
