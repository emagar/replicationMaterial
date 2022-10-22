

#################################################################################################
## NO NEED TO RUN THIS SCRIPT IF THE PURPOSE IS PURE REPLICATION OF RESULTS IN PUBLISHED PAPER ##
## MANIPULATE AND SOURCE SCRIPT FROM WITHIN replication.r TO MODIFY OUR CODING CHOICES         ##
## Prepared by Eric Magar 10-jul-2019                                                          ##
#################################################################################################

# clean memory
rm(list=ls())

# set data directory here
datdir <- "/home/eric/Dropbox/data/latAm/chile/replication/data/" 
setwd(datdir)

# prepare object with boletin filenames
files <- dir(paste(datdir, "raw/boletines/", sep = "")) # reads files existing in boletines directory
files <- files[grep("bol[0-9]+.*.txt", files)] # filters names other than bol*.txt
files <- paste("raw/boletines/", files, sep = "")  # adds path
## files <- read.csv("boletines/1id-bl.csv", encoding = 'utf-8', stringsAsFactors = FALSE) # reads universe of filenames
## files <- files$bl
## files <- paste("boletines/bol", files, ".txt", sep = "")
# grep("2397", files) # debug

############################################################################################
## Notes:                                                                                 ##
## (1) Primary source data was scraped from the Cámara de Diputados web page using Python ##
##     and its Selenium library (by Baiju Muthukadan, [[https://muthukadan.net/]]). Files ##
##     data/raw/boletines/1getBol.py and data/raw/boletines/1loopToGetBol.py contain the  ##
##     code used.                                                                         ##
## (2) The primary source can be visited at https://www.camara.cl. Follow the 'Proyectos  ##
##     de Ley' tab, input a boletin number (e.g. 1201-13).                                ##
############################################################################################

# prepare object to receive bill histories
I <- length(files)
bills <- list(
    #info = data.frame(n=1:I), # matrix with basic bill info
    info = NULL,                        # matrix to receive basic bill info
    urgencias = vector("list", I),      # empty list with urgencias info to be systematized
    sponsors = vector("list", I),       # idem
    hitos = vector("list", I),
    votes = vector("list", I),
    veto = vector("list", I),
    reports = vector("list", I)
    )

# loop over files
library(lubridate)
for (i in 1:I){
    #i <- grep("127-01", files) # debug: read one boletin
    message(sprintf("loop %s of %s", i, I))
    bol <- readLines( files[i], encoding = "utf-8" )
    bol <- gsub(pattern = "[\"]", replacement = "", bol) # some cleaning: removes double quotes inside text
                                        ## get summary info
    start <- grep(pattern = "emmStart Summary", x = bol)
    end <- grep(pattern = "Summary emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with summary info
    chunk <- sub(pattern = "emmStart Summary.*,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Summary emmEnd"   , replacement = "", chunk) # cleans
                                        ## plug info into bill histories object
    tmp <- chunk[grep("Legislatura:", chunk)]
    bill <- data.frame(leg = sub(pattern = "Leg.*: ([0-9]+)", replacement = "\\1", x = tmp))
                                        #
    tmp <- chunk[grep("Fecha de ingreso:", chunk)]
    tmp <- sub(pattern = "Fecha de ingreso: [a-zA-Z]+ (.*)", replacement = "\\1", x = tmp)
    tmp <- gsub(pattern = " de ", replacement = "-", x = tmp)
    tmp <- gsub(pattern = "enero"     , replacement = "1", x = tmp)
    tmp <- gsub(pattern = "febrero"   , replacement = "2", x = tmp)
    tmp <- gsub(pattern = "marzo"     , replacement = "3", x = tmp)
    tmp <- gsub(pattern = "abril"     , replacement = "4", x = tmp)
    tmp <- gsub(pattern = "mayo"      , replacement = "5", x = tmp)
    tmp <- gsub(pattern = "junio"     , replacement = "6", x = tmp)
    tmp <- gsub(pattern = "julio"     , replacement = "7", x = tmp)
    tmp <- gsub(pattern = "agosto"    , replacement = "8", x = tmp)
    tmp <- gsub(pattern = "septiembre", replacement = "9", x = tmp)
    tmp <- gsub(pattern = "octubre"   , replacement = "10", x = tmp)
    tmp <- gsub(pattern = "noviembre" , replacement = "11", x = tmp)
    tmp <- gsub(pattern = "diciembre" , replacement = "12", x = tmp)
    bill$dateIn <- dmy(tmp)
                                        #
    tmp <- chunk[grep("Estado:", chunk)]
    tmp <- sub(pattern = "Estado: (.*)", replacement = "\\1", x = tmp)
    bill$state <- tmp
                                        #
    bill$bol <- sub(pattern = ".*/bol(.*).txt", replacement = "\\1", files[i])
                                        #
    tmp <- chunk[grep("Refundido con:", chunk)]
    if (length(tmp) == 0){
        tmp <- "no"
    } else {
        tmp <- sub(pattern = ".*: (.*)", replacement = "\\1", tmp)
    }
    bill$refundido <- tmp
                                        #
    tmp <- chunk[grep("Materia:", chunk)]
    tmp <- sub(pattern = "Materia: (.*)", replacement = "\\1", tmp)
    bill$materia <- tmp
                                        #
    tmp <- chunk[grep("Iniciativa:", chunk)]
    tmp <- sub(pattern = "Iniciativa: (.*)", replacement = "\\1", tmp)
    tmp <- sub(pattern = "Mensaje", replacement = "1", tmp)
    tmp <- sub(pattern = "Moción", replacement = "0", tmp)
    bill$dmensaje <- as.numeric(tmp)
                                        #
    tmp <- chunk[grep("Cámara de origen:", chunk)]
    tmp <- sub(pattern = "Cámara de origen: (.*)", replacement = "\\1", tmp)
    tmp <- sub(pattern = ".*[Dd]iputados", replacement = "dip", tmp)
    tmp <- sub(pattern = ".*[Ss]enado", replacement = "sen", tmp)
    bill$init <- tmp
                                        #
                                        # find hitos tramitación
    start <- grep(pattern = "emmStart Hitos", x = bol)
    end <- grep(pattern = "Hitos emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Hitos.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Hitos emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasHitos <- ifelse(length(tmp)>0, "no", "yes")
    bills$hitos[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # find informes
    start <- grep(pattern = "emmStart Informes", x = bol)
    end <- grep(pattern = "Informes emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Informes.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Informes emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasReport <- ifelse(length(tmp)>0, "no", "yes")
    bills$reports[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # find urgencias
    start <- grep(pattern = "emmStart Urgencias", x = bol)
    end <- grep(pattern = "Urgencias emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Urgencias.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Urgencias emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasUrg <- ifelse(length(tmp)>0, "no", "yes")
    if (length(tmp)==0){
        tmpD <- chunk[-grep(pattern = "Sin urgencia", chunk)]
        bill$hasUrg <- ifelse(length(tmpD)==1, "no", "yes") # dropping sin urgencia lines, are there any remaining other than the date etc. heading?
    }
    bills$urgencias[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # find autores
    start <- grep(pattern = "emmStart Autores", x = bol)
    end <- grep(pattern = "Autores emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Autores.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Autores emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasSpon <- ifelse(length(tmp)>0, "no", "yes")
    bills$sponsors[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # find votaciones
    start <- grep(pattern = "emmStart Votaciones", x = bol)
    end <- grep(pattern = "Votaciones emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Votaciones.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Votaciones emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasVot <- ifelse(length(tmp)>0, "no", "yes")
    bills$votes[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # find veto
    start <- grep(pattern = "emmStart Veto", x = bol)
    end <- grep(pattern = "Veto emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Veto.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Veto emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasVeto <- ifelse(length(tmp)>0, "no", "yes")
    bills$veto[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # add bill info to info data.frame
    if (i==1){
        bills$info <- bill
    } else {
        bills$info <- rbind(bills$info, bill)
    }
}

summary(bills) # list
rm(bill, bol, chunk, end, files, i, start, tmp, tmpD) # clean

####################################
# systematize hitos de tramitación #
####################################
#
nHitos <- rep(0,I)
bills$info$hasUrgHU <- "." # will receive urgencia info from hitos and urgencia tab
#bills$info$hasVetoH <- "." # will receive veto info from hitos
#
bills$info$debug <- 0 # debug prep --- can receive anything being checked in data
#
bills$hitosRaw <- bills$hitos # keeps raw info for future revision
#
sel <- 1:I
library(lubridate)
for (i in sel){
    message(sprintf("loop %s of %s", i, I))
    tmp <- bills$hitosRaw[[i]]
    tmp <- gsub(pattern = ",", replacement = "", tmp) # drop commas
    tmp <- tmp[-1] # drop line with titles
    N <- length(tmp) # number of hitos
    output <- data.frame(rawText=tmp) # initialize output object
                                        # format dates
    tmp2 <- sub(pattern = "^([0-9]{2}[ .A-Za-z]+[0-9]{4}).*", replacement = "\\1", tmp, perl = TRUE)
    tmp2 <- sub(pattern = " de ", replacement = "/", tmp2, perl = TRUE)
    tmp2 <- sub(pattern = "[. ][de ]*", replacement = "./", tmp2, perl = TRUE)
    tmp2 <- gsub(pattern = "Ene.", replacement = "1", x = tmp2)
    tmp2 <- gsub(pattern = "Feb.", replacement = "2", x = tmp2)
    tmp2 <- gsub(pattern = "Mar.", replacement = "3", x = tmp2)
    tmp2 <- gsub(pattern = "Abr.", replacement = "4", x = tmp2)
    tmp2 <- gsub(pattern = "May.", replacement = "5", x = tmp2)
    tmp2 <- gsub(pattern = "Jun.", replacement = "6", x = tmp2)
    tmp2 <- gsub(pattern = "Jul.", replacement = "7", x = tmp2)
    tmp2 <- gsub(pattern = "Ago.", replacement = "8", x = tmp2)
    tmp2 <- gsub(pattern = "Sep.", replacement = "9", x = tmp2)
    tmp2 <- gsub(pattern = "Oct.", replacement = "10", x = tmp2)
    tmp2 <- gsub(pattern = "Nov.", replacement = "11", x = tmp2)
    tmp2 <- gsub(pattern = "Dic.", replacement = "12", x = tmp2)
    output$date <- dmy(tmp2, quiet = TRUE)
                                        #
    tmp <- sub(pattern = "^[0-9]{2}[ .A-Za-z]+[0-9]{4}(.*)", replacement = "\\1", tmp, perl = TRUE) # crops object
    tmp <- sub(pattern = "^[ ]+", replacement = "", tmp) # removes spaces at start of line
    output$ses <- (function(x){
        pat <- "^(\\d[/ 0-9ª]*) .*"
        ind <- grep(pat, x)
        x <- gsub(pat, "\\1", x)
        x[-ind] <- ""
        x
    })(tmp)
                                        #
    tmp <- sub(pattern = "^\\d[/ 0-9ª]* (.*)", replacement = "\\1", tmp, perl = TRUE) # crops object
                                        # returns text up to first "/" with trámite name
    output$tramite <- sub(pattern = "^(.*?)/.*", replacement = "\\1", tmp, perl = TRUE) # relies on non-greedy *?
    output$tramite <- sub(pattern = "Primer trámite constitucional[ ]*"   , replacement = "1ero", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Segundo trámite constitucional[ ]*"  , replacement = "2do", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Tercer trámite constitucional[ ]*"   , replacement = "3ero", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*aprobaci[óo]n presidencial.*"      , replacement = "toPres", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Discusión veto.*"                    , replacement = "veto", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Discusión.*[Cc]ámara de [Oo]rigen.*" , replacement = "1ero", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Discusión.*[Cc]ámara [Rr]evisora.*"  , replacement = "2do", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*Tribunal Constitucional.*"         , replacement = "tribunal", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*finalización.*"                    , replacement = "final", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Archivado.*"                         , replacement = "onHold", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Tramitación terminada.*"             , replacement = "ended", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Comisión Mixta.*"                    , replacement = "conf", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Disc[.] [Ii]nforme C[.]Mixta.*"      , replacement = "PostConf", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*insistencia.*"                     , replacement = "chOverride", output$tramite, perl = TRUE) 
    output$tramite <- sub(pattern = ".*Congreso Pleno.*"                  , replacement = "Cong", output$tramite, perl = TRUE)
                                        #
    tmp <- sub(pattern = "^.*?/(.*)", replacement = "\\1", tmp, perl = TRUE) # crops object
    tmp <- sub(pattern = "^[ ]+", replacement = "", tmp) # removes spaces at start
    output$chamber <- sub(pattern = "^(Senado|C. Diputados).*", replacement = "\\1", tmp, perl = TRUE)
    output$chamber <- sub(pattern = "^Senado"                                      , replacement = "sen", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^C. Diputados"                                , replacement = "dip", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Congreso Pleno.*"                           , replacement = "con", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Oficio.*"                                   , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^S[.]E[.] el [PV].*retira.*"                  , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*S[.]E[.] el [PV].*retira.*"                 , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Archivado Cuenta oficio.*"                   , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*vuelve a [Cc]omisión.*"                     , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*indicaciones.*"                             , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Dd]iscusión particular.*"                  , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Queda.*"                                     , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Se rechaza el proyecto.*"                    , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Cambia.*tramitación.*"                       , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Se suspende [la]{2} tramitación.*"           , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Por acuerdo de la Sala.*"                    , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*retir.*tramitación.*"    , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Aa]pr[uo]e*ba.*"  , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Cuenta.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Ratifica.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*desarchiv.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Dd]iscusión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Rr]echazado.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*remite.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*dispone.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*retir.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Aa]rv*chj*iv.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Ss]oli[ci]{2}t.*archiv.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*suspende.*tramitación.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[cC]omisión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*oficio.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*rechaza.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Rr]etira.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*segunda discusión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*general y particular.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*aprobación en particular.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*discutida en particular.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*ratifica.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*aplaza.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*acepta.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Tt]abla.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Pasa a [Cc]omisión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Fracasa la sesión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*petición.*[Cc]omisión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Sala.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*incluye.*proyecto.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "LEY N.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*aprueba.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Urgencia.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Decreto.*"                        , replacement = ".", output$chamber, perl = TRUE)
                                        #
    tmp <- sub(pattern = "^(Senado|C. Diputados)[ ]+(.*)", replacement = "\\2", tmp, perl = TRUE) # crops object
    output$action <- sub(pattern = "(.*)[ ]+Ver$", replacement = "\\1", tmp, perl = TRUE) # removes Ver (download procedure missed link 2 relevant docs.)
                                        #
    ## bills$info$debug[i] <-
    ##    min(sapply(output$chamber, function(x) nchar(x))) # counts the maximum n characters of string in output$chamber (shoud be 3) and plugs to info
                                        #
    bills$hitos[[i]] <- output # replaces raw object with data.frame
    nHitos[i] <- N
}
rm(i, N, output, sel, tmp, tmp2) # housecleaning

# infer missing chamber when possible
#
# for single-line hitos, a missing cannot be inferred (no cases, it seems)
#
sel <- which(nHitos==2) # start with cases with two-line hitos
tmpHasMissing <- tmpInferred <- rep(0,I)
for (i in sel){
    if (bills$hitos[[i]]$chamber[1]=="."){ # is 1st-line chamber missing?
        tmpHasMissing[i] <- 1
        if (bills$hitos[[i]]$chamber[2]!="."){ # if 2nd-line chamber not missing, infer line 1 with it
            bills$hitos[[i]]$chamber[1] <- bills$hitos[[i]]$chamber[2]
            tmpInferred[i] <- 1 # record that change occurred -- for verif
        }
    }
    if (bills$hitos[[i]]$chamber[2]=="."){ # is 2nd-line chamber missing?
        tmpHasMissing[i] <- 1
        if (bills$hitos[[i]]$chamber[2]!="."){ # if 1st-line chamber not missing, infer line 2 with it
            bills$hitos[[i]]$chamber[2] <- bills$hitos[[i]]$chamber[1]
            tmpInferred[i] <- 1 # record that change occurred -- for verif
        }
    }
}
#
sel <- which(nHitos>2) # pick now cases with three-or-more-line hitos
#i <- sel[3] # debug
for (i in sel){
    tmpLinesChMissing <- which(bills$hitos[[i]]$chamber==".")
    if (length(tmpLinesChMissing)==0){ # if no missing chambers, move on in loop
        next
    } else {
        tmpHasMissing[i] <- 1
        if (tmpLinesChMissing[1]==1){ # is 1st-line chamber missing?
            if (bills$hitos[[i]]$chamber[2]!="."){ # if 2nd-line chamber not missing, infer line 1 with it
                bills$hitos[[i]]$chamber[1] <- bills$hitos[[i]]$chamber[2]
                tmpInferred[i] <- 1 # record that change occurred -- for verif
                tmpLinesChMissing <- tmpLinesChMissing[-1] # crop object
            }
        }
    }
    if (length(tmpLinesChMissing)==0){ # if no missing chambers, move on in loop
        next
    } else {
        if (tmpLinesChMissing[length(tmpLinesChMissing)]==nHitos[i]){ # is last-line chamber missing?
            if (bills$hitos[[i]]$chamber[(nHitos[i]-1)]!="."){ # if next-to-last-line chamber not missing, infer last line with it
                bills$hitos[[i]]$chamber[nHitos[i]] <- bills$hitos[[i]]$chamber[(nHitos[i]-1)]
                tmpInferred[i] <- 1 # record that change occurred -- for verif
                tmpLinesChMissing <- tmpLinesChMissing[-length(tmpLinesChMissing)] # crop object
            }
        }
    }
    if (length(tmpLinesChMissing)==0){ # if no missing chambers, move on in loop
        next
    } else {
        for (j in tmpLinesChMissing){ # loop over lines with missing chamber
            if (bills$hitos[[i]]$chamber[(j-1)]!="."){ # is line above's camber non-missing?
                bills$hitos[[i]]$chamber[j] <- bills$hitos[[i]]$chamber[(j-1)] # if so, use it to infer missing chamber
                tmpInferred[i] <- 1 # record that change occurred -- for verif
            }
        }
    }
}

# infer comisión mixta and ejecutivo trámites --- approximates com mixta 
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    select <- grep(pattern = "[Cc]omisi[oó]n [Mm]ixta", bills$hitos[[i]]$action)
    bills$hitos[[i]]$chamber[select] <- "conf"
    sel1 <- grep(pattern = "[Oo]ficio de [Ll]ey al [Ee]jecutivo", bills$hitos[[i]]$action)
    sel2 <- grep(pattern = "[Oo]ficio.*[pP]ara promulgación.*[Pp]resident[ea].*", bills$hitos[[i]]$action)
    select <- union(sel1, sel2)
    bills$hitos[[i]]$chamber[select] <- "ejec"
}

# begin correcting tribunal constitucional steps
tmp <- rep(0, I)
for (i in 1:I){
    if (length(grep(pattern = "tribunal", x = bills$hitos[[i]]$tramite))>0) tmp[i] <- 1
}
sel <- which(tmp==1)
for (i in sel){
    bills$hitos[[i]]$chamber[bills$hitos[[i]]$tramite=="tribunal" & bills$hitos[[i]]$chamber!="ejec"] <- "trib"
}
# begin correcting veto steps
tmp <- rep(0, I)
for (i in 1:I){
    if (length(grep(pattern = "veto", x = bills$hitos[[i]]$tramite))>0) tmp[i] <- 1
}
sel <- which(tmp==1)
for (i in sel){
    bills$hitos[[i]]$chamber[bills$hitos[[i]]$tramite=="veto"] <- "veto"
}
# drop redundant veto entries
for (i in sel){
    vet <- grep("veto",bills$hitos[[i]]$chamber)
    if (length(vet)>1){
        vet <- vet[-1] # those that will be dropped
        vet <- min(vet):max(vet) # include anything sandwiched between veto for manipulation
        bills$ hitos[[i]]$chamber[vet] <- "drop"
    }
}

tmp <- rep(0,I)
for (i in 1:I){
    if (length(which(bills$hitos[[i]]$chamber=="."))==0){
        next
    } else {
        tmp[i] <- 1
    }
}
table(tmp) # NO MORE MISSING CHAMBERS
table(tmpHasMissing) # cases with missing chamber encoutered
table(tmpInferred)   # cases with missing chamber inferred
#
bills$info$nHitos <- nHitos
#
rm(i, j, sel, sel1, sel2, tmp, tmpHasMissing, tmpInferred, tmpLinesChMissing, nHitos, select)

# recode bill status
bills$info$status <- bills$info$state # duplicates to retain original
bills$info$dateOut <- "." # will record date published
bills$info$status <- sub(pattern = "Tramitación terminada.*[0-9]{2}/[0-9]{2}/[0-9]{4}.*", replacement = "statute", bills$info$status)
bills$info$status[grep("Tramitación terminada", bills$info$status)] <- "killed/withdrawn"
sel <- which(bills$info$status=="statute")
bills$info$dateOut[sel] <- sub(pattern = "Tramitación terminada.*([0-9]{2}/[0-9]{2}/[0-9]{4}).*", replacement = "\\1", bills$info$state[sel])
bills$info$dateOut <- sub(pattern = "Tramitación terminada.*", replacement = ".", bills$info$dateOut) # missing values
bills$info$status <- sub(pattern = "Primer trámite.*", replacement = "pending: 1er trámite", bills$info$status)
bills$info$status <- sub(pattern = "Segundo trámite.*", replacement = "pending: 2do trámite", bills$info$status)
bills$info$status <- sub(pattern = "Tercer trámite.*", replacement = "pending: 3er trámite", bills$info$status)
bills$info$status <- sub(pattern = "Archivado.*", replacement = "frozen", bills$info$status)
bills$info$status <- sub(pattern = ".*Mixta.*", replacement = "pending: conference", bills$info$status)
bills$info$status <- sub(pattern = ".*veto.*", replacement = "pending: veto", bills$info$status)
bills$info$status <- sub(pattern = ".*[Ii]nsistencia.*", replacement = "pending: 3er trámite", bills$info$status)
bills$info$status <- sub(pattern = ".*aprobaci[óo]n presidencial.*", replacement = "pending: to executive", bills$info$status)
bills$info$status <- sub(pattern = ".*finalización en Cámara.*", replacement = "pending", bills$info$status)
#
bills$info$dateOut <- dmy(bills$info$dateOut, quiet = TRUE)
table(bills$info$status) # debug

# compact bicameral sequence with dates (may still miss comisión mixta, revise bills$hitos$chamber above SEEMS OK NOW)
bills$tramites <- sapply(1:I, function(x) NULL) # initializes empty list with I elements (unnamed; names would go instead of 1:I)
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    if (length(grep("drop", bills$hitos[[i]]$chamber))>0){ # manipulates hitos object in case there are "drop" lines
        tmp <- bills$hitos[[i]][-grep("drop", bills$hitos[[i]]$chamber),] # drops "drop" rows
    } else {
        tmp <- bills$hitos[[i]]
    }
    if (length(grep("ejec", tmp$chamber))>0){ # manipulates tmp object in case there are "ejec" lines
        # this block may be wrong: assumes that after "ejec", $chamber can only be "veto" or "trib", once each at most...
        selejec <- grep("ejec", tmp$chamber)
        if (length(selejec)>0){
            selejec <- selejec[1] # keep only first/only instance
            after <- selejec:nrow(tmp) # indices that will be mnipulated
            tmp2 <- tmp[after,] # rows to manipulate
            tmp2 <- tmp2[grep("ejec|veto|trib", tmp2$chamber),] # keeps only rows with ejec, veto, or trib...
            tmp2 <- tmp2[duplicated(tmp2$chamber)==FALSE,]      # ... removing duplicates
            tmp <- rbind(tmp[1:(selejec-1),], tmp2)             # re-builds object
        }
        # end block
    }
    these <- rep(1, nrow(tmp)) # object that will select cases to include
    dat.tram <- tmp[,c("date", "chamber")] # keeps only date and tramite in separate object
    if (nrow(tmp)>1){ # next block only if multiline <--- ALL MULTILINE, SEEN ABOVE 
        for (j in 2:length(these)){
            #hour(dat.tram$date[j]) <- ifelse(dat.tram$date[j]==dat.tram$date[j-1], hour(dat.tram$date[j]) + 1, hour(dat.tram$date[j])) # adding 1hr to same dates avoids overlaps
            these[j] <- ifelse(dat.tram$chamber[j]==dat.tram$chamber[j-1], 0, 1) # identify tramites different from previous row as 1s
        }
        dat.tram$to <- dat.tram$date  # duplicates date to retain format
        dat.tram <- dat.tram[these==1,] # compact sequence of tramites
        if (nrow(dat.tram)>1){
            dat.tram$to[1:(nrow(dat.tram)-1)] <- dat.tram$to[2:nrow(dat.tram)] # plug end of tramite -- multiline
        } else {
            dat.tram$to <- dmy("5/11/2014")                                    # -- uniline assumed pending
        }
        if (bills$info$status[i]=="statute"){
            dat.tram$to[nrow(dat.tram)] <- bills$info$dateOut[i] # use date published if so
        } else {
            dat.tram$to[nrow(dat.tram)] <- dmy("5/11/2014")      # else date when data downloaded (pending)
        }
        # minute(dat.tram$date) <- minute(dat.tram$date) +1 # adds 1 minute to remove overlap with last "to"
    }
    colnames(dat.tram)[1:2] <- c("from","tramite")
    if (nrow(bills$hitos[[i]])==1){
        dat.tram$to <- dmy("5/11/2014")
    }
    bills$tramites[[i]] <- dat.tram
    bills$tramites[[i]]$period <- interval(bills$tramites[[i]]$from, bills$tramites[[i]]$to) # adds trámite duration
}
rm(dat.tram, i, j, sel, these, after, selejec, tmp2)

## loop over hitos in search of urgencia info
#
## # used to prove that text "urgencia" misses no case of discusión inmediata
## tmp <- 0
## for (i in 1:I){
##     message(sprintf("loop %s of %s", i, I))
##     tmp1 <- grep(pattern = "[Uu]rgencia", bills$hitos[[i]]$action)
##     tmp2 <- grep(pattern = "[Dd]iscusión [Ii]nmediata", bills$hitos[[i]]$action)
##     if (length(tmp2)>0 & length(tmp2[!(tmp2 %in% tmp1)])>1){ # for records with DI, subsets line numbers with DI but no U
##         tmp <- append(tmp, i) # which record
##     }
## }

for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    tmp1 <- grep(pattern = "[Uu]rgencia", bills$hitos[[i]]$action)
    bills$hitos[[i]]$urg <- rep(".", nrow(bills$hitos[[i]]))
    if (length(tmp1)>0){ # for records with word urgencia
        tmp <- bills$hitos[[i]]$action[tmp1] # pick those lines
        # use greps to systematize urgencia action
        tmp <- sub(".*[Pp]asa.*\\(suma urgencia\\).*"                                              , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub(".*[Pp]asa.*\\(simple urgencia\\).*"                                            , replacement = "urg30 on", tmp, perl = TRUE)
        tmp <- sub(".*que retira y hace presente.*urgencia [Ss]imple.*"                            , replacement = "reset: urg30", tmp, perl = TRUE)
        tmp <- sub(".*que retira y hace presente.*urgencia [Ss]uma.*"                              , replacement = "reset: urg15", tmp, perl = TRUE)
        tmp <- sub(".*que retira y hace presente.*urgencia [Dd]iscusión inmediata.*"               , replacement = "reset: urg06", tmp, perl = TRUE)
        tmp <- sub("^Cuenta retiro y se hace presente.*urgencia [Ss]imple.*"                       , replacement = "reset: urg30", tmp, perl = TRUE)
        tmp <- sub(".*que( se)* retira.*urgencia.*"                                                , replacement = "urg off", tmp, perl = TRUE)
        tmp <- sub(".*que hace presente.*urgencia [Ss]imple.*"                                     , replacement = "urg30 on", tmp, perl = TRUE)
        tmp <- sub(".*que hace presente.*urgencia.*[Ss]uma.*"                                      , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub(".*que hace presente.*urgencia.*[Dd]iscusión [Ii]nmediata.*"                    , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*Rep. hace presente.*urgencia.*[Ss]imple.*"                                   , replacement = "urg30 on", tmp, perl = TRUE)
        tmp <- sub(".*Rep. hace presente.*urgencia.*[Ss]uma.*"                                     , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub(".*Rep. hace presente.*urgencia.*[Dd]iscusión [Ii]nmediata.*"                   , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*Hace presente.*urgencia.*[Dd]iscusión [Ii]nmediata.*"                        , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*Asimismo hace presente.*urgencia.*[Dd]iscusión [Ii]nmediata.*"               , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub("^Cuenta [Mm]ensaje.*[Hh]ace presente.*urgencia.*[Dd]iscusi[óo]n [Ii]nmediata.*", replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub("^Cuenta urgencia [Ss]imple.*"                                                  , replacement = "urg30 on", tmp, perl = TRUE)
        tmp <- sub(".*[Rr]et[iu][rt]a y.*hace presente.*urgencia.*[Ss]imple.*"                     , replacement = "reset: urg30", tmp, perl = TRUE)
        tmp <- sub(".*retira y hace presente.*urgencia.*[Ss]uma.*"                                 , replacement = "reset: urg15", tmp, perl = TRUE)
        tmp <- sub(".*[Rr]etira y hace presente.*urgencia.*[Dd]iscusión [Ii]nmediata.*"            , replacement = "reset: urg30", tmp, perl = TRUE)
        tmp <- sub(".*Corte Suprema.*urgencia.*"                                                   , replacement = "urg on @SC", tmp, perl = TRUE)
        tmp <- sub(".*Rep[.].*retir[oa].*urgencia.*"                                               , replacement = "urg off", tmp, perl = TRUE)
        tmp <- sub(".*República retira.*urgencia.*"                                                , replacement = "urg off", tmp, perl = TRUE)
        tmp <- sub(".*Ejecutivo.*retir[oa].*urgencia.*"                                            , replacement = "urg off", tmp, perl = TRUE)
        tmp <- sub("^Cuenta [Mm]ensaje.*retir[oa].*urgencia.*"                                     , replacement = "urg off", tmp, perl = TRUE)
        tmp <- sub("^Urgencia \\\\suma\\\\.*"                                                      , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub("Vicepresidente.*hace presente urgencia \\\\suma\\\\.*"                         , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub("^Urgencia \\\\discusión inmediata\\\\.*"                                       , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub("^Como la urgencia.*es de discusión inmediata.*"                                , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*informe.*mensaje con urgencia suma.*"                                        , replacement = "urg06 still on", tmp, perl = TRUE)
        tmp <- sub(".*informe.*con urgencia simple.*"                                              , replacement = "urg30 still on", tmp, perl = TRUE)
        tmp <- sub(".*proyecto. con urgencia suma.*"                                               , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub(".*proyecto.*tiene urgencia.*[Dd]iscusión [Ii]nmediata.*"                       , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*proyecto.*viene.*urgencia.*[Ss]uma.*"                                        , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub(".*proyecto.*tabla urgencia.*[Dd]iscusión [Ii]nmediata.*"                       , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*solicitar al Presidente.*el retiro de la urgencia.*"                         , replacement = "ask urg off", tmp, perl = TRUE)
        tmp <- sub(".*ha aprobado el proyecto con urgencia.*"                                      , replacement = "urg30 on", tmp, perl = TRUE)
        bills$hitos[[i]]$urg[tmp1] <- tmp
    }
}

## loop over hitos in search of veto info
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    tmp1 <- grep(pattern = "[Vv]eto", bills$hitos[[i]]$action) # try Art. 70 (bicam veto orr)
    tmp2 <- grep(pattern = "([Aa]rt.|artículo) 73", bills$hitos[[i]]$action)
#    tmp3 <- grep(pattern = "insistencia", bills$hitos[[i]]$action) # do this in other loop to find bicameral overrule with president's request art. 68
    tmp1 <- union(tmp1, tmp2) # remove repeated lines
    bills$hitos[[i]]$vet <- rep(".", nrow(bills$hitos[[i]]))
    if (length(tmp1)>0){ 
        tmp <- bills$hitos[[i]]$action[tmp1] # which record
        tmp <- sub(".*(Vicep|Presidente|P. *de la* Re*p*.).*([Cc]omunica|manifiesta).*no (hará|hacer) uso.*([Aa]rt[.]|artículo) 73.*", replacement = "no veto", tmp)
        tmp <- sub(".*(Ejecutivo|President.).*([Cc]omunica|informa|señala).*no (har[áa]|hacer) uso.*([Aa]rt[.]|artículo) 73.*", replacement = "no veto", tmp)
        tmp <- sub(".*[Pp]resident[ea].*[Cc]omunica.*no hará uso.*(Art[.]|artículo) 70.*", replacement = ".", tmp) #art70=bicamOverrule
        tmp <- sub(".*P(dte)*[.] de la Rep[.].*[Cc]omun[ic]{2}a.*no hará uso.*[Vv]eto.*" , replacement = "no veto", tmp)
        tmp <- sub(".*Oficio.*resultado.*veto.*"                                         , replacement = "tell override outcome", tmp)   
        tmp <- sub(".*Oficio rechazo veto.*comunica rechazo e insistencia.*"             , replacement = "tell veto overridden", tmp)
        tmp <- sub(".*Oficio rechazo veto a Cámara.*"                                    , replacement = "tell veto sustained", tmp)
        tmp <- sub(".*Oficio rechazo insistencia a Cámara.*"                             , replacement = "no bicam overrule?", tmp)
        tmp <- sub(".*Oficio aprobación insistencia a Cámara.*"                          , replacement = "bicam overrule?", tmp)
        tmp <- sub(".*[Oo]ficio a Cámara.*aprobando insistencia.*"                       , replacement = "tell veto overridden", tmp)
        tmp <- sub(".*Oficio.*Comunica.*proyecto a.*Presidenta.*efectos.*artículo 73.*"  , replacement = ".", tmp)
        tmp <- sub(".*Oficio.*[Cc]onsulta.*(Presidente|[Ee]jecutivo).*(ejerc[ie][cr](io)*|hará uso).*(veto|73).*", replacement = "ask if will veto", tmp)
        tmp <- sub(".*Oficio.*[Aa]l*.*(President[ea]|[Ee]jecutivo).*([Aa]rt.|artículo) 73.*" , replacement = ".", tmp)
        tmp <- sub(".*Oficio.*[Vv]eto [Pp]residencial.*"                                     , replacement = "veto", tmp)
        tmp <- sub(".*Oficio.*Observaciones del Ejecutivo.*"                                 , replacement = "veto", tmp)
        tmp <- sub(".*Presidente.*[Cc]omunica.*resuelto hacer uso.*([Aa]rt[.]|artículo) 73.*", replacement = "veto", tmp)
        tmp <- sub(".*Oficio.*[Rr]emite proyecto( aprobado)*.*ejercer.*facultad de veto.*"   , replacement = ".", tmp)
        tmp <- sub(".*Oficio de ley.*[Pp]ara.*ejerc[ie][cr](io)*.*facultad.*[Vv]eto.*"       , replacement = ".", tmp)
        tmp <- sub(".*Oficio de [Cc]onsulta.*facultad.*[Vv]eto.*"                            , replacement = ".", tmp)
        tmp <- sub(".*Oficio de ley.*([Cc]onsulta|saber).*facultad.*[Vv]eto.*"               , replacement = ".", tmp)
        tmp <- sub(".*Oficio aprobación veto a Cámara.*"                                     , replacement = "tell veto sustained", tmp)
        tmp <- sub(".*eximir el veto del trámite.*"                                          , replacement = "override disc", tmp)
        tmp <- sub(".*[Cc]omunica texto aprobado.*ejerc[ie][cr](io)*.*veto.*"                , replacement = ".", tmp)
        tmp <- sub(".*[Cc]onsulta.*(Presidente|Ejecutivo).*ejerc[ie][cr](io)*.*veto.*"       , replacement = ".", tmp)
        tmp <- sub(".*Informe.*sobre veto presidencial.*tabla"                               , replacement = "override disc", tmp)
        tmp <- sub(".*Discusión veto.*[Ss]e aprueban.*observaciones.*con excepci.n.*"        , replacement = "veto sustained in part", tmp)
        tmp <- sub(".*Discusión veto.*[Ss]e aprueban.*observaciones.*"                       , replacement = "veto sustained", tmp)
        tmp <- sub(".*Discusión veto.*[Ss]egunda discusión.*"                                , replacement = "override disc", tmp)
        tmp <- sub(".*Discusión veto.*Se aprueba.*y rechazan.*"                              , replacement = "veto sustained in part", tmp)
        tmp <- sub(".*Discusión veto.*[Rr]echaza[dn][o]*.*"                                  , replacement = "veto overridden", tmp)
        tmp <- sub(".*Discusión veto.*[Pp]endiente su discusión.*"                           , replacement = "override disc", tmp)
        tmp <- sub(".*Discusión veto.*[Aa]probado*.*"                                        , replacement = "veto sustained", tmp) # VERIFY, ACCEPTED
        tmp <- sub("^Discusión veto$"                                                        , replacement = "override disc", tmp)
        tmp <- sub(".*Cuenta veto presidencial.*"                                            , replacement = "veto", tmp)
        tmp <- sub(".*Cuenta [Oo]ficio rechazo [Vv]eto.*"                                    , replacement = "override disc", tmp)
        tmp <- sub(".*Cuenta( del)* [Oo]ficio.*([Pp]resident[ea]|Ejecutivo).*no (hará|hacer*) uso.*veto.*", replacement = "no veto", tmp)
        tmp <- sub(".*Cuenta [Oo]ficio aprobación veto.*aprobado parcialmente.*"             , replacement = "veto sustained in part", tmp)
        tmp <- sub(".*Cuenta [Oo]ficio aprobación veto.*manda comunicar.*[Pp]resident[ea].*" , replacement = "veto sustained", tmp)
        tmp <- sub(".*Cuenta [Oo]ficio aprobación veto.*remite.*al proyecto.*"               , replacement = "override disc", tmp)
        tmp <- sub(".*Cuenta [Oo]ficio aprobación veto.*[Pp]asa a.*[Cc]omisión.*"            , replacement = "override disc", tmp)
        tmp <- sub("^Cuenta [Oo]ficio aprobación veto$"                                      , replacement = "override disc", tmp);
        bills$hitos[[i]]$vet[tmp1] <- tmp;
    }
    # adds info to describe hitos
    bills$hitos[[i]]$bol <- rep(bills$info$bol[i], times = nrow(bills$hitos[[i]]));
    bills$hitos[[i]]$durgPest <- rep(bills$info$hasUrg[i], times = nrow(bills$hitos[[i]]));
    bills$hitos[[i]]$dvetPest <- rep(bills$info$hasVeto[i], times = nrow(bills$hitos[[i]]));
    ## next block seeks inconsistencies between urgencia tab and urgencia info in hitos (spots very few post 2006)
    if (length(grep(pattern = "[^.]", bills$hitos[[i]]$urg))>0 & bills$info$hasUrg[i]=="yes"){         # both report urgencia
        tmp <- 1
    } else if (length(grep(pattern = "[^.]", bills$hitos[[i]]$urg))==0 & bills$info$hasUrg[i]=="yes"){ # hito misses urgencia
        tmp <- 2
    } else if (length(grep(pattern = "[^.]", bills$hitos[[i]]$urg))>0 & bills$info$hasUrg[i]=="no"){   # uTab misses urgencia
        tmp <- 3
    } else {                                                                                           # none report urgencia
        tmp <- 4
    }
    bills$hitos[[i]]$debug <- rep(tmp, times = nrow(bills$hitos[[i]]));
    bills$info$debug[i] <- tmp
    bills$info$hasUrgHU[i] <- ifelse( length(grep(pattern = "[^.]", bills$hitos[[i]]$urg))==0 & bills$info$hasUrg[i]=="no", "no", "yes" )
}
rm(i, tmp, tmp1, tmp2)

# preliminary analysis
library(lubridate)
# compare urgencia report in Urgencias and in Hitos
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/1990") & bills$info$dateIn<dmy("1/3/1994")])
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/1994") & bills$info$dateIn<dmy("1/3/1998")])
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/1998") & bills$info$dateIn<dmy("1/3/2002")])
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/2002") & bills$info$dateIn<dmy("1/3/2006")])
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/2006") & bills$info$dateIn<dmy("1/3/2010")])
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/2010")])
table(bills$info$debug)
#
# crosstabs of urgencias and mensajes by yr
bills$info$legyr <- 0
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1990") & bills$info$dateIn<dmy("1/3/1991")] <- 1
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1991") & bills$info$dateIn<dmy("1/3/1992")] <- 2
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1992") & bills$info$dateIn<dmy("1/3/1993")] <- 3
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1993") & bills$info$dateIn<dmy("1/3/1994")] <- 4
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1994") & bills$info$dateIn<dmy("1/3/1995")] <- 5
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1995") & bills$info$dateIn<dmy("1/3/1996")] <- 6
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1996") & bills$info$dateIn<dmy("1/3/1997")] <- 7
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1997") & bills$info$dateIn<dmy("1/3/1998")] <- 8
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1998") & bills$info$dateIn<dmy("1/3/1999")] <- 9
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1999") & bills$info$dateIn<dmy("1/3/2000")] <- 10
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2000") & bills$info$dateIn<dmy("1/3/2001")] <- 11
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2001") & bills$info$dateIn<dmy("1/3/2002")] <- 12
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2002") & bills$info$dateIn<dmy("1/3/2003")] <- 13
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2003") & bills$info$dateIn<dmy("1/3/2004")] <- 14
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2004") & bills$info$dateIn<dmy("1/3/2005")] <- 15
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2005") & bills$info$dateIn<dmy("1/3/2006")] <- 16
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2006") & bills$info$dateIn<dmy("1/3/2007")] <- 17
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2007") & bills$info$dateIn<dmy("1/3/2008")] <- 18
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2008") & bills$info$dateIn<dmy("1/3/2009")] <- 19
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2009") & bills$info$dateIn<dmy("1/3/2010")] <- 20
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2010") & bills$info$dateIn<dmy("1/3/2011")] <- 21
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2011") & bills$info$dateIn<dmy("1/3/2012")] <- 22
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2012") & bills$info$dateIn<dmy("1/3/2013")] <- 23
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2013") & bills$info$dateIn<dmy("1/3/2014")] <- 24
#
for (i in 1:24){ # loop over legislative years
    sel <- which(bills$info$legyr==i); tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
    print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
}
tmp <- table(bills$info$dmensaje, bills$info$hasUrgH, useNA = "ifany") # whole period
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
#
sel <- which(bills$info$dateIn>=dmy("1/3/1990") & bills$info$dateIn<dmy("1/3/2006")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
#
# by legislature
sel <- which(bills$info$dateIn>=dmy("1/3/1990") & bills$info$dateIn<dmy("1/3/1994")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
sel <- which(bills$info$dateIn>=dmy("1/3/1994") & bills$info$dateIn<dmy("1/3/1998")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
sel <- which(bills$info$dateIn>=dmy("1/3/1998") & bills$info$dateIn<dmy("1/3/2002")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
sel <- which(bills$info$dateIn>=dmy("1/3/2002") & bills$info$dateIn<dmy("1/3/2006")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
sel <- which(bills$info$dateIn>=dmy("1/3/2006") & bills$info$dateIn<dmy("1/3/2010")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
sel <- which(bills$info$dateIn>=dmy("1/3/2006") & bills$info$dateIn<dmy("1/3/2014")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins

## # bind together hitos data.frames of different bills... export as csv to see it in excel
## library(plyr)
## sel <- which(bills$info$dateIn > dmy("28/2/2002"))
## ## sel <- runif(n=I); sel <- which(sel<.25) # process 25% randomly
## ## table(sel)
## tmp <- rbind.fill(bills$hitos[sel]) 
## head(tmp)
## #
## tmp2 <- tmp[, c("bol", "date", "tramite", "chamber", "urg", "vet", "durgPest", "dvetPest", "action", "debug")]
## tmp2$bolnum <- as.numeric(sub(pattern = "([0-9]+)-[0-9]+", replacement = "\\1", tmp2$bol))
## tmp2 <- tmp2[order(tmp2$bolnum),]
## tmp2$color <- 0; for (i in 2:nrow(tmp2)) tmp2$color[i] <- ifelse(tmp2$bolnum[i]==tmp2$bolnum[i-1], tmp2$color[i-1], 1 - tmp2$color[i-1])
## table(tmp2$color)
## write.csv(tmp2, file = "tmp.csv")

#########################
# systematize urgencias #
#########################
library(plyr)
library(lubridate)
bills$urgRaw <- bills$urgencias # preserve raw data
names(bills)[grep("urgencias", names(bills))] <- "urg" # shortens the object's name
bills$urg <- sapply(1:I, function(x) NULL) # initializes empty list with I elements (unnamed; names would go where 1:I)
#
work <- which(bills$info$hasUrg=="yes") # work only this in loop <-- OJO: there are 6 bills w urgencia in hitos only since 1998 (11 since 1990) GET THEM
#
# new slots for info # OJO: NEED TO ADJUST THESE
bills$info$nUrg <- 0
bills$info$nInChains <- 0
bills$info$nSimple <- 0
bills$info$nSuma <- 0
bills$info$nInmed <- 0
bills$info$nRet <- 0
#bills$info$nExtended <- 0
#bills$info$nShortened <- 0
#bills$info$nInDip <- 0
#bills$info$nInSen <- 0

# neat function to compute urgencia dealines excluding week-ends
# solution 1 takes holidays other than weekends into account, which is a plus
library(timeDate)
deadline <- function(x, nBizDys = 6){ # function to process deadlines excluding week-ends and holidays... how do you change default=holidayNYSE with non-prepackaged holidays (eg., Chile's http://www.feriadoschilenos.cl/)? NYSE: Jan1, MLKJan20, WashBdFeb17, GoodFdy, MemDyMay26, Jul4, LabDySep1, ThksNov2627, Dec25... Chile: Jan1, May1, DiscPdteCongMay21, Sep18-19, Dec25 (SemSta?)... check http://stackoverflow.com/questions/26777282/in-using-timedate-r-package-i-receive-an-error-when-specifying-gbnewyearseve
    output <- Reduce(rbind, Map((function(x, howMuch = 15){
        x <- as.Date(x)
        days <- x + 1:(howMuch*2)
        Deadline <- days[isBizday(as.timeDate(days))][howMuch]
        data.frame(DateIn = x, Deadline, DayOfWeek = weekdays(Deadline),   
                   TimeDiff = difftime(Deadline, x))  # useful to get more info, if so wished
    }), x, howMuch = nBizDys))
    output$Deadline
}
#deadline(date.in, nBizDys=30) # example of use
#
## # solution 2 removes weekends only, still needs to be turned into function
## library(chron)
## deadline <- function(x, nDays=31) {
##     x1 <-seq(as.Date(x)+1, length.out=nDays*2, by='1 day')
##     data.frame(Start=x,End=x1[!is.weekend(x1)][nDays])
## }
## do.call(rbind, lapply(date.in, deadline))

# redefine %within% to exclude upper bounds
"%my_within%" <- function(a,b) standardGeneric("%my_within%")
setGeneric("%my_within%")
#
setMethod("%my_within%", signature(b = "Interval"), function(a,b){
    if(!is.instant(a)) stop("Argument 1 is not a recognized date-time")
    a <- as.POSIXct(a)
    (as.numeric(a) - as.numeric(b@start) < b@.Data) & (as.numeric(a) - as.numeric(b@start) >= 0)
})
#
setMethod("%my_within%", signature(a = "Interval", b = "Interval"), function(a,b){
    a <- int_standardize(a)
    b <- int_standardize(b)
    start.in <- as.numeric(a@start) >= as.numeric(b@start) 
    end.in <- (as.numeric(a@start) + a@.Data) < (as.numeric(b@start) + b@.Data)
    start.in & end.in
})

# clean data from primary source
i <- which(bills$info$bol=="1484-01")
tmp <- bills$urgRaw[[i]]; tmp <- tmp[c(-2,-3,-5)]
bills$urgRaw[[i]] <- tmp
#
i <- which(bills$info$bol=="2296-18")
tmp <- bills$urgRaw[[i]]; tmp[3] <- "17 de Jul. de 2002   Simple 237-339  "; tmp <- tmp[-2]; 
bills$urgRaw[[i]] <- tmp
#
i <- which(bills$info$bol=="2347-15")
tmp <- bills$urgRaw[[i]]; tmp[11] <- "04 de Ago. de 1999 17 de Ago. de 1999 Suma 73-340 126-340"; tmp <- tmp[c(-2, -6:-9)]; 
bills$urgRaw[[i]] <- tmp
#
i <- which(bills$info$bol=="4639-11")
tmp <- bills$urgRaw[[i]]; tmp[2] <- "14 de Nov. de 2006 28 de Nov. de 2006 Suma 466-354 504-354"; tmp <- tmp[-4]; 
bills$urgRaw[[i]] <- tmp
#
i <- which(bills$info$bol=="2035-06")
tmp <- bills$urgRaw[[i]];
tmp[2] <- "02 de May. de 2001   Suma 0020501  "; tmp[4] <- "10 de Abr. de 2001 18 de Abr. de 2001 Suma 395-343 0020501"; 
bills$urgRaw[[i]] <- tmp
#
i <- which(bills$info$bol=="2121-04")
tmp <- bills$tramites[[i]]; tmp$to[2] <- dmy("20-01-1998"); tmp$period[2] <- interval(tmp$from[2], tmp$to[2]); tmp <- tmp[-3:-5,]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="114-06")
tmp <- bills$tramites[[i]]; tmp$to[2] <- tmp$to[3]; tmp$from[3] <- tmp$to[3]; tmp$to[3] <- tmp$to[4]; tmp$from[4] <- tmp$to[4]; tmp$tramite[3] <- "ejec"; tmp$tramite[4] <- "veto"; tmp$period <- interval(tmp$from, tmp$to) # ojo, infiero veto porque últ hito menciona ingreso de observaciones, sin más
#old tmp <- bills$tramites[[i]]; tmp$to[4] <- tmp$from[4] + days(1); tmp$period[4] <- interval(tmp$from[4], tmp$to[4]); tmp <- tmp[-5,]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="1902-17")
tmp <- bills$tramites[[i]]; tmp$to[2] <- dmy("21-12-2000"); tmp$from[3] <- dmy("21-12-2000"); tmp$tramite[3] <- "dip"; tmp <- tmp[-4,]; tmp$period <- interval(tmp$from, tmp$to)
#old tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4),]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2036-11")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[4,]); tmp$to[4] <- tmp$from[4] + days(7); tmp$from[5] <- tmp$to[4]; tmp$tramite[5] <- "veto"; tmp$period <- interval(tmp$from, tmp$to) # ojo, infiero veto porque últ hito menciona ingreso de observaciones, sin más
#old tmp <- bills$tramites[[i]]; tmp$to[4] <- tmp$from[4] + days(1); tmp$from[5] <- tmp$to[4]; tmp$to[5] <- tmp$from[5] + days(1); tmp$period[4] <- interval(tmp$from[4], tmp$to[4]); tmp$period[5] <- interval(tmp$from[5], tmp$to[5]); 
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2185-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[4,]); tmp$to[4] <- tmp$from[4] + days(7); tmp$from[5] <- tmp$to[4]; tmp$tramite[5] <- "veto"; tmp$period <- interval(tmp$from, tmp$to) # ojo, infiero veto porque últ hito menciona ingreso de observaciones, sin más
#old tmp <- bills$tramites[[i]]; tmp$to[4] <- tmp$from[4] + days(1); tmp$from[5] <- tmp$to[4]; tmp$to[5] <- tmp$from[5] + days(1); tmp$period[4] <- interval(tmp$from[4], tmp$to[4]); tmp$period[5] <- interval(tmp$from[5], tmp$to[5]); 
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2361-23")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-4,-6:-11,-14,-15),]; tmp$tramite[5] <- "ejec"; tmp$to[4] <- tmp$from[5]; tmp$to[5] <- tmp$to[5] + days(1); tmp$from[6] <- tmp$to[5]; tmp$from[7] <- tmp$to[6]; tmp$period <- interval(tmp$from, tmp$to)
#old tmp <- bills$tramites[[i]]; tmp <- tmp[c(-4,-6:-11),]  ## OJO: AQUÍ ME ESTOY COMIENDO UN RENGLON DE MAS... LO ARREGLO MAS ABAJO
bills$tramites[[i]] <- tmp
#
## i <- which(bills$info$bol=="2496-15")
## tmp <- bills$tramites[[i]]; tmp$to[4] <- tmp$from[4] + days(1); tmp$from[5] <- tmp$to[4]; tmp$period[4] <- interval(tmp$from[4], tmp$to[4]); tmp$period[5] <- interval(tmp$from[5], tmp$to[5]); 
## bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="737-03")
tmp <- bills$tramites[[i]]; tmp <- tmp[-4:-5,]; tmp$to[3] <- tmp$from[4]; tmp$period <- interval(tmp$from, tmp$to); 
#old tmp <- bills$tramites[[i]]; tmp$to[7] <- tmp$from[7] + days(1); tmp$from[8] <- tmp$to[7]; tmp$to[8] <- tmp$from[8] + days(1); tmp$period[7] <- interval(tmp$from[7], tmp$to[7]); tmp$period[8] <- interval(tmp$from[8], tmp$to[8]); 
bills$tramites[[i]] <- tmp
#
# fill missing trámites from urg by hand
i <- which(bills$info$bol=="561-06")
bills$urgRaw[[i]][2] <-  "18 de Dic. de 1991   Suma    " # decía 18 agosto 1992
#
i <- which(bills$info$bol=="679-05")
bills$urgRaw[[i]][2] <-  "06 de May. de 1992   Suma    " # decía 1991
#
i <- which(bills$info$bol=="1077-05")
bills$urgRaw[[i]][2] <-  "17 de Dic. de 1993   Simple    " # decía 1992
#
i <- which(bills$info$bol=="1135-04")
bills$urgRaw[[i]][3] <-   "22 de Mar. de 1994 05 de Abr. de 1994 Suma 220394 050494" # decía 1992
#
i <- which(bills$info$bol=="1240-11")
bills$urgRaw[[i]][2] <- "14 de Jul. de 1994 02 de Ago. de 1994 Suma 190794 20894"
bills$urgRaw[[i]][4] <- "05 de Jul. de 1994 19 de Jul. de 1994 Suma 050794 190794"
#
i <- which(bills$info$bol=="1444-15")
bills$urgRaw[[i]][2] <-   "22 de Nov. de 1994  Simple"
#
i <- which(bills$info$bol=="1575-10")
bills$urgRaw[[i]][2] <-   "02 de May. de 2001  Simple 428-343  "
#
i <- which(bills$info$bol=="1738-04")
bills$urgRaw[[i]][2] <-    "05 de Oct. de 2001   Suma 2-345  " # decía 5 mar 2002, pareciera que urgencia es sobre tribunal const
#
i <- which(bills$info$bol=="1906-04")
bills$urgRaw[[i]][2] <-     "09 de Ago. de 1997   Suma    " # decía 1995
#
i <- which(bills$info$bol=="2265-01")
bills$urgRaw[[i]][8] <-     "02 de Sep. de 1999   Suma   25-340  " # decía 1997
#
i <- which(bills$info$bol=="2361-23")
bills$urgRaw[[i]][2] <- "28 de Ago. de 2004 31 de Ago. de 2004 Suma 286-351 333-351"
#old bills$tramites[[i]]$from[5] <- dmy("19/10/2004")
#
i <- which(bills$info$bol=="2374-07")
bills$urgRaw[[i]][2] <- "04 de Nov. de 1999   Suma 110-342  "
#
i <- which(bills$info$bol=="2628-13")
bills$urgRaw[[i]][2] <- "04 de Ene. de 2002   Simple  170-345  "
#
i <- which(bills$info$bol=="2922-08")
bills$urgRaw[[i]][2] <-  "18 de Oct. de 2003 28 de Oct. de 2003 Suma 112-350 114-350"
bills$urgRaw[[i]][3] <- "15 de Ene. de 2004    Suma 365-350 "
#
i <- which(bills$info$bol=="3098-06")
bills$urgRaw[[i]][20] <- "08 de Jul. de 2003 05 de Ago. de 2003 Simple 101-346 157-349"
#
i <- which(bills$info$bol=="3190-04")
bills$urgRaw[[i]][4] <- "15 de Abr. de 2003   Simple 536-348  "
#
i <- which(bills$info$bol=="3406-03")
bills$urgRaw[[i]][3] <-  "04 de Nov. de 2003   Discusión inmediata 133-350  "
bills$tramites[[i]]$to[1] <- dmy("05/11/2003")
bills$tramites[[i]]$from[2] <- dmy("05/11/2003")
bills$tramites[[i]]$period <- interval(bills$tramites[[i]]$from, bills$tramites[[i]]$to)
#
i <- which(bills$info$bol=="3447-15")
bills$urgRaw[[i]] <- bills$urgRaw[[i]][-2]
#
i <- which(bills$info$bol=="3671-03")
bills$urgRaw[[i]][7] <- "21 de Jun. de 2005 02 de Ago. de 2005 Simple 58-353 142-353"
#
i <- which(bills$info$bol=="3885-07")
bills$urgRaw[[i]][10] <- "21 de Jun. de 2005 05 de Jul. de 2005 Discusión inmediata 57-353 89-353"
#
i <- which(bills$info$bol=="3899-05")
bills$urgRaw[[i]] <- gsub(pattern = "0005", replacement = "2005", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4040-06")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4545-11")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4742-13")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4813-06")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4814-13")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4900-27")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4937-18")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4970-04")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5076-15")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5080-11")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5081-15")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5083-04")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5091-15")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5114-10")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5120-21")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5148-05")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5173-05")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5308-18")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5431-11")
bills$urgRaw[[i]] <- gsub(pattern = "0008", replacement = "2008", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5687-23")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5763-05")
bills$urgRaw[[i]] <- gsub(pattern = "0008", replacement = "2008", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5898-07")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
## i <- which(bills$info$bol=="6110-24")
## bills$urgRaw[[i]] <- NULL # urgencias en nov 2014, post corte
## bills$urg[[i]] <- NULL
## bills$info$nUrg[i] <- bills$info$nSimple[i] <- 0
#
i <- which(bills$info$bol=="6189-06")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6231-05")
bills$urgRaw[[i]] <- gsub(pattern = "2009", replacement = "2008", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6252-09")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6443-07")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; bills$tramites[[i]] <- tmp
##old bills$tramites[[i]]$to[4] <- dmy("04/06/2009")
##old bills$tramites[[i]]$from[5] <- dmy("04/06/2009")
##old bills$tramites[[i]]$to[5] <- dmy("12/06/2009")
bills$urgRaw[[i]] <- gsub(pattern = "Ago.", replacement = "Jun.", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6562-07")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6582-11")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6586-15")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6628-06")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6639-25")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6691-07")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6726-06")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6739-02")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6758-15")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6759-10")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6791-06")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="7141-08")
bills$urgRaw[[i]] <- gsub(pattern = "1211", replacement = "2011", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="7203-02")
bills$urgRaw[[i]] <- gsub(pattern = "Ago.", replacement = "Sep.", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="7854-07")
bills$urgRaw[[i]] <- gsub(pattern = "1211", replacement = "2011", bills$urgRaw[[i]])

# manipulate tramites to remove fake tercer trámite when revisora made no changes to bill
tramVerif <- rep(0, I) # alternative would be adding +1 after each change (and include cases that do not exhaust problem below)
tmp1 <- rep(0, I) # will receive dummy sin modificaciones pointing to índices that need manipulation
tmp2 <- rep(0, I) # will receive length tramites
for (i in 1:I){ 
    if (length(grep("Oficio aprobaci[óo]n sin modificaciones a.*de [Oo]rigen", bills$hitos[[i]]$action))>0) tmp1[i] <- 1
    tmp2[i] <- nrow(bills$tramites[[i]])
}
#
sel <- c(1:I)[tmp1==1 & tmp2==3] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==3
for (i in sel){
    if (bills$tramites[[i]]$tramite[3]=="ejec"){
        tmp1[i] <- 0 # if third tramite is executive, needs no manipulation
        tramVerif[i] <- 1
    }
}
sel <- c(1:I)[tmp1==1 & tmp2==3] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==3 that all need work
for (i in sel){
    bills$tramites[[i]]$tramite[3] <- "ejec" # change third entry to executive
    tmp1[i] <- 0
    tramVerif[i] <- 1
}
sel <- c(1:I)[tmp1==1 & tmp2==4] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==4 that all need work
for (i in sel){
    if (bills$tramites[[i]]$tramite[4]=="ejec"){ # if fourth trámite is ejec
        bills$tramites[[i]] <- bills$tramites[[i]][-3,] # then drop third trámite
        bills$tramites[[i]]$to[2] <- bills$tramites[[i]]$from[3] # and arrange period in case needed
        tmp1[i] <- 0 # remove from manipulation dummy
        tramVerif[i] <- 1
    }
}
#
sel <- c(1:I)[tmp1==1 & tmp2==4] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==4 that all need work
# change by hand
i <- which(bills$info$bol=="3120-10")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- tmp$from[3]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="339-10")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- tmp$from[3]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="476-07")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- tmp$from[3]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="6690-10")
tmp <- bills$tramites[[i]]; tmp$from[4] <- dmy("03-03-2011"); tmp$tramite[3] <- "trib"; tmp$tramite[4] <- "ejec"; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0
tramVerif[sel] <- 1
#
sel <- c(1:I)[tmp1==1 & tmp2==2] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==2 that all need work
# change by hand
i <- which(bills$info$bol=="140-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,], tmp[2,]); tmp$to[1] <- dmy("04-12-1990"); tmp$from[2] <- tmp$to[1]; tmp$to[2] <- dmy("05-03-1991"); tmp$from[3] <- tmp$to[2]; tmp$to[3] <- tmp$from[3] + days(1); tmp$from[4] <- tmp$to[3]; tmp$tramite[2] <- "dip"; tmp$tramite[4] <- "veto"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="147-13")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("24-04-1991"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="1711-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-05-1996"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="218-05")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("20-12-1990"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="257-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("17-04-1991"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="258-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("05-11-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="291-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("10-04-1991"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="292-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-09-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="321-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-09-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="322-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("02-07-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="323-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-01-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="337-07")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("12-08-1991"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="347-13")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("16-05-1991"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="3517-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("19-10-2004"); tmp$from[3] <- tmp$to[2]; tmp$tramite[2] <- "sen"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="356-04")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("14-06-1994"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="366-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("08-10-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="367-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("28-01-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="377-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("04-12-1991"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="379-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("12-09-1991"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="387-04")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-04-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="388-07")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("15-12-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="403-07")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-04-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="417-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("24-07-1991"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="440-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-04-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="657-02")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("28-05-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="664-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("18-08-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="665-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("18-08-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="666-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("18-08-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="671-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("17-06-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="681-13")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("20-05-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="697-13")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("16-09-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="700-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("16-06-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="716-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("15-09-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="728-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("09-09-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="729-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("15-06-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="744-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-06-1994"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="753-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-02-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="762-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("15-12-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="768-04")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-04-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="769-13")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("09-11-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="770-05")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("25-08-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="777-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("24-03-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="778-07")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("21-12-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="803-01")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-12-1994"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="822-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-12-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="848-02")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("11-07-1994"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="852-05")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("19-11-1992"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="865-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-07-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="866-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("15-06-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="879-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-07-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="880-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("28-04-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="906-05")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-04-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="931-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("24-05-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="932-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-07-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="933-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("07-09-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="941-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("23-11-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="951-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("07-04-1993"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="959-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("07-09-1995"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0 # remove indices
tramVerif[sel] <- 1
#
sel <- c(1:I)[tmp1==1 & tmp2==5] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==5
#
for (i in sel){
    if (bills$tramites[[i]]$tramite[4]=="ejec"){ # fourth entry is exec
        tmp <- bills$tramites[[i]]
        tmp <- tmp[-3,] # drop third row
        tmp$to[2] <- tmp$from[3] # fix dates
        bills$tramites[[i]] <- tmp
        tmp1[i] <- 0 # remove from indices
        tramVerif[i] <- 1
    }
}
sel <- c(1:I)[tmp1==1 & tmp2==5] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==5
#
for (i in sel){
    if (bills$tramites[[i]]$tramite[5]=="ejec"){ # fifth entry is exec
        tmp <- bills$tramites[[i]]
        tmp <- tmp[-3:-4,] # drop third and fourth rows
        tmp$to[2] <- tmp$from[3] # fix dates
        bills$tramites[[i]] <- tmp
        tmp1[i] <- 0 # remove from indices
        tramVerif[i] <- 1
    }
}
#
sel <- c(1:I)[tmp1==1 & tmp2==5] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==5
#
# change by hand
i <- which(bills$info$bol=="113-11")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3:-4,]; tmp$to[2] <- tmp$from[3]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="5500-10")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3:-4,]; tmp$to[2] <- tmp$from[3]; tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "ejec"; tmp$to[3] <- dmy("06-10-2009"); tmp$from[4] <- tmp$to[3]
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0
tramVerif[sel] <- 1
#
sel <- c(1:I)[tmp1==1 & tmp2==6] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==1 that all need work
for (i in sel){
    if (bills$tramites[[i]]$tramite[6]=="ejec"){ # sixth entry is exec
        tmp <- bills$tramites[[i]]
        tmp <- tmp[-3:-5,] # drop third row
        tmp$to[2] <- tmp$from[3] # fix dates
        bills$tramites[[i]] <- tmp
        tmp1[i] <- 0 # remove from indices
        tramVerif[i] <- 1
    }
}
#
sel <- c(1:I)[tmp1==1 & tmp2==6] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==1 that all need work
#
# change by hand
i <- which(bills$info$bol=="2689-06")
tmp <- bills$tramites[[i]]; tmp$to[4] <- dmy("05-06-2001"); tmp <- tmp[c(-3,-5,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2718-02")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2814-06")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="3178-07")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4),]; tmp$to[2] <- tmp$from[3]; tmp$to[4] <- dmy("04-03-2003")
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="3729-13")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="4639-11")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="5099-07")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="5326-06")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="6349-06")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="6560-10")
tmp <- bills$tramites[[i]]; tmp$to[5] <- tmp$to[6]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="8387-05")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0
tramVerif[sel] <- 1
#
sel <- c(1:I)[tmp1==1 & tmp2==7] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==1 that all need work
# change by hand
i <- which(bills$info$bol=="2465-06")
tmp <- bills$tramites[[i]]; tmp$to[6] <- tmp$to[7]; tmp <- tmp[c(-3,-5,-7),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2677-06")
tmp <- bills$tramites[[i]]; tmp$to[6] <- tmp$to[7]; tmp <- tmp[c(-3,-5,-7),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="3595-05")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-5,-6),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="4313-06")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4,-5),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="6733-06")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4,-5),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="8696-04")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4,-5),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0
tramVerif[sel] <- 1
#
sel <- c(1:I)[tmp1==1 & tmp2>7] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==1 that all need work
# change by hand
i <- which(bills$info$bol=="1577-10")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3:-7,]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2520-07")
tmp <- bills$tramites[[i]]; tmp$to[7] <- tmp$to[8]; tmp <- tmp[c(-3,-5,-6,-8),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="3265-07")
tmp <- bills$tramites[[i]]; tmp$to[8] <- tmp$to[9]; tmp <- tmp[c(-3,-4,-5,-7,-9),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="4047-10")
tmp <- bills$tramites[[i]]; tmp$to[8] <- tmp$to[9]; tmp <- tmp[c(-4,-5,-6,-7,-9),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0
tramVerif[sel] <- 1
#
# did single-tramite pass (they should not, else info missing)
tmp2 <- rep(0, I) # will receive length tramites revised
for (i in 1:I){ 
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp2==1 & bills$info$status=="statute")
# change by hand
i <- which(bills$info$bol=="133-05")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("29-08-1990"); tmp$to[2] <- tmp$from[3] <- dmy("30-08-1990"); tmp$to[3] <- dmy("04-09-1990"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; 
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1381-05") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("04-11-1994"); tmp$to[2] <- tmp$from[3] <- dmy("04-12-1994"); tmp$to[3] <- dmy("06-12-1994"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="397-05") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("04-08-1991"); tmp$to[2] <- tmp$from[3] <- dmy("04-09-1991"); tmp$to[3] <- dmy("10-09-1991"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="471-10") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("04-10-1991"); tmp$to[2] <- tmp$from[3] <- dmy("04-11-1991"); tmp$to[3] <- dmy("29-09-1991"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="642-10") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("26-03-1994"); tmp$to[2] <- tmp$from[3] <- dmy("26-03-1995"); tmp$to[3] <- dmy("26-03-1997"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="981-13") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("13-05-1993"); tmp$to[2] <- tmp$from[3] <- dmy("23-05-1993"); tmp$to[3] <- dmy("31-05-1997"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
sel <- which(tmp2==1 & bills$info$status=="pending: 3er trámite")
i <- which(bills$info$bol=="7486-01")
bills$info$status[i] <- "killed/withdrawn"
i <- which(bills$info$bol=="9036-07")
bills$info$status[i] <- "killed/withdrawn"
tramVerif[sel] <- 1
#
sel <- which(tmp2==1 & bills$info$status=="pending: 2do trámite")
for (i in sel){
    tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp); tmp$tramite[2] <- ifelse(tmp$tramite[1]=="dip", "sen", "dip")
    tmp$to[1] <- tmp$from[2] <- bills$hitos[[i]]$date[nrow(bills$hitos[[i]])] # take last date from hitos
    bills$tramites[[i]] <- tmp
}
tramVerif[sel] <- 1
#
tmp2 <- rep(0, I) # will receive length tramites revised
for (i in 1:I){ 
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp2==1)
table(bills$info$status[sel])
tramVerif[sel] <- 1 # huge leap of faith? assume all coded frozen/killed/withdrawn/pending with single trámite are correct...
#
sel <- which(tmp2==2 & bills$info$status=="statute") # two tramites that are statute
#
# drop tramite==dip or sen after third tramite  (does not exhaust problems in tmp1==1 cases, so will not code as tramVerif)
for (i in 1:I){
    check <- grep("dip|sen", bills$tramites[[i]]$tramite)
    drop <- check[check>3]
    if (length(drop)>0){
        tmp <- bills$tramites[[i]]
        tmp <- tmp[-drop,]
        bills$tramites[[i]] <- tmp
    }
}
#
# drop repeated conf
tmp1 <- rep(0, I) # will receive dummy conf repeated
for (i in 1:I){ 
    if (length(grep("conf", bills$tramites[[i]]$tramite))>1) tmp1[i] <- 1 # conf appears more than once
}
#
sel <- which(tmp1==1)
# drop repeated instances of conf (does not exhaust problems in tmp1==1 cases, so will not add tramVerif)
for (i in sel){
    tmp <- bills$tramites[[i]]
    drop <- which(tmp$tramite=="conf")
    tmp$to[min(drop)] <- tmp$to[max(drop)] # plug last date's to first
    drop <- min(drop):max(drop) # select everything in between
    drop <- drop[-1] # drop everything sandwiched between repeated conf (2nd inclusive)
    tmp <- tmp[-drop,]
    bills$tramites[[i]] <- tmp
}
#
# miscoded case
i <- which(bills$info$bol=="334-07") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp$tramite[3] <- "dip"; tmp <- rbind(tmp, tmp[4,], tmp[4,]); tmp$tramite[5] <- "ejec"; tmp$tramite[6] <- "veto"
tmp$to[4] <- tmp$from[5] <- dmy("21-07-1992"); tmp$to[5] <- tmp$from[6] <- dmy("08-10-1992"); 
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
# cases missing return to iniciadora when project was modified by revisora
tmp1 <- rep(0,I)
tmp2 <- rep(0,I)
for (i in 1:I){
    if (length(grep(".*[Oo]ficio.*aprobación.*proyecto.*con modificaciones.*", bills$hitos[[i]]$action))>0) tmp1[i] <- 1
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp1==1 & tmp2==2) # when two tramites only, one must be missing
for (i in sel){
    tmp <- bills$tramites[[i]]
    tmp <- rbind(tmp, tmp[1,])
    tmp$to[3] <- tmp$to[2] # take last date
    tmp$to[2] <- tmp$from[3] <- bills$hitos[[i]]$date[nrow(bills$hitos[[i]])] # take last date from hitos
}
tramVerif[sel] <- 1
#
sel <- which(tmp1==1 & tmp2>2) # when 3 or more tramites, third must be same as first
for (i in sel){
    i <- sel[2] # debug
    tmp <- bills$tramites[[i]]
#    if (tmp$tramite[3]==tmp$tramite[1]) next
    if (tmp$tramite[3]=="conf" | tmp$tramite[3]=="veto"){
        tmp <- rbind(tmp[1:2,], tmp[1,], tmp[3:tmp2[i],]) # adds row for missing return to iniciadora
        tmp$from[3] <- tmp$to[3] <- tmp$to[2] # picks date from last recorded trámite
    }
}
#
# if veto, must have exec trámite before
tmp1 <- rep(0,I)
tmp2 <- rep(0,I)
for (i in 1:I){
    if (length(grep("veto", bills$tramites[[i]]$tramite))>0 & length(grep("ejec", bills$tramites[[i]]$tramite))==0) tmp1[i] <- 1 # no ejec despite veto
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp1==1 & bills$info$status=="statute")
for (i in sel){
    tmp <- bills$tramites[[i]]
    n <- grep("veto", tmp$tramite)
    tmp <- rbind(tmp[1:(n-1),], tmp[n,], tmp[n:tmp2[i],])
    tmp$tramite[n] <- "ejec"
    tmp$from[n] <- tmp$to[n-1]
    tmp$to[n] <- tmp$from[n+1]
    bills$tramites[[i]] <- tmp
}
# clean
i <- which(bills$info$bol=="1034-15")
tmp <- bills$tramites[[i]]
tmp <- tmp[-3,]
bills$tramites[[i]] <- tmp
#
# if statute, must have exec trámite
tmp1 <- rep(0,I)
tmp2 <- rep(0,I)
for (i in 1:I){
    if (length(grep("ejec", bills$tramites[[i]]$tramite))==0) tmp1[i] <- 1 # no ejec
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp1==1 & bills$info$status=="statute")
for (i in sel){
    tmp <- bills$tramites[[i]]
    tmp <- rbind(tmp, tmp[tmp2[i],])
    tmp$tramite[tmp2[i]+1] <- "ejec";
    tmp$from[tmp2[i]+1] <- tmp$to[tmp2[i]+1] <- bills$hitos[[i]]$date[nrow(bills$hitos[[i]])] # take last date from hitos
    bills$tramites[[i]] <- tmp
}
tramVerif[sel] <- 1
#
# tribunal repeated, trámites dropped
tmp1 <- rep(0,I)
for (i in 1:I){
    if (length(grep("trib", bills$tramites[[i]]$tramite))>1) tmp1[i] <- 1 
}
sel <- which(tmp1==1)
for (i in sel){
#    i <- sel[3]; bills$info$bol[i] # debug
    tmp <- bills$tramites[[i]]
    drop <- grep("trib", tmp$tramite)
    drop <- drop[-1] # drops repeated instances of tribunal
    tmp <- tmp[-drop,]
    bills$tramites[[i]] <- tmp
}
#
# two tramites that are not statute taken as ok if trámites are dip and sen only
tmp2 <- rep(0,I)
for (i in 1:I){
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp2==2 & bills$info$status!="statute")
table(tramVerif[sel]) # most still marked as not revised
#
tmp1 <- rep(0,I)
for (i in sel){
    tmp <- bills$tramites[[i]]
    if (length(grep("conf|ejec|veto|trib", tmp$tramite))>0) tmp1[i] <- 1 # should only have dip and sen
}
table(tmp1[sel]) # all ok
tramVerif[sel] <- 1
#
# by hand
i <- which(bills$info$bol=="171-02") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$tramite[2] <- "dip"; tmp$to[1] <- tmp$from[2] <- dmy("31-10-1990"); tmp$to[2] <- tmp$from[3] <- dmy("05-12-1990")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="825-03") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp); tmp$tramite[2] <- "dip"; tmp$to[1] <- tmp$from[2] <- dmy("09-06-1993"); tmp$to[2] <- tmp$from[3] <- dmy("04-08-1993"); tmp$to[3] <- tmp$from[4] <- dmy("17-08-1993")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="2247-05") 
tmp <- bills$tramites[[i]]; tmp$tramite[2] <- "sen"; tmp$to[1] <- tmp$from[2] <- dmy("18-11-1998"); tmp$to[2] <- tmp$from[3] <- dmy("19-11-1998"); tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="5058-07") 
tmp <- bills$tramites[[i]]; tmp$to[1] <- tmp$to[3]; tmp <- tmp[1,]
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="674-14") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp[1,], tmp); tmp$tramite[2] <- "dip"; tmp$tramite[5] <- "trib"; tmp$tramite[6] <- "ejec"; tmp$to[1] <- tmp$from[2] <- dmy("22-03-1993"); tmp$to[2] <- tmp$from[3] <- dmy("04-10-1995"); tmp$to[4] <- tmp$from[5] <- dmy("13-06-1996")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="2093-05") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp[1,], tmp); tmp$tramite[2] <- "sen"; tmp$to[1] <- tmp$from[2] <- tmp$to[2] <- tmp$from[3] <- dmy("18-11-1997"); tmp$to[3] <- tmp$from[4] <- tmp$to[4] <- tmp$from[5] <- dmy("25-11-1997")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="3993-05") 
tmp <- bills$tramites[[i]]; tmp$tramite[2] <- "sen"; 
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="603-13") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp[1,], tmp); tmp$tramite[2] <- "dip"; tmp$to[1] <- tmp$from[2] <- dmy("07-09-1992");  tmp$to[2] <- tmp$from[3] <- dmy("20-03-1993");
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="7308-06") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[5,]); tmp$tramite[5] <- "ejec"; tmp$from[5] <- tmp$to[4]; tmp$to[5] <- tmp$from[6] <- dmy("18-12-2012")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1035-07") 
tmp <- bills$tramites[[i]]; tmp$tramite[5] <- "ejec"; tmp$tramite[6] <- "veto"; tmp$to[1] <- tmp$from[2] <- dmy("12-09-1995"); tmp$to[4] <- tmp$from[5] <- dmy("04-07-2000"); tmp$to[5] <- tmp$from[6] <- dmy("16-08-2000")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="111-06")
# didn't fix it, will be dropped in analysis
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1251-18") 
tmp <- bills$tramites[[i]]; tmp$tramite[3] <- "dip"; tmp$tramite[4] <- "ejec"; tmp$tramite[5] <- "veto"; tmp$tramite[6] <- "trib"; tmp$to[3] <- tmp$from[4] <- dmy("04-04-2000"); tmp$to[4] <- tmp$from[5] <- dmy("04-05-2000"); tmp$to[5] <- tmp$from[6] <- dmy("21-06-2000"); tmp$to[6] <- dmy("05-08-2000")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="259-07")
# didn't fix it, will be dropped in analysis
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="446-03")
# didn't fix it, will be dropped in analysis
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="5724-26") 
tmp <- bills$tramites[[i]]; tmp$tramite[4] <- "ejec"; tmp$tramite[5] <- "veto"; tmp$tramite[6] <- "trib";  tmp$from[4] <- tmp$to[3] <- dmy("04-11-2009"); tmp$to[4] <- tmp$from[5] <- dmy("10-11-2009"); tmp$to[5] <- tmp$from[6] <- dmy("01-12-2009"); 
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="628-11")
# didn't fix it, will be dropped in analysis
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="111-06") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp); tmp[2,] <- tmp[3,]; tmp$tramite[3] <- "dip"; tmp$from[3] <- tmp$to[2] <- dmy("09-11-1993")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1502-02") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp); tmp[2,] <- tmp[3,]; tmp$tramite[3] <- "dip"; tmp$to[2] <- tmp$from[3] <- dmy("17-11-1999"); tmp$from[4] <- tmp$to[3] <- dmy("04-01-2000")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1516-02") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp); tmp[2,] <- tmp[3,]; tmp$tramite[3] <- "dip"; tmp$to[2] <- tmp$from[3] <- dmy("17-11-1999"); tmp$from[4] <- tmp$to[3] <- dmy("04-01-2000")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1867-06") 
tmp <- bills$tramites[[i]]; tmp[4,] <- tmp[3,]; tmp$tramite[3] <- "sen"; tmp$to[3] <- tmp$from[4] <- dmy("04-08-1999"); tmp$to[4] <- dmy("04-11-2014")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="195-08") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp); tmp[2,] <- tmp[3,]; tmp$tramite[3] <- "dip"; tmp$to[2] <- tmp$from[3] <- dmy("05-12-1990")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
# ************** BLOCK MOVED BEGINS *******************
## CONTINUE REVISING TRÁMITES
# fix tramite date by hand
i <- which(bills$info$bol=="6041-08")
bills$tramites[[i]]$to[3] <- bills$tramites[[i]]$from[4]; bills$tramites[[i]]$period <- interval(bills$tramites[[i]]$from, bills$tramites[[i]]$to)
#
#tmp1 <- rep(0, I) # will receive dummy sin modificaciones pointing to índices that need manipulation
tmp2 <- rep(0, I) # will receive length tramites
for (i in 1:I){ 
#    if (length(grep("Oficio aprobaci[óo]n sin modificaciones a.*de [Oo]rigen", bills$hitos[[i]]$action))>0) tmp1[i] <- 1
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp2==1 & tramVerif==0) # pick single-trámite, non-revised
table(bills$info$dpassed[sel]) ## none have passed... infer that single trámite is right
tramVerif[sel] <- 1
#
sel <- which(tmp2==2 & tramVerif==0) # pick two-trámite, non-revised
table(bills$info$dpassed[sel]) # none have passed
tramVerif[sel] <- 1
# all should have dip and sen  only
tmp1 <- rep(0, I) 
for (i in 1:I){ 
    if (length(grep("dip|sen", bills$tramites[[i]]$tramite))!=2) tmp1[i] <- 1 # all should have dip or sen, nothing else
}
sel <- which(tmp2==2 & tramVerif==0 & tmp1==1)
# checked by hand: have redundant second trámite that needs to be dropped
for (i in sel){
    tmp <- bills$tramites[[i]]
    tmp <- tmp[1,]
    bills$tramites[[i]] <- tmp
}
#
# change by hand
i <- which(bills$info$bol=="356-04")
bills$info$status[i] <- "statute"
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1282-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- interval(tmp$from, tmp$to); # assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1285-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- interval(tmp$from, tmp$to); # assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1778-07")
tmp <- bills$tramites[[i]]
tmp$to[1] <- tmp$to[3]; tmp <- tmp[1,]
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1854-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- interval(tmp$from, tmp$to); # assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="2293-10")
tmp <- bills$tramites[[i]]
tmp <- tmp[-3,]; 
bills$tramites[[i]] <- tmp
bills$info$status[i] <- "killed/withdrawn"
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="3119-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- interval(tmp$from, tmp$to); # assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="5115-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- interval(tmp$from, tmp$to); # assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="6649-10")
bills$info$status[i] <- "statute"
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="7160-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- interval(tmp$from, tmp$to); # assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
sel <- which(tmp2==3 & tramVerif==0 & bills$info$dpassed==0) # pick three-trámite, non-revised, that didn't pass; revised by hand, all ok
tramVerif[sel] <- 1
#
sel <- which(tmp2==3 & tramVerif==0 & bills$info$dpassed==1) # pick three-trámite, non-revised, that passed
tmp1 <- rep(1, I) 
for (i in sel){ 
    if (bills$tramites[[i]]$tramite[3]=="ejec") tmp1[i] <- 0 # all should have ejec as third trámite
}
length(which(tmp2==3 & tramVerif==0 & bills$info$dpassed==1 & tmp1==1))==0 # all have ejec as third, assume all ok
tramVerif[sel] <- 1
#
tmp2 <- rep(0, I) # will receive length tramites
for (i in 1:I){ 
#    if (length(grep("Oficio aprobaci[óo]n sin modificaciones a.*de [Oo]rigen", bills$hitos[[i]]$action))>0) tmp1[i] <- 1
    tmp2[i] <- nrow(bills$tramites[[i]])
}
#
sel <- which(tmp2==4 & tramVerif==0) # pick four-trámite, non-revised
# checked by hand, all seem ok
tramVerif[sel] <- 1
#
sel <- which(tmp2==5 & tramVerif==0) # pick five-trámite, non-revised
# checked by hand, all seem ok
tramVerif[sel] <- 1
#
sel <- which(tmp2==6 & tramVerif==0) # pick six-trámite, non-revised
# checked by hand, all seem ok
tramVerif[sel] <- 1
#
sel <- which(tmp2==7 & tramVerif==0) # pick seven-trámite, non-revised
# checked by hand, all seem ok
tramVerif[sel] <- 1
# *************** BLOCK MOVED ENDS ******************
#
# check conf in 3rd trám to verify if not missing a trámite in origen (looking for "rechazo ideal de legislar" and for "oficio rechazo c. origen" would achieve clean revision)
tmp1 <- rep(0,I)
for (i in 1:I){
    tmp <- bills$tramites[[i]]
    if (length(grep("conf", tmp$tramite))>0){
        tmp1[i] <- grep("conf", tmp$tramite)
    }
}
sel <- which(tmp1==3)
tramVerif[i] <- 1 # checked most by hand, they look ok
#
# check by hand
i <- which(bills$info$bol=="1200-15")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- bills$hitos[[i]]$date[nrow(bills$hitos[[i]])]
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1291-15")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- bills$hitos[[i]]$date[nrow(bills$hitos[[i]])]
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1707-18")
tmp <- bills$tramites[[i]]; tmp$to[1] <- tmp$from[2] <- tmp$to[3]; tmp$to[2] <- dmy("11-11-2014"); tmp$tramite[2] <- "sen"; tmp <- tmp[-3,]
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="5697-29")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- dmy("19-01-2010")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="6417-07")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- dmy("31-07-2009")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="6979-06")
tmp <- bills$tramites[[i]]; tmp$to[2] <- tmp$to[3]; tmp <- tmp[-3,]; 
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="8149-09")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- dmy("01-10-2013")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="8612-02")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- dmy("16-10-2013")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="8618-11")
tmp <- bills$tramites[[i]]; tmp$to[3] <- dmy("11-11-2014")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="922-15")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- dmy("11-11-2014")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="995-15")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- dmy("11-11-2014")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
# FIND EMPTY STRINGS IN TRAMITE --- WOULD BE PREFERABLE TO DO THIS WHEN EXTRACTING FROM HITOS
tmp1 <- rep(0,I)
for (i in 1:I){
    tmp <- which(bills$tramites[[i]]$tramite=="")
    if (length(tmp)>0) tmp1[i] <- tmp
}
sel <- which(tmp1>0)
for (i in sel){
    #i <- sel[2] # debug
    if (tmp1[i]==1){
        tmp <- bills$tramites[[i]]
        tmp <- tmp[-tmp1[i],]
        bills$tramites[[i]] <- tmp
    } else {
        tmp <- bills$tramites[[i]]
        tmp$to[(tmp1[i]-1)] <- tmp$to[tmp1[i]]
        tmp <- tmp[-tmp1[i],]
        bills$tramites[[i]] <- tmp
    }
}
#
# checked all other tramVerif==0, seem ok
#table(tramVerif) # ALL REVISED
#
rm(check, drop, n, i, sel, tmp, tmp1, tmp2, tramVerif, vet) # clean

# RE DO ALL PERIODS (NEED TO REVISE FROM AND TO...)
for (i in 1:I){
    #i <- which(bills$info$bol=="372-15") # debug
    message(sprintf("loop %s of %s", i, I))
    tmp <- bills$tramites[[i]]
    tmp$period <- interval(tmp$from, tmp$to)
    bills$tramites[[i]] <- tmp
}
# add tramite number to the object created above
for (i in 1:I){
    #i <- 1 # debug
    N <- nrow(bills$tramites[[i]])
    bills$tramites[[i]]$nTr <- 1:N # OJO: WILL BE RE-DONE BELOW; RETAINED HERE TO AVOID BREAKS IN CODE 
}
rm(N)
#
## change nTr to 1=origen, 2=revisora, 3=origen.bis, 4=conf, 5=ejec, 6=veto, 7=trib; change urgIn accordingly
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    tmp <- bills$tramites[[i]]
    skip <- grep(pattern = "conf*|ejec|veto|trib", tmp$tramite)
    if (length(skip)>0){
        tmp1 <- tmp[-skip,]
    } else {
        tmp1 <- tmp
    }
    tmp1$nTr <- 1:nrow(tmp1)
    if (length(skip)>0){
        tmp[-skip,] <- tmp1
    } else {
        tmp <- tmp1
    }
    tmp$nTr[tmp$tramite=="conf"] <- 4
    tmp$nTr[tmp$tramite=="ejec"] <- 5
    tmp$nTr[tmp$tramite=="veto"] <- 6
    tmp$nTr[tmp$tramite=="trib"] <- 7
    bills$tramites[[i]] <- tmp
}
#
options(warn=2) # turns warnings into errors, which break the loop (use warn=1 to return to normal) 
for (i in work){
    #j <- 1
    #j <- j + 1 # debug
    #i <- work[j] # debug
    message(sprintf("processing record %s", i))
    #bills$info$bol[i] # debug
    tmp <- bills$urgRaw[[i]]
    tmp <- tmp[-grep(pattern = "Fecha Inicio", tmp)] # remove header assuming it contains Fecha Inicio and may not be there
    U <- length(tmp)
    tmp <- gsub(pattern = "(de [0-9]+) ", replacement = "\\1,", tmp) # separates date(s) with a comma
    tmp2 <- nchar( gsub(pattern = "[^,]", replacement = "", tmp) )  # how many dates (commas) in each line
                                        # prepares dates
    tmp <- gsub(pattern = " de ", replacement = "/", tmp)
    tmp <- gsub(pattern = "Ene.", replacement = "1", x = tmp)
    tmp <- gsub(pattern = "Feb.", replacement = "2", x = tmp)
    tmp <- gsub(pattern = "Mar.", replacement = "3", x = tmp)
    tmp <- gsub(pattern = "Abr.", replacement = "4", x = tmp)
    tmp <- gsub(pattern = "May.", replacement = "5", x = tmp)
    tmp <- gsub(pattern = "Jun.", replacement = "6", x = tmp)
    tmp <- gsub(pattern = "Jul.", replacement = "7", x = tmp)
    tmp <- gsub(pattern = "Ago.", replacement = "8", x = tmp)
    tmp <- gsub(pattern = "Sep.", replacement = "9", x = tmp)
    tmp <- gsub(pattern = "Oct.", replacement = "10", x = tmp)
    tmp <- gsub(pattern = "Nov.", replacement = "11", x = tmp)
    tmp <- gsub(pattern = "Dic.", replacement = "12", x = tmp)
                                        #
    tmp <- gsub(pattern = ",[ ]+", replacement = ",", tmp) # drops spaces after commas
                                        #
    output <- data.frame(type=character(U)) # initialize output object
    output$on <- dmy(gsub(pattern = "^([0-9/]*),.*", "\\1", tmp, perl = TRUE)) # adds date urgencia introduced
                                        #
    tmp3 <- sub(pattern = ".*(Sin urgencia).*", replacement = "\\1", tmp, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Simple).*", replacement = "\\1", tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Suma).*", replacement = "\\1", tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Discusión inmediata).*", replacement = "\\1", tmp3, perl = TRUE)
    output$type <- tmp3
## # use something like this to determine if urgencia happened while bill was in conf...
## bills$tramites[[i]]$period <- interval(bills$tramites[[i]]$from, bills$tramites[[i]]$to) # <- put this in tramites loop
## sel <- which( bills$tramites[[i]]$tramite == "conf" )
## if (length(sel)>0){
##     output$on %my_within% bills$tramites[[i]]$period
## #    output$on[sel] %my_within% bills$tramites[[i]]$period
## }
         #
         # when urgencia deadline is de jure (need to change if the bill is in Comisión mixta) --- check if Senado and Comisión mixta urgencias are included
    tmp3 <- sub(pattern = ".*(Sin urgencia).*", replacement = 0, tmp, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Simple).*", replacement = 30, tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Suma).*", replacement = 15, tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Discusión inmediata).*", replacement = 6, tmp3, perl = TRUE)
    tmp3 <- as.numeric(tmp3)
    select <- which(output$on < dmy("3/7/2010")) # change urgencias before constitutional reform
    tmp3[select] <- mapvalues(tmp3[select], from = c(6,15), to = c(3,10), warn_missing = FALSE)
                                        #
    output$deadline <- output$on # inherits format for NAs
    select <- which(tmp2!=0)
    if (length(select)>0){
        output$deadline[tmp3!=0] <- deadline(output$on[tmp3!=0], as.numeric(tmp3[tmp3!=0]))
    }
                                        #
                                        # urgencia retired?
    output$dcaduca <- 0; output$dcaduca[grep("CADUCA", tmp)] <- 1 # urgencias "caducadas" were not retired (eg 1035-07)
    output$dretir <- 0
    select <- which(tmp2==2)
    if (length(select)>0){
        output$dretir[tmp2==2 & output$dcaduca==0] <- 1
    }
                                        # when urgencia was removed, if at all
    output$off <- output$deadline
    select <- which(tmp2==2)
    if (length(select)>0){
        output$off[tmp2==2] <- dmy(gsub(pattern = ".*[0-9],([0-9/]*),.*", "\\1", tmp[tmp2==2], perl = TRUE))
    }
    ##
    ##
    
                                        # ADD ON AND OFF MESSAGE NUMS (NAME TO FIND THIS: on/offMessageNumbers) 
    #i <- which(bills$info$bol=="372-15") # debug
    obj1 <- bills$urgRaw[[i]]
    obj2 <- gsub(pattern = ".* ([0-9]+[-][0-9]+)[ ]([0-9]+[-][0-9]+)?", replacement = "\\1", obj1[-1]) # on message number
    obj3 <- gsub(pattern = ".* ([0-9]+[-][0-9]+)[ ]([0-9]+[-][0-9]+)?", replacement = "\\2", obj1[-1]) # retiro message number
                                        #remove spaces from obj2 obj3
    obj2 <- gsub(pattern = " ", replacement = "", obj2)
    obj3 <- gsub(pattern = " ", replacement = "", obj3)
                                        #plug to output    
    output$msgOn <- obj2
    output$msgOff <- obj3
    
                                        # put NAs in off for urgencias not retired
    select <- which(tmp2==1)
    if (length(select)>0){
        output$off[tmp2==1] <- NA
    }
    
                                        # drops instances of no urgencia, if any
    select <- which(output$type=="Sin urgencia")
    if (length(select)>0) {
        output <- output[-select,]
    }
                                        #
                                        # sort chrono
    output <- output[order(output$on),]
                                        #
                                        # find and consolidate chains
    output$chain <- 0
    U <- nrow(output) # number of messages left
    output$tramite <- "." # prepare to receive trámite
    output$trNum <- 0 # prepare to receive trámite number
    for(u in 1:U){  # plug trámite to output; Not sure what it does when 2+ trámites in same day (eg. 2121-04)
#        u <- u+1 # debug
        sel <- output$on[u] %my_within% bills$tramites[[i]]$period # in which period does date.on belong in?
#        sel # debug
        if (length(bills$tramites[[i]]$tramite[sel])==0){
            output$tramite[u] <- "check: no overlap"
        } else {
            output$tramite[u] <- bills$tramites[[i]]$tramite[sel]
            output$trNum[u] <- bills$tramites[[i]]$nTr[sel]
        }
    }
    if (U > 1){
        for (k in 2:U){
            if (output$dretir[k-1]==1 & output$on[k]==output$off[k-1]){
                output$chain[k] <- 1
            }
        }
        tmp4 <- output$chain
        for (k in 2:U){
            tmp4[k] <- output$chain[k] + tmp4[k-1] * output$chain[k] # zero if no chain, else how many links
        }
        output$chain <- tmp4; rm(tmp4)
        # verify that chain indeed happened in same trámite and not in next
        if (U>1){
            for (u in 2:U){
                if (output$tramite[u]!=output$tramite[u-1]){ # spot and recode false chains (not in same trámite)
                    output$chain[u] <- 0
                }
            }
        }
    }
    output$change <- 0 # by default, no change in deadline
    ## # bloc useful if urgencia chains were consolidated
    ## output$nshorten <- output$nextend <- 0
    ## output$newDeadline <- output$deadline; # by default it is the same, with nil change
    ## output$nlinks <- output$chain + 1 # will receive info if 
    if (U > 1){
        for (u in U:2){ ## reverse loop over urgencia messages (so that first link of multi-chain inherits downstream info)
            if (output$chain[u]!=0){                              # choose urgencies in chains
                output$dretir[u-1] <- 0                           # recode: upstream message was not retired
                output$change[u] <- as.numeric(output$deadline[u] - output$deadline[u-1])*100 / as.numeric(output$deadline[u-1] - output$on[u-1]) # % change new deadline -- OJO: necesita más info para ser preciso: quitar fechas en que el Congreso no estuvo en sesión, quitar días festivos del conteo de días etc.
                ## # bloc useful if chains were to be consolidated
                ## output$dnlinks[u-1] <- output$nlinks[u]           # plug nlinks upstream
                ## output$newDeadline[u-1] <- output$deadline[u]     # it just got a new deadline
                ## output$newDeadline[u-1] <- output$deadline[u]     # they just got a new deadline
                ## if (output$deadline[u] >= output$deadline[u-1]){
                ##     output$nextend[u-1] <- 1                      # either longer
                ## } else {
                ##     output$shorten[u-1] <- 1                      # or shorter
                ## }
                ## output$newDeadline[u] <- NA
                ## output$change[u-1] <- as.numeric(output$newDeadline[u-1] - output$off[u-1]) *100 / as.numeric(output$deadline[u-1] - output$on[u-1])
                ## output$off[u-1] <- NA
            }
        }
    }
    ## ##                                     # drop chains after consolidating info
    ## ## select <- which(output$chain==1)
    ## ## if (length(select)>0) {
    ## ##     output <- output[-select,]
    ## ##}
    ## ## output$chain <- NULL
    #
    bills$urg[[i]] <- output # plug systematized object back into database
                                        #
                                        # plug into slot for systematized data
    if (nrow(output)>0){ # anything left after dropping sin urgencia?
        bills$info$nUrg[i] <- U
        bills$info$nInChains[i] <- nrow(output[output$chain!=0,])
        bills$info$nSimple[i] <- nrow(output[output$type=="Simple",])
        bills$info$nSuma[i] <- nrow(output[output$type=="Suma",])
        bills$info$nInmed[i] <- nrow(output[output$type=="Discusión inmediata",])
        bills$info$nRet[i] <- nrow(output[output$dretir==1,])
    }
    if (nrow(output)==0){ # in case nothing left after dropping sin urgencia, change info
        bills$info$hasUrg[i] <- "no"
    }
}
#output # debug
#message(sprintf("i=%s bol=%s", i, bills$info$bol[i]))
options(warn=1)

# fill wrong trámites from urg by hand (single-day trámite missed by loop above)
i <- which(bills$info$bol=="279-03")
bills$urg[[i]]$tramite[1] <- "sen"; bills$urg[[i]]$trNum[1] <- 1
bills$urg[[i]]$tramite[2] <- "dip"; bills$urg[[i]]$trNum[2] <- 2
#
#old i <- which(bills$info$bol=="2361-23")
#old bills$urg[[i]]$tramite[25] <- "sen"; bills$urg[[i]]$trNum[25] <- 4
#
i <- which(bills$info$bol=="3190-04")
bills$urgRaw[[i]][4] <- "15 de Abr. de 2003   Simple 536-348  "
#
rm(i, k, output, sel, select, tmp, tmp1, tmp2, tmp3, u, U, work) # housecleaning
#
# find urgencias in trámites ejec, veto, trib to move trNum them to max legislative stage (bills$urg[[i]]$tramite left untouched, though)
tmp1 <- rep(0,I)
sel <- which(bills$info$nUrg>0)
for (i in sel){ # find records with trNum 5 6 7
    # i <- which(bills$info$bol=="3882-04") # debug
    tmp <- bills$urg[[i]]$trNum
    if (max(tmp)>4) tmp1[i] <- 1
}
sel <- which(tmp1==1) # select them for manipulation
for (i in sel){
    tmp <- bills$tramites[[i]]$nTr
    tmp <- max(tmp[tmp<5]) # max trámite reached other than ejec, veto, trib
    bills$urg[[i]]$trNum[bills$urg[[i]]$trNum>4] <- tmp
}

# WHICH TRÁMITE(S) RECEIVED AT LEAST ONE URGENCY: 1, 2, 3, 4, 12, 13, 14, 23, 24, 34, 123, 124, 134, 234, 1234 (0 if none)
bills$info$urgIn <- 0 # prepares column to receive which trámites had 1+ urgencies
sel <- which(bills$info$nUrg>0)
for (i in sel){
    tmp <- bills$urg[[i]]$trNum # trámite numbers with an urgency
    tmp[tmp>4] <- 4 # recode trNum as 1,2,3,4+ 
    tmp <- as.numeric(names(table(tmp))) # remove repeated numbers
    tmp <- paste(tmp, sep="", collapse = "")
    bills$info$urgIn[i] <- as.numeric(tmp)
}
table(bills$info$urgIn)
rm(sel,tmp,i)
# BILL PASSED DUMMY
bills$info$dpassed <- 0
bills$info$dpassed[bills$info$status=="statute"] <- 1

# re-arrange columns in data.frame, dropping useless ones
bills$info <- bills$info[, c("bol", "legyr", "dateIn", "init", "dmensaje", "dpassed", "status", "dateOut", "urgIn", "nUrg", "nInChains", "nSimple", "nSuma", "nInmed", "nRet", "refundido", "leg", "state", "materia", "hasHitos", "hasReport", "hasUrg", "hasSpon", "hasVot", "hasVeto", "hasUrgHU", "nHitos")] 

# SORT BILLS AND OBJECTS (SEE MY http://stackoverflow.com/questions/27303945/sort-nested-lists-in-r)
tmp <- as.numeric( sub(pattern = "([0-9]+)-.*", replacement = "\\1", bills$info$bol) ); ord <- order(tmp)  # order: boletín w/o committee
bills <- lapply(bills, function(x, ord) {
      if (is.data.frame(x)) return(x[ord,])
      return(x[ord])
    },
    ord = ord
)
rm(tmp, ord, obj1, obj2, obj3)

# add titulo from csv file
tmp <- read.csv(file = paste(datdir, "raw/proyec3.csv", sep = ""), stringsAsFactors = FALSE)
tmp <- tmp[,c("bl", "titulo")]; colnames(tmp)[1] <- "bol" # keep titulos and bol only
tmp2 <- merge(x = bills$info, y = tmp, by = "bol", all = FALSE)
tmp <- as.numeric(sub(pattern = "([0-9]+)-[0-9]+", replacement = "\\1", tmp2$bol))
tmp2 <- tmp2[order(tmp),]
table(tmp2$bol==bills$info$bol)
bills$info <- tmp2

## Add objects with session dates 1990-2014
library(lubridate)
ses <- read.csv(file = paste(datdir, "raw/sesionesCamara.csv", sep = ""), stringsAsFactors = FALSE)
colnames(ses) <- c("legislatura","date","session","stat")
ses$txt <- ses$session # keep text
ses$session <- sub(pattern = "Sesión ([0-9].*) en .*", replacement = "\\1", ses$txt)
ses$session <- sub(pattern = "Sesión en ([Cc]ongreso pleno) en .*", replacement = "\\1", ses$session)
#
tmp <- ses$date
tmp <- gsub(pattern = " de ", replacement = "-", x = tmp)
tmp <- gsub(pattern = "Ene."     , replacement = "1", x = tmp)
tmp <- gsub(pattern = "Feb."   , replacement = "2", x = tmp)
tmp <- gsub(pattern = "Mar."     , replacement = "3", x = tmp)
tmp <- gsub(pattern = "Abr."     , replacement = "4", x = tmp)
tmp <- gsub(pattern = "May."      , replacement = "5", x = tmp)
tmp <- gsub(pattern = "Jun."     , replacement = "6", x = tmp)
tmp <- gsub(pattern = "Jul."     , replacement = "7", x = tmp)
tmp <- gsub(pattern = "Ago."    , replacement = "8", x = tmp)
tmp <- gsub(pattern = "Sep.", replacement = "9", x = tmp)
tmp <- gsub(pattern = "Oct."   , replacement = "10", x = tmp)
tmp <- gsub(pattern = "Nov." , replacement = "11", x = tmp)
tmp <- gsub(pattern = "Dic." , replacement = "12", x = tmp)
ses$date <- dmy(tmp)
## # compare date in string to date column: ALL OK
## tmp <- ses$txt
## tmp <- sub(pattern = "Sesión [0-9].* en (.*)", replacement = "\\1", tmp)       # drop start
## tmp <- sub(pattern = "Sesión en [cC]ongreso pleno en (.*)", replacement = "\\1", tmp)       # drop start
## tmp <- sub(pattern = "(.*) de [0-9]+:.*:[0-9]+ hrs.", replacement = "\\1", tmp) # drop hours
## tmp <- sub(pattern = "(.*) a las.*hrs.", replacement = "\\1", tmp)              # drop hours
## tmp <- gsub(pattern = " de ", replacement = "-", x = tmp)
## tmp <- sub(pattern = ".* ([0-9]+[-].*)", replacement = "\\1", x = tmp)
## tmp <- gsub(pattern = "enero"     , replacement = "1", x = tmp)
## tmp <- gsub(pattern = "febrero"   , replacement = "2", x = tmp)
## tmp <- gsub(pattern = "marzo"     , replacement = "3", x = tmp)
## tmp <- gsub(pattern = "abril"     , replacement = "4", x = tmp)
## tmp <- gsub(pattern = "mayo"      , replacement = "5", x = tmp)
## tmp <- gsub(pattern = "junio"     , replacement = "6", x = tmp)
## tmp <- gsub(pattern = "julio"     , replacement = "7", x = tmp)
## tmp <- gsub(pattern = "agosto"    , replacement = "8", x = tmp)
## tmp <- gsub(pattern = "septiembre", replacement = "9", x = tmp)
## tmp <- gsub(pattern = "octubre"   , replacement = "10", x = tmp)
## tmp <- gsub(pattern = "noviembre" , replacement = "11", x = tmp)
## tmp <- gsub(pattern = "diciembre" , replacement = "12", x = tmp)
## tmp <- dmy(tmp)
## table(tmp == ses$date)
#
# sort and keep dates only --- if something else needed it can be added from ses here
ses <- ses[order(ses$date, ses$session),]; ses$ddip <- 1 # prepare new data.frame with date (sen sessions will be added here)
tmp <- ses[,c("date","ddip")];
tmp <- tmp[duplicated(tmp)==FALSE, ] # drop repeated dates (days with 2nd+ session)
bills$sessions <- tmp
rm(ses, tmp, tmp2)
#
# add senado sessions
ses <- read.csv(file = paste(datdir, "raw/sesionesSenado.csv", sep = ""), stringsAsFactors = FALSE)
colnames(ses) <- c("legislatura","sesion","tipo","fch")
tmp <- ses$fch
tmp <- sub(pattern = "(.*[12][90][901][0-9]).*", replacement = "\\1", tmp) # drop trailing spaces
tmp <- gsub(pattern = " de ", replacement = "-", x = tmp)
tmp <- sub(pattern = "[A-Za-záé]+\\W([0-9]{1,2}[-].*)", replacement = "\\1", tmp) # drop weekdays
tmp <- gsub(pattern = "Enero"     , replacement = "1", x = tmp)
tmp <- gsub(pattern = "Febrero"   , replacement = "2", x = tmp)
tmp <- gsub(pattern = "Marzo"     , replacement = "3", x = tmp)
tmp <- gsub(pattern = "Abril"     , replacement = "4", x = tmp)
tmp <- gsub(pattern = "Mayo"      , replacement = "5", x = tmp)
tmp <- gsub(pattern = "Junio"     , replacement = "6", x = tmp)
tmp <- gsub(pattern = "Julio"     , replacement = "7", x = tmp)
tmp <- gsub(pattern = "Agosto"    , replacement = "8", x = tmp)
tmp <- gsub(pattern = "Septiembre", replacement = "9", x = tmp)
tmp <- gsub(pattern = "Octubre"   , replacement = "10", x = tmp)
tmp <- gsub(pattern = "Noviembre" , replacement = "11", x = tmp)
tmp <- gsub(pattern = "Diciembre" , replacement = "12", x = tmp)
tmp <- dmy(tmp)
ses$fch <- tmp # needed to recover cong pleno below
#
tmp <- tmp[order(tmp)]
tmp <- tmp[duplicated(tmp)==FALSE]
tmp <- data.frame(date = tmp, dsen = rep(1, length(tmp)))
#
tmp2 <- merge(x = bills$sessions, y = tmp, by = "date", all = TRUE); tmp2[is.na(tmp2)==TRUE] <- 0 # merge senado and diputado sessions
tmp2 <- tmp2[order(tmp2$date),]
#
bills$sessions <- tmp2 # paste object with both chambers' sessions
#
# recover Congreso pleno dates that are missing in dip data
sel <- which(ses$tipo=="Congreso pleno") # some of these sessions do not appear in diputados data, will add them
bills$sessions$ddip[which(bills$sessions$date %in% ses$fch[sel])] <- 1
#
rm(tmp, tmp2)
#
head(bills$sessions)
rm(ses, sel)

## LOAD VALERIA'S DATA INCLUDING BILL IMPORTANCE
library(dplyr)
library(lubridate)
vp5 <- read.csv(file = "raw/Base revisada.csv", stringsAsFactors = FALSE)
vp5$Fechaingreso <- mdy(vp5$Fechaingreso)
table(year(vp5$Fechaingreso), vp5$Importancia, useNA = "always")
# keep importancia only
vp5$importancia <- vp5$Importancia # rename
#vp5
vp5 <- vp5[,c("nboletin","importancia")]
#
# merge Vale's importancia here <<< FOR UNKNOWN REASONS, bills$info changes after merging importancia in... will keep importancia apart in same order...
tmp <- bills$info
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = vp5, by.x = "bol", by.y = "nboletin", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden),]
importancia.vale <- tmp[, c("bol","importancia")]
importancia.vale <- as.tibble(importancia.vale)

# materia según boletín
bills$info$materiaBol <- as.numeric(gsub(pattern = "[0-9]+[-]([0-9]+)", replacement = "\\1", bills$info$bol))

# codifica materia
bills$info$materiaCat <- NA
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Derechos Humanos|derecho de las personas|Gobierno de facto|Justicia militar|militares|libertad de expresión|retorno|rehabilitación de la nacionalidad|amnist[ií]a|ciudadanos|fuerzas armadas|armada|marina de chile|exilio|exilado|tropas|tortura|conscript", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "militares/DD.HH."
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("exporta|importa|zona franca|aduanas|arancel|turismo|comercio|tlc|tle chile-", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Comercio"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Acuicultura|Pesca|Forestal|Agr[ií]c|ambient|desalinización|cooperativa|cultivo|residuos", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Agricultura/medio ambiente"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("cobre|miner|de minas", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Cobre"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Educación|Capacitación|Universidad|Subvención|ciencia|tecnología|investigación|enseñan|ex[áa]men|acreditación|universidad|univesitari", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Educación"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Kinesio|Salud|medicamento|sanitario", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Salud"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Convenio|antártico|Acuerdo|otros países|tratado", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Convenios intl"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Estadio|Deporte|hípica", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Deporte"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("industria", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Industria"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("trabajo|laboral|trabajador|empleador|Retiro Voluntario|Colegios Profesionales|aguinaldo|salario|sindica|pensi[oó]n|bono|ingreso", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Trabajo pensiones"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Consumidor|clientes|nutricional|bienes raíces", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Consumidor"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Vivienda|construcci", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Vivienda"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Crédito|Inversi[oó]n|pago electrónico|medios de pago|financier|seguro", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Sector financiero"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Bienes del estado|personal de|empresas del estado|dirección pública", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Sector público"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Monumentos", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Symbolic"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Patrimonio cultural", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Cultura"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Penitenciaria|penal|policía|Infracciones|drogas|criminal|delito|terror|penas|indulto|carabineros|fuerzas del* orden|mercenario|armas", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Law and order"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Generador|El[ée]ctri|petr[óo]le", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Energía"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("Elecci[oó]n|electora|Congreso|presidencia|consejeros|concejales|diputados|senadores|ministerio|municip|administración pública|poder judicial|feriado judicial|partidos|votaci[oó]|toma de razón|quórum", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Gov branches/elections"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("carretera|autopista|aeropuerto|trenes|cruceros|barcos|aéreo|aviación|aeronáuti|navegación|puertos|transporte|televisi[óo]n|radio|microondas|internet", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Comunicación y transportes"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("nacionalización", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Private bill"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("recaudación|tributar|impuesto|presupuesto|fiscal", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Recaudación"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("migraci[oó]n|masiva de diarios|cédula|chileatiende|zonas aisladas|estadística", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Interior"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("mujer|familia|niñ[oa]|infantil|adopción|maternidad", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "Género"
bills$info[sel.na,] <- tmp # return to data
#
sel.na <- which(is.na(bills$info$materiaCat)); tmp <- bills$info[sel.na,] # ignore already coded rows
sel <- grep("rr[.]ee|visa|diplomátic", tmp$materia, ignore.case = TRUE)
tmp$materiaCat[sel] <- "RR.EE."
bills$info[sel.na,] <- tmp # return to data
#
# unfinished... reasonable overlap with bolCat
sel <- which(is.na(bills$info$materiaCat)==TRUE & bills$info$dmensaje==1)
table(is.na(bills$info$materiaCat[bills$info$dmensaje==1]))
tail(bills$info$materia[sel])
#
table(bills$info$materiaBol[bills$info$dmensaje==1], is.na(bills$info$materiaCat[bills$info$dmensaje==1]))
#
tmp$materiaBol <- NULL # recoded more finely below

# imports Memo Rosas's roll call summaries 2002-2014
tmp01 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2002-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp02 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2003-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp03 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2004-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp04 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2005-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp05 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2006-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp06 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2007-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp07 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2008-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp08 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2009-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp09 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2010-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp10 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2011-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp11 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2012-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp12 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2013-Votes.csv", sep = ""), stringsAsFactors=FALSE)
tmp13 <- read.csv(file = paste(datdir, "raw/memoRollCallSum/ChileDep2014-Votes.csv", sep = ""), stringsAsFactors=FALSE) # had weird encoding, semi-solved it 
tmp <- rbind(tmp01, tmp02, tmp03, tmp04, tmp05, tmp06, tmp07, tmp08, tmp09, tmp10, tmp11, tmp12, tmp13); rm(tmp01, tmp02, tmp03, tmp04, tmp05, tmp06, tmp07, tmp08, tmp09, tmp10, tmp11, tmp12, tmp13)
#
# drop non-boletín votes ... drops procedural votes
sel <- grep(pattern = "Boletín", tmp$Detail)
tmp <- tmp[sel,]
#
# clean boletines
tmp$bol <- tmp$Detail; tmp$Detail <- NULL
tmp$bol <- sub(pattern = "Boletín N°(.*)", replacement = "\\1", tmp$bol)
#
# duplicate votes of joint boletines
sel <- grep(";|,| y ", tmp$bol)
tmp1 <- tmp[sel,]; tmp <- tmp[-sel,] # crop duplicate votes for manipulation
tmp1$bol <- sub("y otros", replacement = "", tmp1$bol) # will not figure which these are
tmp1$bol <- gsub(" y", replacement = ";", tmp1$bol)
tmp1$bol <- gsub(",", replacement = ";", tmp1$bol)
sel <- grep(";|,", tmp1$bol)
tmp <- rbind(tmp, tmp1[-sel,]) # return cases not figured out to data
tmp1 <- tmp1[sel,]
#
tmp2 <- tmp1 # duplicate
tmp2$bol <- sub(pattern = "^(.*); (.*)$", replacement = "\\2", tmp2$bol) # take last boletin
tmp1$bol <- sub(pattern = "^(.*); (.*)$", replacement = "\\1", tmp1$bol) # leave rest for further manipulation
sel <- grep(";", tmp1$bol) # indices with multiboletines
tmp2 <- rbind(tmp2, tmp1[-sel,]) # paste rest in object
tmp <- rbind(tmp, tmp2)          # and return to data
tmp1 <- tmp1[sel,]
#
tmp2 <- tmp1 # duplicate again
tmp2$bol <- sub(pattern = "^(.*); (.*)$", replacement = "\\2", tmp2$bol) # take last boletin
tmp1$bol <- sub(pattern = "^(.*); (.*)$", replacement = "\\1", tmp1$bol) # leave rest for further manipulation
sel <- grep(";", tmp1$bol) # indices with multiboletines
tmp2 <- rbind(tmp2, tmp1[-sel,]) # paste rest in object
tmp <- rbind(tmp, tmp2)          # and return to data
tmp1 <- tmp1[sel,]
#
tmp2 <- tmp1 # duplicate again
tmp2$bol <- sub(pattern = "^(.*); (.*)$", replacement = "\\2", tmp2$bol) # take last boletin
tmp1$bol <- sub(pattern = "^(.*); (.*)$", replacement = "\\1", tmp1$bol) # leave rest for further manipulation
sel <- grep(";", tmp1$bol) # indices with multiboletines
tmp2 <- rbind(tmp2, tmp1[-sel,]) # paste rest in object
tmp <- rbind(tmp, tmp2)          # and return to data
tmp1 <- tmp1[sel,]
#
tmp2 <- tmp1 # duplicate again
tmp2$bol <- sub(pattern = "^(.*); (.*)$", replacement = "\\2", tmp2$bol) # take last boletin
tmp1$bol <- sub(pattern = "^(.*); (.*)$", replacement = "\\1", tmp1$bol) # leave rest for further manipulation
sel <- grep(";", tmp1$bol) # indices with multiboletines
tmp2 <- rbind(tmp2, tmp1[-sel,]) # paste rest in object
tmp <- rbind(tmp, tmp2)          # and return to data
tmp1 <- tmp1[sel,]
#
tmp2 <- tmp1 # duplicate again
tmp2$bol <- sub(pattern = "^(.*); (.*)$", replacement = "\\2", tmp2$bol) # take last boletin
tmp1$bol <- sub(pattern = "^(.*); (.*)$", replacement = "\\1", tmp1$bol) # leave rest for further manipulation
sel <- grep(";", tmp1$bol) # indices with multiboletines
tmp2 <- rbind(tmp2, tmp1[-sel,]) # paste rest in object
tmp <- rbind(tmp, tmp2)          # and return to data
tmp1 <- tmp1[sel,]
#
nrow(tmp1) ## VERIFY, MUST BE ZERO
#
# continue processing data
tmp$bol <- gsub(pattern = " ", replacement = "", tmp$bol) # drop empty spaces
tmp$bol <- gsub(pattern = "\\(S\\)", replacement = "", tmp$bol) # drop extras in boletín
sel <- grep("P.A", tmp$bol); tmp <- tmp[-sel,] # se cuelan aún algunos proyectos de acuerdo
tmp <- tmp[-which(tmp$bol==""),] # drop cases missing boletín number
# cases with incomplete boletín
sel <- which(tmp$bol=="3342"); tmp$bol[sel] <- "3342-06"
sel <- which(tmp$bol=="4977"); tmp$bol[sel] <- "4977-08"
sel <- which(tmp$bol=="5426"); tmp$bol[sel] <- "5426-03"
sel <- which(tmp$bol=="7972"); tmp$bol[sel] <- "7972-05"
sel <- which(tmp$bol=="7874"); tmp$bol[sel] <- "7874-04"
sel <- which(tmp$bol=="8467"); tmp$bol[sel] <- "8467-12"
sel <- which(tmp$bol=="9294"); tmp$bol[sel] <- "9294-06"
sel <- which(tmp$bol=="6452"); tmp$bol[sel] <- "6452-10"
sel <- which(tmp$bol=="6450"); tmp$bol[sel] <- "6450-10"
sel <- which(tmp$bol=="112");  tmp$bol[sel] <- "7972-05"
#
library(lubridate)
sel <- grep(pattern = "[0-9]+/[0-9]+/20", tmp$Date) # cases with four-digit years
tmp$date <- mdy(tmp$Date) # compute all dates
tmp$date[-sel] <- mdy(tmp$Date[-sel]) # fix two-digit ones
tmp$Date <- NULL
#
# sort chrono
tmp <- tmp[order(tmp$date, tmp$bol),]
tmp$Observation <- NULL
#
# q = minimum number of aye votes to pass (called "quorum")
tmp$q <- 0
tmp$q[grep("Constitucional 3/5", tmp$Quorum)] <- 120*3/5
tmp$q[grep("1/3", tmp$Quorum)] <- 120*1/3 # insistencias --- 2/3 must reject
tmp$q[grep("2/3", tmp$Quorum)] <- 120*2/3
tmp$q[grep("3/5", tmp$Quorum)] <- 120*3/5
tmp$q[grep("Ley Orgánica Constitucional", tmp$Quorum)] <- as.integer(120*4/7)+1
tmp$q[grep("Quorum Calificado", tmp$Quorum)] <- (120*1/2)+1
tmp$q[grep("Quorum Simple", tmp$Quorum)] <- 1 # need a plurality only, one aye would suffice --- change to somethinglarger, e.g. average ayes in such votes?
tmp$q[grep("Constitucional 2/3", tmp$Quorum)] <- 1 # need 2/3 of present, so one would suffice 
# thres = threshold to win (>0 means plurality suffices, >2/3 means thwo thirds of present)
tmp$thres <- 0 # most cases
tmp$thres[grep("Constitucional 2/3", tmp$Quorum)] <- 2/3
#
tmp$qRule <- ""
tmp$qRule[tmp$q==1 & tmp$thres==1/2] <- "plurality"
tmp$qRule[tmp$q==1 & tmp$thres==2/3] <- "two-thirds present" # case of prez veto override of constitutional amendment? see final arts of const
tmp$qRule[tmp$q==40] <- "one-third"
tmp$qRule[tmp$q==61] <- "majority"
tmp$qRule[tmp$q==69] <- "four-sevenths"
tmp$qRule[tmp$q==72] <- "three-fifths"
#
tmp$Quorum <- NULL
#
tmp$ayes <- tmp$In.Favor
tmp$nays <- tmp$Against
tmp$noVote <- tmp$Abstencion
tmp$dispen <- tmp$Dispensados
tmp$noShow <- 120 - (tmp$ayes+tmp$nays+tmp$noVote+tmp$dispen)
tmp$In.Favor <- tmp$Against <- tmp$Abstencion <- tmp$Dispensados <- NULL
#
tmp$Result <- gsub(pattern = " ", replacement = "", tmp$Result) # drops spaces
tmp$dpassed <- ifelse ( tmp$ayes > tmp$nays   &
                        tmp$ayes/(tmp$ayes+tmp$nays+tmp$noVote) > tmp$thres   &
                        tmp$ayes > (tmp$q - ceiling(tmp$dispen/2))  , 1, 0) # assumes dispensados are to be removed from quorum
tmp$dpassed <- ifelse ( tmp$ayes > tmp$nays   &
                        tmp$ayes/(tmp$ayes+tmp$nays+tmp$noVote+tmp$dispen) > tmp$thres   &
                        tmp$ayes > tmp$q, 1, 0 ) # this does not make that assumption
#
# many coded as dpassed==0 appear "Aprobado" and vice versa... seem to be mistakes in the web page
## table(tmp$Result, tmp$dpassed)
## sel <- which(tmp$Result=="Aprobado" & tmp$dpassed==0)
## tmp[sel[2],]
tmp$Result <- NULL
#
tmp$votype <- ""
tmp$Type <- gsub(pattern = " ", replacement = "", tmp$Type) # drops spaces
tmp$votype[tmp$Type=="General"] <- "general"
tmp$votype[tmp$Type=="Particular"] <- "partic"
tmp$votype[tmp$Type=="Generalyparticular"] <- "both"
tmp$votype[tmp$Type=="ínica"] <- "sole"
tmp$votype[tmp$Type=="Única"] <- "sole"
tmp$Type <- NULL
#
tmp$artic <- tmp$Article; tmp$Article <- NULL
sel <- grep("([Cc]lausura|[Cc]ierre)( del)* debate", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("([Ii]ntegración|[Ii]ntegrarán)( de)*( la)* Comisión Mixta", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("([Pp]etición|[Ss]olicitud) para omitir", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("[Ss]olicitud para omitir", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("[Ss]olicitud de votar", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("[Rr]esolución de la Mesa", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("([Ee]nvío|[Rr]emisión) de proyecto", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("([Dd]evolución).*a la", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("nuevamente.*a [Cc]omisión", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("[Aa]cuerdo de la [Ss]ala", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("[Ss]olicitud.*votación particular", tmp$artic)
tmp$votype[sel] <- "proced"
sel <- grep("[Oo]bserva.*Presidente", tmp$artic)
tmp$votype[sel] <- "overr"
sel <- grep("[Oo]bservación|[Oo]bservaciones", tmp$artic)
tmp$votype[sel] <- "overr"
sel <- grep("OBSERVACIONES", tmp$artic)
tmp$votype[sel] <- "overr"
sel <- grep("[Vv]eto", tmp$artic)
tmp$votype[sel] <- "overr"
#
allVot <- tmp[, c("bol","date","q","thres","qRule","ayes","nays","noVote", "noShow", "dpassed", "votype", "artic")] # end by creating votes object
#head(allVot)
rm(tmp, sel, skip, tmp1, tmp2)
#
# CREATE OBJECT allUrg
# extract urgencias object, unlisted to prepare urgencia-as-unit data
tmp <- bills$urg
sel <- which(bills$info$nUrg>0) # bills with at least one urgencia
for (i in sel){
    tmp[[i]]$bol <- bills$info$bol[i]  # add boletin number
}
tmp <- tmp[sel] # drop elements without urgency
#
library(plyr)
allUrg <- ldply(tmp, data.frame) # unlist the data.frames into one large data.frame
#
# clean message on/off numbers
tmp1 <- allUrg$msgOn
tmp2 <- allUrg$msgOff
tmp1 <- sub(pattern = ".*(Simple|Suma|Discusióninmediata)", replacement = "", tmp1)
tmp1 <- sub(pattern = "CADUCAD[AO]", replacement = "", tmp1)
tmp1 <- sub(pattern = "Caducada", replacement = "", tmp1)
tmp2 <- sub(pattern = ".*(Simple|Suma|Discusióninmediata)", replacement = "", tmp2)
tmp2 <- sub(pattern = "CADUCAD[AO]", replacement = "", tmp2)
tmp2 <- sub(pattern = "Caducada", replacement = "", tmp2)
#
sel <- which(tmp1=="360-341383.341"); tmp1[sel] <- "360-341"; tmp2[sel] <- "383-341"
sel <- which(tmp1=="1022-3551135"); tmp1[sel] <- "1022-355"
sel <- which(tmp1=="11351173-355"); tmp1[sel] <- "11-351"; tmp2[sel] <- "173-355"
sel <- which(tmp1=="338-361a338-361"); tmp1[sel] <- "338-361a"; tmp2[sel] <- "338-361"
sel <- which(tmp1=="428-360457360"); tmp1[sel] <- "428-360"; tmp2[sel] <- "457-360"
sel <- which(tmp1=="216-.360241-360"); tmp1[sel] <- "216-360"; tmp2[sel] <- "241-360"
sel <- which(tmp1=="1769-3571812.357"); tmp1[sel] <- "1769-357"; tmp2[sel] <- "1812-357"
sel <- which(tmp1=="1812.3571889-357"); tmp1[sel] <- "1812-357"; tmp2[sel] <- "1889-357"
sel <- which(tmp1=="18973571933-357"); tmp1[sel] <- "1897-357"; tmp2[sel] <- "1933-357"
sel <- which(tmp1=="1850-3571897357"); tmp1[sel] <- "1850-357"; tmp2[sel] <- "1897-357"
sel <- which(tmp1=="1117-3561181"); tmp1[sel] <- "1117-356"
sel <- which(tmp1=="11811250-356"); tmp1[sel] <- "1181"; tmp2[sel] <- "1250-356"
sel <- which(tmp1=="374-355441"); tmp1[sel] <- "374-355"
sel <- which(tmp1=="197-355300.355"); tmp1[sel] <- "197-355"
sel <- which(tmp1=="300.355398-355"); tmp1[sel] <- "300-355"; tmp2[sel] <- "398-355"
sel <- which(tmp1=="278-348342.348"); tmp1[sel] <- "278-348"
sel <- which(tmp1=="30-34610042002"); tmp1[sel] <- "30-346"
sel <- which(tmp1=="149-351urg-351"); tmp1[sel] <- "149-351"
sel <- which(tmp1=="244-3450000"); tmp1[sel] <- "244-345"
sel <- which(tmp1=="2134382-343"); tmp1[sel] <- "21-343"; tmp2[sel] <- "82-343"
sel <- which(tmp1=="360-341383.341"); tmp1[sel] <- "360-341"; tmp2[sel] <- "383-341"
sel <- which(tmp1=="232-3405101999"); tmp1[sel] <- "232-340"
sel <- which(tmp1=="510199981-341"); tmp1[sel] <- "5101999"; tmp2[sel] <- "81-341"
sel <- which(tmp1=="20071999144-340"); tmp1[sel] <- "20071999"; tmp2[sel] <- "144-340"
sel <- which(tmp1=="23-34020071999"); tmp1[sel] <- "23-340"
sel <- which(tmp1=="02031999272-339"); tmp1[sel] <- "02031999"; tmp2[sel] <- "272-339"
sel <- which(tmp1=="272-33913071999"); tmp1[sel] <- "272-339"
sel <- which(tmp1=="13071999140-340"); tmp1[sel] <- "13071999"; tmp2[sel] <- "140-340"
sel <- which(tmp1=="1407199903081999"); tmp1[sel] <- "14071999"; tmp2[sel] <- "03081999"
sel <- which(tmp1=="0308199917081999"); tmp1[sel] <- "03081999"; tmp2[sel] <- "17081999"
sel <- which(tmp1=="42-34023061999"); tmp1[sel] <- "42-340"
sel <- which(tmp1=="0607199910081999"); tmp1[sel] <- "06071999"; tmp2[sel] <- "10081999"
sel <- which(tmp1=="10081999235-340"   ); tmp1[sel] <- "10081999"   ; tmp2[sel] <- "235-340"
sel <- which(tmp1=="190-33902031999"); tmp1[sel] <- "190-339"
sel <- which(tmp1=="0203199916031999"); tmp1[sel] <- "02031999"; tmp2[sel] <- "16031999"
sel <- which(tmp1=="3003199930031999"); tmp1[sel] <- "30031999"; tmp2[sel] <- "30031999"
sel <- which(tmp1=="1603199930031999"); tmp1[sel] <- "16031999"; tmp2[sel] <- "30031999"
sel <- which(tmp1=="1404199918051999"); tmp1[sel] <- "14041999"; tmp2[sel] <- "18051999"
sel <- which(tmp1=="1304199914041999"); tmp1[sel] <- "13041999"  ; tmp2[sel] <- "14041999"  
sel <- which(tmp1=="3003199913041999"); tmp1[sel] <- "30031999"; tmp2[sel] <- "13041999"
sel <- which(tmp1=="02031999273-339"); tmp1[sel] <- "02031999"; tmp2[sel] <- "273-339"
sel <- which(tmp1=="2206199920071999"); tmp1[sel] <- "22061999"; tmp2[sel] <- "20071999"
sel <- which(tmp1=="2007199921071999"); tmp1[sel] <- "20071999"; tmp2[sel] <- "21071999"
sel <- which(tmp1=="30059030051990"); tmp1[sel] <- "300590"; tmp2[sel] <- "30051990"     ## OJO: CHECK 2/4 DIGIT YEARS ELSEWHERE!
sel <- which(tmp1=="031090061190"); tmp1[sel] <- "031090"; tmp2[sel] <- "061190"
sel <- which(tmp1=="290590120690"); tmp1[sel] <- "290590"; tmp2[sel] <- "120690"
sel <- which(tmp1=="031090061190"); tmp1[sel] <- "031090"; tmp2[sel] <- "061190"
sel <- which(tmp1=="3005199026061990"); tmp1[sel] <- "30051990"; tmp2[sel] <- "26061990"
sel <- which(tmp1=="111290150191"); tmp1[sel] <- "111290"; tmp2[sel] <- "150191"
sel <- which(tmp1=="200591130691"); tmp1[sel] <- "200591"; tmp2[sel] <- "130691"
sel <- which(tmp1=="021193-328040194"); tmp1[sel] <- "021193-328"
sel <- which(tmp1=="040194090394"); tmp1[sel] <- "040194"; tmp2[sel] <- "090394"
sel <- which(tmp1=="170392150492"); tmp1[sel] <- "170392"; tmp2[sel] <- "150492"
sel <- which(tmp1=="150492120592"); tmp1[sel] <- "150492"; tmp2[sel] <- "120592"
sel <- which(tmp1=="280792190892"); tmp1[sel] <- "280792"; tmp2[sel] <- "190892"
sel <- which(tmp1=="071092211092"); tmp1[sel] <- "071092"; tmp2[sel] <- "211092"
sel <- which(tmp1=="30/05/9504/07/95"); tmp1[sel] <- "300595"; tmp2[sel] <- "040795"
sel <- which(tmp1=="04/07/9501/08/95"); tmp1[sel] <- "040795"; tmp2[sel] <- "010895"
sel <- grep(pattern = "[/]", tmp1)
tmp1[sel] <- sub(pattern = ".*([0-9]{2}[/][0-9]{2}[/][0-9]+)", replacement = "\\1", tmp1[sel])
tmp2[sel] <- sub(pattern = ".*([0-9]{2}[/][0-9]{2}[/][0-9]+)", replacement = "\\1", tmp2[sel])
tmp1[sel] <- gsub(pattern = "[/]", replacement = "", tmp1[sel])
tmp2[sel] <- gsub(pattern = "[/]", replacement = "", tmp2[sel])
sel <- which(tmp1=="300393040593"      ); tmp1[sel] <- "300393"; tmp2[sel] <- "040593"
sel <- which(tmp1=="020698010798"); tmp1[sel] <- "020698"; tmp2[sel] <- "010798"
sel <- which(tmp1=="010798040898"); tmp1[sel] <- "010798"; tmp2[sel] <- "040898"
sel <- which(tmp1=="040898180898"      ); tmp1[sel] <- "040898"      ; tmp2[sel] <- "180898"      
sel <- which(tmp1=="141293-328040194"); tmp1[sel] <- "141293-328"
sel <- which(tmp1=="040194090394"); tmp1[sel] <- "040194"; tmp2[sel] <- "090394"
sel <- which(tmp1=="070395-330110495"  ); tmp1[sel] <- "070395-330"
sel <- which(tmp1=="131092101192"      ); tmp1[sel] <- "131092"      ; tmp2[sel] <- "101192"
sel <- which(tmp1=="118-326256"        ); tmp1[sel] <- "118-326"
sel <- which(tmp1=="10798-338040898"); tmp1[sel] <- "10798-338"
sel <- grep(pattern = "[.]", tmp1); tmp1[sel] <- sub(pattern = "[.]", replacement = "-", tmp1[sel])
sel <- grep(pattern = "[.]", tmp2); tmp2[sel] <- sub(pattern = "[.]", replacement = "-", tmp2[sel])
sel <- which(tmp1=="101192091292"); tmp1[sel] <- "101192091292"; tmp2[sel] <- "101192091292"
sel <- which(tmp1=="091292120193"); tmp1[sel] <- "091292"; tmp2[sel] <- "120193"
sel <- which(tmp1=="020692090692"); tmp1[sel] <- "020692"; tmp2[sel] <- "090692"
sel <- which(tmp1=="3006926892"        ); tmp1[sel] <- "3006926892"        ; tmp2[sel] <- "3006926892"        
sel <- which(tmp1=="6892010992"); tmp1[sel] <- "6892010992"; tmp2[sel] <- "6892010992"
sel <- which(tmp1=="010992021193"); tmp1[sel] <- "010992021193"; tmp2[sel] <- "010992021193"
sel <- which(tmp1=="040194090394"); tmp1[sel] <- "040194090394"; tmp2[sel] <- "040194090394"
sel <- which(tmp1=="0410199415111994"); tmp1[sel] <- "0410199415111994"; tmp2[sel] <- "0410199415111994"
sel <- which(tmp1=="1511199403011995"); tmp1[sel] <- "1511199403011995"; tmp2[sel] <- "1511199403011995"
sel <- which(tmp1=="111192091292"); tmp1[sel] <- "111192091292"; tmp2[sel] <- "111192091292"
sel <- which(tmp1=="091292070193"); tmp1[sel] <- "091292070193"; tmp2[sel] <- "091292070193"
sel <- which(tmp1=="070193100393"        ); tmp1[sel] <- "070193"        ; tmp2[sel] <- "100393"        
sel <- which(tmp1=="080593070793"        ); tmp1[sel] <- "080593"        ; tmp2[sel] <- "070793"        
sel <- which(tmp1=="070793100893"        ); tmp1[sel] <- "070793"        ; tmp2[sel] <- "100893"
sel <- which(tmp1=="100893140993"        ); tmp1[sel] <- "100893"; tmp2[sel] <- "140993"
sel <- which(tmp1=="091193120194"        ); tmp1[sel] <- "091193"; tmp2[sel] <- "120194"
sel <- which(tmp1=="120194240194"        ); tmp1[sel] <- "120194"; tmp2[sel] <- "240194"
sel <- which(tmp1=="080693070793"        ); tmp1[sel] <- "080693"; tmp2[sel] <- "070793"
sel <- which(tmp1=="161196141293"        ); tmp1[sel] <- "161196"; tmp2[sel] <- "141293"
sel <- which(tmp1=="141293040194"        ); tmp1[sel] <- "141293"; tmp2[sel] <- "040194"
sel <- which(tmp1=="040194090394"        ); tmp1[sel] <- "040194"; tmp2[sel] <- "090394"
sel <- which(tmp1=="061092031192"        ); tmp1[sel] <- "061092"; tmp2[sel] <- "031192"
sel <- which(tmp1=="031192261192"        ); tmp1[sel] <- "031192"; tmp2[sel] <- "261192"
sel <- which(tmp1=="261192221292"        ); tmp1[sel] <- "261192"; tmp2[sel] <- "221292"
sel <- which(tmp1=="281092091232"        ); tmp1[sel] <- "281092"; tmp2[sel] <- "091232"
sel <- which(tmp1=="091232120193"        ); tmp1[sel] <- "091232"; tmp2[sel] <- "120193"
sel <- which(tmp1=="031193141293"        ); tmp1[sel] <- "031193"; tmp2[sel] <- "141293"
sel <- which(tmp1=="141293040194"        ); tmp1[sel] <- "141293"; tmp2[sel] <- "040194"
sel <- which(tmp1=="040194090394"        ); tmp1[sel] <- "040194"; tmp2[sel] <- "090394"
sel <- which(tmp1=="171192091292"        ); tmp1[sel] <- "171192"; tmp2[sel] <- "091292"
sel <- which(tmp1=="060695180795"        ); tmp1[sel] <- "060695"; tmp2[sel] <- "180795"
sel <- which(tmp1=="110593040194"        ); tmp1[sel] <- "110593"; tmp2[sel] <- "040194"
sel <- which(tmp1=="040194090394"        ); tmp1[sel] <- "040194"; tmp2[sel] <- "090394"
sel <- which(tmp1=="260897020997"        ); tmp1[sel] <- "260897"; tmp2[sel] <- "020997"
sel <- which(tmp1=="070493200493"        ); tmp1[sel] <- "070493"; tmp2[sel] <- "200493"
sel <- which(tmp1=="070493200493"        ); tmp1[sel] <- "070493"; tmp2[sel] <- "200493"
sel <- which(tmp1=="200493210493"        ); tmp1[sel] <- "200493"; tmp2[sel] <- "210493"
sel <- which(tmp1=="070793100893"        ); tmp1[sel] <- "070793"; tmp2[sel] <- "100893"
sel <- which(tmp1=="100893140993"        ); tmp1[sel] <- "100893"; tmp2[sel] <- "140993"
sel <- which(tmp1=="050494100594"        ); tmp1[sel] <- "050494"; tmp2[sel] <- "100594"
sel <- which(tmp1=="021193141293"        ); tmp1[sel] <- "021193"; tmp2[sel] <- "141293"
sel <- which(tmp1=="221195121295"        ); tmp1[sel] <- "221195"; tmp2[sel] <- "121295"
sel <- which(tmp1=="160394190494"        ); tmp1[sel] <- "160394"; tmp2[sel] <- "190494"
sel <- which(tmp1=="191093141293"        ); tmp1[sel] <- "191093"; tmp2[sel] <- "141293"
sel <- which(tmp1=="040194090394"        ); tmp1[sel] <- "040194"; tmp2[sel] <- "090394"
sel <- which(tmp1=="180194090394"        ); tmp1[sel] <- "180194"; tmp2[sel] <- "090394"
sel <- which(tmp1=="220394050494"        ); tmp1[sel] <- "220394"; tmp2[sel] <- "050494"
sel <- which(tmp1=="040898010998"        ); tmp1[sel] <- "040898"; tmp2[sel] <- "010998"
sel <- which(tmp1=="111094151194"        ); tmp1[sel] <- "111094"; tmp2[sel] <- "151194"
sel <- which(tmp1=="050794190794"        ); tmp1[sel] <- "050794"; tmp2[sel] <- "190794"
sel <- which(tmp1=="201094021194"        ); tmp1[sel] <- "201094"; tmp2[sel] <- "021194"
sel <- which(tmp1=="021194151194"        ); tmp1[sel] <- "021194"; tmp2[sel] <- "151194"
sel <- which(tmp1=="151194061294"        ); tmp1[sel] <- "151194"; tmp2[sel] <- "061294"
sel <- which(tmp1=="140395180495"        ); tmp1[sel] <- "140395"; tmp2[sel] <- "180495"
sel <- which(tmp1=="220895050995"        ); tmp1[sel] <- "220895"; tmp2[sel] <- "050995"
sel <- which(tmp1=="040698110698"        ); tmp1[sel] <- "040698"; tmp2[sel] <- "110698"
sel <- which(tmp1=="040495190495"        ); tmp1[sel] <- "040495"; tmp2[sel] <- "190495"
sel <- which(tmp1=="110795160895"        ); tmp1[sel] <- "110795"; tmp2[sel] <- "160895"
sel <- which(tmp1=="160895031095"        ); tmp1[sel] <- "160895"; tmp2[sel] <- "031095"
sel <- which(tmp1=="031095071195"        ); tmp1[sel] <- "031095"; tmp2[sel] <- "071195"
sel <- which(tmp1=="071195121295"        ); tmp1[sel] <- "071195"; tmp2[sel] <- "121295"
sel <- which(tmp1=="160196050396"        ); tmp1[sel] <- "160196"; tmp2[sel] <- "050396"
sel <- which(tmp1=="020496070596"        ); tmp1[sel] <- "020496"; tmp2[sel] <- "070596"
sel <- which(tmp1=="070596040696"        ); tmp1[sel] <- "070596"; tmp2[sel] <- "040696"
sel <- which(tmp1=="040696020796"        ); tmp1[sel] <- "040696"; tmp2[sel] <- "020796"
sel <- which(tmp1=="020796090796"        ); tmp1[sel] <- "020796"; tmp2[sel] <- "090796"
sel <- which(tmp1=="090796160796"        ); tmp1[sel] <- "090796"; tmp2[sel] <- "160796"
sel <- which(tmp1=="160796270896"        ); tmp1[sel] <- "160796"; tmp2[sel] <- "270896"
sel <- which(tmp1=="170697010797"        ); tmp1[sel] <- "170697"; tmp2[sel] <- "010797"
sel <- which(tmp1=="010797150797"        ); tmp1[sel] <- "010797"; tmp2[sel] <- "150797"
sel <- which(tmp1=="150797060897"        ); tmp1[sel] <- "150797"; tmp2[sel] <- "060897"
sel <- which(tmp1=="060897260897"        ); tmp1[sel] <- "060897"; tmp2[sel] <- "260897"
sel <- which(tmp1=="260897090997"        ); tmp1[sel] <- "260897"; tmp2[sel] <- "090997"
sel <- which(tmp1=="090997071097"        ); tmp1[sel] <- "090997"; tmp2[sel] <- "071097"
sel <- which(tmp1=="071097211097"        ); tmp1[sel] <- "071097"; tmp2[sel] <- "211097"
sel <- which(tmp1=="211097111197"        ); tmp1[sel] <- "211097"; tmp2[sel] <- "111197"
sel <- which(tmp1=="110898150998"        ); tmp1[sel] <- "110898"; tmp2[sel] <- "150998"
sel <- which(tmp1=="040898010998"        ); tmp1[sel] <- "040898"; tmp2[sel] <- "010998"
sel <- which(tmp1=="170697200797"        ); tmp1[sel] <- "170697"; tmp2[sel] <- "200797"
sel <- which(tmp1=="200797020997"        ); tmp1[sel] <- "200797"; tmp2[sel] <- "020997"
sel <- which(tmp1=="010797150797"        ); tmp1[sel] <- "010797"; tmp2[sel] <- "150797"
sel <- which(tmp1=="150797100997"        ); tmp1[sel] <- "150797"; tmp2[sel] <- "100997"
sel <- which(tmp1=="180898010998"        ); tmp1[sel] <- "180898"; tmp2[sel] <- "010998"
sel <- which(tmp1=="140798220798"        ); tmp1[sel] <- "140798"; tmp2[sel] <- "220798"
sel <- which(tmp1=="220798010998"        ); tmp1[sel] <- "220798"; tmp2[sel] <- "010998"
sel <- which(tmp1=="010998150998"        ); tmp1[sel] <- "010998"; tmp2[sel] <- "150998"
sel <- which(tmp1=="101198151298"        ); tmp1[sel] <- "101198"; tmp2[sel] <- "151298"
sel <- which(tmp1=="120193020398-326"    ); tmp1[sel] <- "120193"    ; tmp2[sel] <- "020398-326"
sel <- which(tmp1=="170697-335190697"    ); tmp1[sel] <- "170697-335"    ; tmp2[sel] <- "190697"
sel <- which(tmp1=="07061994205-329"     ); tmp1[sel] <- "07061994"     ; tmp2[sel] <- "205-329"
sel <- which(tmp1=="1101199925011999"    ); tmp1[sel] <- "11011999"    ; tmp2[sel] <- "25011999"
sel <- which(tmp1=="2501199909031994"    ); tmp1[sel] <- "25011999"    ; tmp2[sel] <- "09031994"
sel <- which(tmp1=="19049431051994"      ); tmp1[sel] <- "190494"      ; tmp2[sel] <- "31051994"
sel <- which(tmp1=="14129304011994"      ); tmp1[sel] <- "141293"      ; tmp2[sel] <- "04011994"
sel <- which(tmp1=="04011994130294"      ); tmp1[sel] <- "04011994"      ; tmp2[sel] <- "130294"
sel <- which(tmp1=="10798-338040898"     ); tmp1[sel] <- "10798-338"     ; tmp2[sel] <- "040898"
sel <- which(tmp1=="151194349-330"       ); tmp1[sel] <- "151194"       ; tmp2[sel] <- "349-330"
sel <- which(tmp1=="19079420894"         ); tmp1[sel] <- "190794"         ; tmp2[sel] <- "20894"
sel <- which(tmp1=="061294367-330"       ); tmp1[sel] <- "061294"       ; tmp2[sel] <- "367-330"
sel <- which(tmp1=="0203199906041999"    ); tmp1[sel] <- "02031999"    ; tmp2[sel] <- "06041999"
sel <- which(tmp1=="0604199906101999"    ); tmp1[sel] <- "06041999"    ; tmp2[sel] <- "06101999"
sel <- which(tmp1=="17-34002031999"      ); tmp1[sel] <- "17-340"      ; tmp2[sel] <- "02031999"
sel <- which(tmp1=="0610199980-341"      ); tmp1[sel] <- "06101999"      ; tmp2[sel] <- "80-341"
sel <- which(tmp1=="0501199919011999"    ); tmp1[sel] <- "05011999"    ; tmp2[sel] <- "19011999"
sel <- which(tmp1=="15-12-199805011999"  ); tmp1[sel] <- "15-12-1998"  ; tmp2[sel] <- "05011999"
sel <- which(tmp1=="0203199903031999"    ); tmp1[sel] <- "02031999"    ; tmp2[sel] <- "03031999"
sel <- which(tmp1=="675-33006061995"     ); tmp1[sel] <- "675-330"     ; tmp2[sel] <- "06061995"
sel <- which(tmp1=="06061995110795"      ); tmp1[sel] <- "06061995"      ; tmp2[sel] <- "110795"
sel <- which(tmp1=="270896250-333"       ); tmp1[sel] <- "270896"       ; tmp2[sel] <- "250-333"
sel <- which(tmp1=="6019820198-336"      ); tmp1[sel] <- "60198"      ; tmp2[sel] <- "20198-336"
sel <- which(tmp1=="1798-338040898"      ); tmp1[sel] <- "1798-338"      ; tmp2[sel] <- "040898"
sel <- which(tmp1=="040696963-333"       ); tmp1[sel] <- "040696"       ; tmp2[sel] <- "963-333"
sel <- which(tmp1=="198-344xxxxx"        ); tmp1[sel] <- "198-344"        ; tmp2[sel] <- "xxxxx"
sel <- which(tmp1=="300496150496"        ); tmp1[sel] <- "300496"        ; tmp2[sel] <- "150496"
sel <- which(tmp1=="467-33220000"        ); tmp1[sel] <- "467-332"        ; tmp2[sel] <- "20000"
sel <- which(tmp1=="2000120002"          ); tmp1[sel] <- "20001"          ; tmp2[sel] <- "20002"
sel <- which(tmp1=="2000220003"          ); tmp1[sel] <- "20002"          ; tmp2[sel] <- "20003"
sel <- which(tmp1=="2000320004"          ); tmp1[sel] <- "20003"          ; tmp2[sel] <- "20004"
sel <- which(tmp1=="2000420005"          ); tmp1[sel] <- "20004"          ; tmp2[sel] <- "20005"
sel <- which(tmp1=="2000520006"          ); tmp1[sel] <- "20005"          ; tmp2[sel] <- "20006"
sel <- which(tmp1=="2000720008"          ); tmp1[sel] <- "20007"          ; tmp2[sel] <- "20008"
sel <- which(tmp1=="2000920010"          ); tmp1[sel] <- "20009"          ; tmp2[sel] <- "20010"
sel <- which(tmp1=="2001020011"          ); tmp1[sel] <- "20010"          ; tmp2[sel] <- "20011"
sel <- which(tmp1=="2001120012"          ); tmp1[sel] <- "20011"          ; tmp2[sel] <- "20012"
sel <- which(tmp1=="2001320014"          ); tmp1[sel] <- "20013"          ; tmp2[sel] <- "20014"
sel <- which(tmp1=="2001520016"          ); tmp1[sel] <- "20015"          ; tmp2[sel] <- "20016"
sel <- which(tmp1=="20016200017"         ); tmp1[sel] <- "20016"         ; tmp2[sel] <- "200017"
sel <- which(tmp1=="20017160-336"        ); tmp1[sel] <- "20017"        ; tmp2[sel] <- "160-336"
sel <- which(tmp1=="0403971787-02"       ); tmp1[sel] <- "040397"       ; tmp2[sel] <- "1787-02"
sel <- which(tmp1=="1787-02150497"       ); tmp1[sel] <- "1787-02"       ; tmp2[sel] <- "150497"
sel <- which(tmp1=="18-3361456"          ); tmp1[sel] <- "18-336"          ; tmp2[sel] <- "1456"
sel <- which(tmp1=="14561004"            ); tmp1[sel] <- "1456"            ; tmp2[sel] <- "1004"
sel <- which(tmp1=="100430013-336"       ); tmp1[sel] <- "1004"       ; tmp2[sel] <- "30013-336"
sel <- which(tmp1=="3001430015"          ); tmp1[sel] <- "30014"          ; tmp2[sel] <- "30015"
sel <- which(tmp1=="4898-338180898"      ); tmp1[sel] <- "4898-338"      ; tmp2[sel] <- "180898"
sel <- which(tmp1=="20109847-339"        ); tmp1[sel] <- "201098"        ; tmp2[sel] <- "47-339"
sel <- which(tmp1=="106-3351008"         ); tmp1[sel] <- "106-335"         ; tmp2[sel] <- "1008"
sel <- which(tmp1=="57-3351024"          ); tmp1[sel] <- "57-335"          ; tmp2[sel] <- "1024"
sel <- which(tmp1=="395-3430020501"      ); tmp1[sel] <- "395-343"
sel <- which(tmp1=="0203199910031999"    ); tmp1[sel] <- "02031999"    ; tmp2[sel] <- "10031999"
sel <- which(tmp1=="140798-338040898"    ); tmp1[sel] <- "140798-338"    ; tmp2[sel] <- "040898"
sel <- which(tmp1=="49-338110898"        ); tmp1[sel] <- "49-338"        ; tmp2[sel] <- "110898"
sel <- which(tmp1=="184-33630010"        ); tmp1[sel] <- "184-336"        ; tmp2[sel] <- "30010"
sel <- which(tmp1=="010798-338040898"    ); tmp1[sel] <- "010798-338"    ; tmp2[sel] <- "040898"
sel <- which(tmp1=="13-339101198"        ); tmp1[sel] <- "13-339"        ; tmp2[sel] <- "101198"
sel <- which(tmp1=="27-335170697"        ); tmp1[sel] <- "27-335"        ; tmp2[sel] <- "170697"
sel <- which(tmp1=="15129805011999"      ); tmp1[sel] <- "151298"      ; tmp2[sel] <- "05011999"
sel <- which(tmp1=="0501199902031999"    ); tmp1[sel] <- "05011999"    ; tmp2[sel] <- "02031999"
sel <- which(tmp1=="0203199906041999"    ); tmp1[sel] <- "02031999"    ; tmp2[sel] <- "06041999"
sel <- which(tmp1=="0604199911051999"    ); tmp1[sel] <- "06041999"    ; tmp2[sel] <- "11051999"
sel <- which(tmp1=="0106199906071999"    ); tmp1[sel] <- "01061999"    ; tmp2[sel] <- "06071999"
sel <- which(tmp1=="0607199910081999"    ); tmp1[sel] <- "06071999"    ; tmp2[sel] <- "10081999"
sel <- which(tmp1=="10081999238-340"     ); tmp1[sel] <- "10081999"     ; tmp2[sel] <- "238-340"
sel <- which(tmp1=="201098159-339"       ); tmp1[sel] <- "201098"       ; tmp2[sel] <- "159-339"
sel <- which(tmp1=="0203199902031999"    ); tmp1[sel] <- "02031999"    ; tmp2[sel] <- "02031999"
sel <- which(tmp1=="0203199911051999"    ); tmp1[sel] <- "02031999"    ; tmp2[sel] <- "11051999"
sel <- which(tmp1=="1105199906071999"    ); tmp1[sel] <- "11051999"    ; tmp2[sel] <- "06071999"
sel <- which(tmp1=="132-34021071999"     ); tmp1[sel] <- "132-340"     ; tmp2[sel] <- "21071999"
sel <- which(tmp1=="21071999214-340"     ); tmp1[sel] <- "21071999"     ; tmp2[sel] <- "214-340"
sel <- which(tmp1=="43-33814798"         ); tmp1[sel] <- "43-338"         ; tmp2[sel] <- "14798"
sel <- which(tmp1=="1479821798"          ); tmp1[sel] <- "14798"          ; tmp2[sel] <- "21798"
sel <- which(tmp1=="116-338020998"       ); tmp1[sel] <- "116-338"       ; tmp2[sel] <- "020998"
sel <- which(tmp1=="8-339101198"         ); tmp1[sel] <- "8-339"         ; tmp2[sel] <- "101198"
sel <- which(tmp1=="101198211-339"       ); tmp1[sel] <- "101198"       ; tmp2[sel] <- "211-339"
sel <- which(tmp1=="21109857-339"        ); tmp1[sel] <- "211098"        ; tmp2[sel] <- "57-339"
sel <- which(tmp1=="157-33902031999"     ); tmp1[sel] <- "157-339"     ; tmp2[sel] <- "02031999"
sel <- which(tmp1=="239-33930031999"     ); tmp1[sel] <- "239-339"     ; tmp2[sel] <- "30031999"
sel <- which(tmp1=="48-338110898"        ); tmp1[sel] <- "48-338"        ; tmp2[sel] <- "110898"
sel <- which(tmp1=="101192091292"    ); tmp1[sel] <- "101192"    ; tmp2[sel] <- "091292"
sel <- which(tmp1=="010992021193"    ); tmp1[sel] <- "010992"    ; tmp2[sel] <- "021193"
sel <- which(tmp1=="0410199415111994"); tmp1[sel] <- "04101994"  ; tmp2[sel] <- "15111994"
sel <- which(tmp1=="1511199403011995"); tmp1[sel] <- "15111994"  ; tmp2[sel] <- "03011995"
sel <- which(tmp1=="111192091292"    ); tmp1[sel] <- "111192"    ; tmp2[sel] <- "091292"
sel <- which(tmp1=="091292070193"    ); tmp1[sel] <- "091292"    ; tmp2[sel] <- "070193"
sel <- which(tmp1=="1251811-336"     ); tmp1[sel] <- "125"       ; tmp2[sel] <- "1811-336"
sel <- which(tmp1=="269-356-356"     ); tmp1[sel] <- "269-356"   ; tmp2[sel] <- "520-356"
sel <- which(tmp1=="1637-3-357"      ); tmp2[sel] <- ""
allUrg$msgOn  <- tmp1
allUrg$msgOff <- tmp2
#
# drop urgencias after 10/3/2014
library(lubridate)
sel <- which(allUrg$on>dmy("10/03/2014"))
allUrg <- allUrg[-sel,]

##########################################################
## This line of analysis (chains) was dropped years ago ##
##########################################################
# ATTEMPT TO CORRECT NO OVERLAPS IN URGENCIAS WITHOUT FIXING ALL FROM:TO DATES IN TRAMITES---CHERRY-PICK CASES WITH NO OVERLAP
# SEEMS TO WORK!!
sel <- grep("check", allUrg$tramite) # select cases with check
for (i in sel){
    #i <- sel[170] # debug
    tmpOffendingDate <- allUrg$on[i]
    tmpBol <- allUrg$bol[i]
    tmpIndex <- which(bills$info$bol==tmpBol)
    tmpTram <- bills$tramites[[tmpIndex]]
    tmpHit <- bills$hitos[[tmpIndex]]
    tmpHit <- tmpHit[,c("date","chamber","action")]; tmpHit$action <- substring(tmpHit$action, 1, 40) # simplify object
    tmpHit <- tmpHit[-grep("Oficio (de )*(ley|modificaciones|rechazo|aprobación|aceptación)", tmpHit$action),] # drop oficios that tend to blur seq
    tmp <- tmpHit$date; tmp <- tmp[-1]; tmp <- c(tmp, bills$info$dateOut[tmpIndex]) # lag
    tmpHit$per <- interval(start = tmpHit$date, end = tmp)
    tmpSel <- tmpOffendingDate %my_within% tmpHit$per # in which period does offending date belong in?
    allUrg$tramite[i] <- tmpHit$chamber[tmpSel] # plug trámite where there was no overlap
}
#table(allUrg$tramite, useNA = "ifany")
#allUrg$bol[which(is.na(allUrg$tramite)==TRUE)]
# fix these by hand
sel <- which(allUrg$bol=="8149-09" & is.na(allUrg$tramite)==TRUE);  allUrg$tramite[sel] <- "sen"; 
sel <- which(allUrg$bol=="8612-02" & is.na(allUrg$tramite)==TRUE);  allUrg$tramite[sel] <- "sen"; 
#sel <- which(allUrg$bol=="5458-07" & is.na(allUrg$tramite)==TRUE);  allUrg$tramite[sel] <- "sen"; 
# these are not actual urgencias, mistakes in source
sel <- which(allUrg$bol=="497-15");  allUrg <- allUrg[-sel,] 
sel <- which(allUrg$bol=="706-15");  allUrg <- allUrg[-sel,] 
sel <- which(allUrg$bol=="771-15");  allUrg <- allUrg[-sel,]  
sel <- which(allUrg$bol=="801-15");  allUrg <- allUrg[-sel,] 
sel <- which(allUrg$bol=="802-15");  allUrg <- allUrg[-sel,] 
sel <- which(allUrg$bol=="1200-15"); allUrg <- allUrg[-sel,] 
sel <- which(allUrg$bol=="1291-15"); allUrg <- allUrg[-sel,] 
sel <- which(allUrg$bol=="1308-15"); allUrg <- allUrg[-sel,] 
#
# remove NAs in off dates (plug deadline)
sel <- which(is.na(allUrg$off)==TRUE); allUrg$off[sel] <- allUrg$deadline[sel]
#
# re-do chains, change, dretir across the board
#allUrg <- allUrg[order(allUrg$bol, allUrg$on),] # sort urgencias
tmp <- allUrg    # duplicate
tmp$msgOn[which(tmp$msgOn=="")] <- "NA" # remove empty on messages (missing) to avoid coding them as equal to missing off message below
tmp$chain2 <- 0; tmp$change2 <- 0; tmp$dretir2 <- tmp$dretir # open slot for new data
tmp$chain3 <- 0 # chain2 will recode chain with on/off dates, chain3 code new one with message numbers
tmpSel <- as.data.frame(table(tmp$bol), stringsAsFactors = FALSE)
for (i in 1:nrow(tmpSel)){
    message(sprintf("loop %s of %s", i, nrow(tmpSel)))
    #i <- 126 # debug
    #i <- which(tmpSel[,1]=="8149-09") # debug
    sel <- which(tmp$bol==tmpSel[i,1]) # select urgencias tied to one boletín
    tmpBillUrg <- tmp[sel,] # isolate urgencia messages
    #head(tmpBillUrg) # debug
    U <- tmpSel[i,2]
    ## for (j in 1:(U-1)){ # recode dretir with on/off dates
    ##     if (tmpBillUrg$off[j]==tmpBillUrg$on[j+1] &
    ##         tmpBillUrg$tramite[j]==tmpBillUrg$tramite[j+1]) tmpBillUrg$dretir2[j] <- 0
    ## }
    if (U==1) next         # cases with single urgencia 
    for (j in 1:(U-1)){ # recode dretir with on/off dates and same trámite (conf,veto,trib taken as same trámite)
        #j <- 1 #debug
        if (tmpBillUrg$off[j]>=tmpBillUrg$on[j+1] &
            (tmpBillUrg$tramite[j]==tmpBillUrg$tramite[j+1] |
             tmpBillUrg$tramite[j+1]=="conf" |
             tmpBillUrg$tramite[j+1]=="veto" |
             tmpBillUrg$tramite[j+1]=="trib")) tmpBillUrg$dretir2[j] <- 0
    }
    ## for (j in 2:U){
    ##     #j <- 2 # debug
    ##     if (tmpBillUrg$on[j]==tmpBillUrg$off[j-1] & tmpBillUrg$tramite[j]==tmpBillUrg$tramite[j-1]) tmpBillUrg$chain2[j] <- 1;
    ## }
# Computes chains using two criteria: equal on/off dates or equal on/off message numbers (when both info available) and equal on/off dates only (when message number missing)---sometimes on and off dates differ by 1 day, yet same message number indicates that exec is resetting urgency deadline and terms, eg bol=="372-15". [Added 12sep2015: Previous method misses chains that lack off message number (eg. bol=7007-18). Should change tmpBillUrg$on[j]==tmpBillUrg$off[j-1] with tmpBillUrg$on[j]<=tmpBillUrg$off[j-1]?
    for (j in 2:U){
        #j <- 2 # debug
        if (tmpBillUrg$on[j]<=tmpBillUrg$off[j-1] &         # with dates
            (tmpBillUrg$tramite[j]==tmpBillUrg$tramite[j-1] |
             tmpBillUrg$tramite[j]=="conf" |
             tmpBillUrg$tramite[j]=="veto" |
             tmpBillUrg$tramite[j]=="trib")) tmpBillUrg$chain2[j] <- 1;
        if ((tmpBillUrg$on[j]<=tmpBillUrg$off[j-1] |         # with dates
             tmpBillUrg$msgOn[j]==tmpBillUrg$msgOff[j-1]) &  # and message numbers
            (tmpBillUrg$tramite[j]==tmpBillUrg$tramite[j-1] |
             tmpBillUrg$tramite[j]=="conf" |
             tmpBillUrg$tramite[j]=="veto" |
             tmpBillUrg$tramite[j]=="trib")) tmpBillUrg$chain3[j] <- 1;
    }
    tmp2 <- tmpBillUrg$chain2
    tmp3 <- tmpBillUrg$chain3
    for (j in 2:U){
        tmp2[j] <- tmpBillUrg$chain2[j] + tmp2[j-1] * tmpBillUrg$chain2[j] # zero if no chain, else how many links
        tmp3[j] <- tmpBillUrg$chain3[j] + tmp3[j-1] * tmpBillUrg$chain3[j] # zero if no chain, else how many links
    }
    tmpBillUrg$chain2 <- tmp2; rm(tmp2)
    tmpBillUrg$chain3 <- tmp3; rm(tmp3)
    for (j in 2:U){
        if (tmpBillUrg$chain3[j]>0) tmpBillUrg$change2[j] <- as.numeric(tmpBillUrg$deadline[j] - tmpBillUrg$deadline[j-1])*100 / as.numeric(tmpBillUrg$deadline[j-1] - tmpBillUrg$on[j-1]) # % change new deadline CHOOSE IF chain2 (dates) or chain3 (dates or message num) should be used!
    }
    tmp[sel,] <- tmpBillUrg  # return manipulated object
}
## # compare dretir2 and dretir, chain2 and chain, change2 and change
## table(tmp$dretir2 - tmp$dretir)
## table(tmp$chain2)
## table(tmp$chain3)
## tmp$bol[which(tmp$dretir2 - tmp$dretir!=0)]
## tmp[which(tmp$bol=="372-15"),]
#
# input recoded info instead
tmp$chain <- tmp$chain3
tmp$dretir <- tmp$dretir2
tmp$change <- tmp$change2
tmp$chain2 <- tmp$chain3 <- tmp$dretir2 <- tmp$change2 <- NULL
#
# correction by hand (similar cases must be all over the place: retired messages after chamber has passed bill to other chamber are not retired!
sel <- which(tmp$bol=="8612-02" & tmp$tramite=="dip" & tmp$off==dmy("08-01-2013"))
tmp$off[sel] <- dmy("13-12-2012") # approved, moved to senate
tmp$dretir[sel] <- 0
## ## ALL THIS SHOULD BE NO LONGER NEEDED
## # check/fix chain info in manual corrections
## sel <- which(tmp$bol=="8149-09"); tmp[sel,]
## sel <- which(tmp$bol=="8149-09" & tmp$tramite=="dip" & tmp$on==dmy("12-06-2012"))
## tmp <- tmp[-sel,]
## sel <- which(tmp$bol=="8612-02" & tmp$tramite=="dip" & tmp$on==dmy("08-01-2013"))
## tmp <- tmp[-sel,]
## sel <- which(tmp$bol=="8612-02" & tmp$tramite=="sen" & tmp$on==dmy("16-10-2013") & tmp$off==dmy("16-10-2013"))
## tmp <- tmp[-sel,]
## sel <- which(tmp$bol=="5458-07" & tmp$tramite=="dip" & tmp$on==dmy("08-09-2009"))
## tmp$tramite[sel] <- "sen"
#
# replace object with manipulates dretir, chain, change
allUrg <- tmp
#
#sel <- which(allUrg$bol=="8612-02"); allUrg[sel,] # INTERESTING CASE STUDY? urg#1 in sen dropped (for summer break?) then a looong chain. Didn't pass and theme uninteresting (fireworks mensaje) SEARCH OTHERS
#
rm(i,j,sel,tmp,tmpBillUrg,tmpBol,tmpHit,tmpIndex,tmpOffendingDate,tmpSel,tmpTram,U,tmp1) # cleaning
#
# fix mistakes from source in urgencia dates
sel <- which(allUrg$bol=="2111-10" & year(allUrg$off)==1997)
year(allUrg$off[sel]) <- 1998;
allUrg$dretir[which(allUrg$bol=="2111-10")] <- 0 # none retired, all passed to other chamber
#
sel <- which(allUrg$bol=="4369-05" & allUrg$off==dmy("10-08-2006"))
allUrg$on[sel] <- allUrg$on[sel] - days(7)
allUrg$deadline[sel] <- allUrg$deadline[sel] - days(7)
allUrg$type[sel] <- "Discusión inmediata"
allUrg$tramite[sel] <- "dip"; allUrg$trNum[sel] <- 1
sel <- which(allUrg$bol=="4369-05" & allUrg$off==dmy("27-09-2006"));
allUrg$dretir[sel] <- 0 # not retired
allUrg$off[sel] <- dmy("3-10-2006") # seem to be a chain link
#
# add numeric type: 1,2,3 for di, su, si; 4.1,4.2,4.3 for resets of each type; 5.1,5.2,5.3 will be for retired of each type, added below
allUrg$typeN <- 0; allUrg$typeN[allUrg$type=="Discusión inmediata"] <- 1; allUrg$typeN[allUrg$type=="Suma"] <- 2; allUrg$typeN[allUrg$type=="Simple"] <- 3;
tmp2 <- allUrg$typeN # will be used for retires
allUrg$typeN[allUrg$chain>0] <- 4 + allUrg$typeN[allUrg$chain>0]/10 
# add messages retiring urgency
tmp <- allUrg[allUrg$dretir==1,]; tmp2 <- tmp2[allUrg$dretir==1]
tmp$typeN <- 5 + tmp2/10
tmp$on <- tmp$deadline <- tmp$off
tmp$dretir <- 0
allUrg <- rbind(allUrg, tmp) # binds retiring messages for graph
#
# sort urgencias so that retiro messages do not appear bunched at the end 
tmp <- 1:nrow(allUrg) # preserve original order id on dates tie
allUrg <- allUrg[order(allUrg$bol, allUrg$on, tmp),]
allUrg$chain[allUrg$typeN>5] <- allUrg$chain[allUrg$typeN>5] + 1 # fix chain number of retiro messages (they are copies of message they withdraw)
#
# drop urgencias after 10/3/2014 (repeat, since off messages were added)
sel <- which(allUrg$on>dmy("10/03/2014"))
allUrg <- allUrg[-sel,]
table(allUrg$typeN, useNA = "ifany")
## # exports csv of allUrg to process in plots.r
## save(bills, allUrg, file = paste(datdir, "allUrg.RData", sep = "")) # <--- export
## rm(sel, tmp, tmp2)
## # further transformations of allUrg in plots.r in preparation for graph AND BELOW <-- need to move save below
#
# MANIPULATE/CLEAN URGENCIAS OBJECT, PREP FOR ANALYSIS 
# add session dates to recode on date with next closest session
tmpD <- bills$sessions[bills$sessions$ddip==1,] # diputado sessions
tmpS <- bills$sessions[bills$sessions$dsen==1,] # senado sessions
allUrg$on2 <- allUrg$on; allUrg$deadline2 <- allUrg$deadline
for (i in 1:nrow(allUrg)){
    message(sprintf("loop %s of %s", i, nrow(allUrg)))
    #i <- 1 # debug
    tmpD1 <- tmpD$date - allUrg$on[i] # difference from urgency's date to all sessions
    tmpS1 <- tmpS$date - allUrg$on[i] 
    j <- which(tmpD1==min(tmpD1[tmpD1>=0])) # closest next session date
    #if (is.na(allUrg$tramite[i])==FALSE & allUrg$tramite[i]=="dip") allUrg$on2[i] <- tmpD$date[j]
    if (is.na(allUrg$tramite[i])==FALSE & allUrg$tramite[i]!="sen") allUrg$on2[i] <- tmpD$date[j] # veto, conf fixed with diputado session dates
    j <- which(tmpS1==min(tmpS1[tmpS1>=0])) # closest next session date
    if (is.na(allUrg$tramite[i])==FALSE & allUrg$tramite[i]=="sen") allUrg$on2[i] <- tmpS$date[j]
}
# quick fix for new deadline: add days difference (deadline function not working...)
tmp2 <- difftime(allUrg$on2, allUrg$on, units="days")
allUrg$deadline2 <- allUrg$deadline + days(tmp2)
## tmp2 <- rep(0, nrow(allUrg)) # for days to deadline
## sel <- which(allUrg$typeN==1 | allUrg$typeN==4.1); tmp2[sel] <- 6
## sel <- which(allUrg$typeN==2 | allUrg$typeN==4.2); tmp2[sel] <- 15
## sel <- which(allUrg$typeN==3 | allUrg$typeN==4.3); tmp2[sel] <- 30
## sel <- which(allUrg$on2  < dmy("3/7/2010")); tmp2[sel] <- mapvalues(tmp2[sel], from = c(6,15), to = c(3,10), warn_missing = FALSE) #pre-reform
## library(timeDate)
## sel <- which(tmp2>0); allUrg$deadline2[sel] <- deadline(allUrg$on2[sel], tmp2[sel])
#
allUrg$on <- allUrg$on2; allUrg$deadline <- allUrg$deadline2; allUrg$on2 <- allUrg$deadline2 <- NULL # recode on and deadline dates
# remove off date from mensajes caducados---coded as expired with deadline (none were retired)
sel <-  which(allUrg$dcaduca==1 &                    allUrg$off>allUrg$deadline) # change off date to deadline
allUrg$off[sel] <- allUrg$deadline[sel]
## sel2 <- which(allUrg$dcaduca==1 & allUrg$dretir==1 & allUrg$off>allUrg$deadline & allUrg$typeN>5)
## allUrg <- allUrg[-sel2,] # drop their retiro messages---none had been retired, in fact
#
# many "retiros" actually occurred after deadline... meaning???
tmp <- rep(0, nrow(allUrg)); tmp[allUrg$off>allUrg$deadline] <- 1
message("many retiros actually occurred after deadline... meaning???"); table(tmp, allUrg$typeN)
#
# splits allUrg into two objects: the original with all URGENCY MESSAGES, a new one with all URGENCY CHAINS (incl. singletons)
allUrgChains <- allUrg
# consolidate chains
tmp <- allUrgChains
tmp$nTyp3 <- tmp$nTyp2 <- tmp$nTyp1 <- tmp$links <- tmp$chainN <- 0;
tmp$nTyp1[tmp$typeN==  1] <- 1 # by default, count types 1,2,3 for cases where loop ignores
tmp$nTyp2[tmp$typeN==4.1] <- 1
tmp$nTyp2[tmp$typeN==  2] <- 1
tmp$nTyp2[tmp$typeN==4.2] <- 1
tmp$nTyp3[tmp$typeN==  3] <- 1
tmp$nTyp3[tmp$typeN==4.3] <- 1
tmpSel <- as.data.frame(table(tmp$bol), stringsAsFactors = FALSE)
for (i in 1:nrow(tmpSel)){
    message(sprintf("loop %s of %s", i, nrow(tmpSel)))
    #i <-1740 # debug
    sel <- which(tmp$bol==tmpSel[i,1]) # select urgencias tied to one boletín
    tmpBillUrg <- tmp[sel,] # isolate urgencia messages
    #head(tmpBillUrg) # debug
    if (tmpSel[i,2]==1){                    # cases with single urgencia 
        tmp$chainN[sel] <- tmp$links[sel] <- 1  # cases with single urgencia 
        next                                # cases with single urgencia 
    }
    tmpBillUrg$chainN[tmpBillUrg$chain==0] <- 1:length(tmpBillUrg$chainN[tmpBillUrg$chain==0]) # fills chain number
    for (j in 2:nrow(tmpBillUrg)){                                                             # fills chain number
        if (tmpBillUrg$chain[j]==0) next
        tmpBillUrg$chainN[j] <- tmpBillUrg$chainN[j-1]
    }
    tmpBillUrg$links <-    ave(tmpBillUrg$chain+1,  tmpBillUrg$chainN, FUN=max, na.rm=FALSE) # may want to subtract 1 in case last message retires urgency
    tmpBillUrg$deadline <- ave(tmpBillUrg$deadline, tmpBillUrg$chainN, FUN=max, na.rm=FALSE)
    tmpBillUrg$off <-      ave(tmpBillUrg$off,      tmpBillUrg$chainN, FUN=max, na.rm=FALSE)
    tmpBillUrg$dretir <-   ave(tmpBillUrg$dretir,   tmpBillUrg$chainN, FUN=max, na.rm=FALSE)
    tmpBillUrg$dcaduca <-  ave(tmpBillUrg$dcaduca,  tmpBillUrg$chainN, FUN=max, na.rm=FALSE)
    tmpBillUrg$change <-   ave(tmpBillUrg$change,   tmpBillUrg$chainN, FUN=sum, na.rm=FALSE)
    tmpBillUrg$nTyp1 <-    ave(tmpBillUrg$nTyp1,    tmpBillUrg$chainN, FUN=sum, na.rm=FALSE)
    tmpBillUrg$nTyp2 <-    ave(tmpBillUrg$nTyp2,    tmpBillUrg$chainN, FUN=sum, na.rm=FALSE)
    tmpBillUrg$nTyp3 <-    ave(tmpBillUrg$nTyp3,    tmpBillUrg$chainN, FUN=sum, na.rm=FALSE)
    #
    tmpBillUrg <- tmpBillUrg[duplicated(tmpBillUrg$chainN)==FALSE,] # drop redundant chain info
    tmp <- tmp[-sel,]             # drop unmanipulated data from object
    tmp <- rbind(tmp, tmpBillUrg) # ... replacing by manipulated info
}
#
# rename elements
tmp$chain <- tmp$type <- NULL
# fine-tune typeN: urg went all the way to deadline 1.0, 2,0, 3.0; deadline modif 1.4, 2.4, 3.4; retir 1.5, 2.5, 3.5; modif, then retir 1.45, 2.45, 3.45
tmp$typeN[tmp$links==1 & tmp$dretir==1] <- tmp$typeN[tmp$links==1 & tmp$dretir==1] + .5 
tmp$typeN[tmp$links>1  & tmp$dretir==0] <- tmp$typeN[tmp$links>1  & tmp$dretir==0] + .4 
tmp$typeN[tmp$links>1  & tmp$dretir==1] <- tmp$typeN[tmp$links>1  & tmp$dretir==1] + .45 
table(tmp$typeN, useNA = "ifany") ##### lots of .45s, miscoding retiros??? <-- FIX MAY BE TO FINE-TUNE DATE TO OTHER CHAMBER, DROPPING LATER RETIROS
# sort
tmp1 <- 1:nrow(tmp) # preserve original order in case of tie
tmp <- tmp[order(tmp$bol, tmp$on, tmp1),]
# did bill eventually pass? exec-init?
tmp$dmensaje <- tmp$dpassed <- NA
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    tmp$dpassed[i] <-  bills$info$dpassed [which(bills$info$bol==tmp$bol[i])]
    tmp$dmensaje[i] <- bills$info$dmensaje[which(bills$info$bol==tmp$bol[i])]
}
head(tmp)
#
# replace by manipulated object
allUrgChains <- tmp
#
# make single object containing messages and chains
allUrg <- list(messages=allUrg, chains=allUrgChains)
rm(allUrgChains)
#
rm(i,j,sel,tmp,tmp1,tmp2,tmpBillUrg,tmpD,tmpS,tmpD1,tmpS1,tmpSel)

# ADD WHETHER OR NOT A VOTE FOLLOWED URGENCIAS IN DIPUTADOS
tmp <- allUrg$chains # extract object for manipulation
sel <- which(tmp$on > dmy("11-03-2002") & tmp$tramite!="sen") # only have diputado votes since 2002
tmp$nvotDdln2 <- tmp$nvotDdln <- NA                                         # add slots for new data
#
tmp <- tmp[sel,] # subset for manipulation
#head(tmp)
for (i in 1:nrow(tmp)){
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    #i <- 103 # debug
    tmpVotIndices <- grep(pattern = tmp$bol[i], allVot$bol)
    if (length(tmpVotIndices)==0){
        tmp$nvotDdln[i] <- tmp$nvotDdln2[i] <- 0;
        next
    } else {
        tmpPeriod <- interval(start = tmp$on[i], end = tmp$off[i])
        tmpHit <- allVot$date[tmpVotIndices] %within% tmpPeriod # in which period does date.on belong in?
        tmp$nvotDdln[i] <- length(which(tmpHit==TRUE)) # input number of votes held in period
        tmpPeriod <- interval(start = tmp$on[i], end = tmp$off[i]+weeks(2)) # extend period by 1 week
        tmpHit <- allVot$date[tmpVotIndices] %within% tmpPeriod # in which period does date.on belong in?
        tmp$nvotDdln2[i] <- length(which(tmpHit==TRUE)) # input number of votes held in period
    }
}
#
# descriptives: was a vote in dip held within deadline? within deadline plus 2 weeks?
sel <- which(tmp$dpassed==0)
votInDdl <- table(tmp$nvotDdln[sel]>0, tmp$typeN[sel])
votInDdl2 <- table(tmp$nvotDdln2[sel]>0, tmp$typeN[sel])
#
## round(prop.table(votInDdl ,2)*100, 0)
## margin.table(votInDdl ,2)
## round(prop.table(votInDdl2,2)*100, 0)
## margin.table(votInDdl2,2)
message("Vote occurred within deadline")
rbind(
    round(prop.table(votInDdl ,2)*100, 0),
    margin.table(votInDdl ,2)
    )
## message("Vote occurred within deadline doubled")
## rbind(
##     round(prop.table(votInDdl2 ,2)*100, 0),
##     margin.table(votInDdl2 ,2)
##     )
#
rm(votInDdl,votInDdl2)
#
## # PUZZLE: urgencias sumas and simple that are not in chains tend to not get to a vote...
## #Vote occurred within deadline (bills that didn't eventually pass)
##         1 1.4 1.45 1.5   2 2.4 2.45 2.5   3 3.4 3.45 3.5
## FALSE  52  50   71   0  92  79   78  11  96  78   84  71
## TRUE   48  50   29 100   8  21   22  89   4  22   16  29
##        25   4    7   4 370  39  109  27 272  36  138  17
## #
## #Vote occurred within deadline (bills that eventually passed)
##         1 1.4 1.45 1.5   2 2.4 2.45 2.5   3 3.4 3.45 3.5
## FALSE  23   8   41   0  72  39   43   9  66  38   52  29
## TRUE   77  92   59 100  28  61   57  91  34  62   48  71
##       127  37   46  53 358  94  233 110 190  90  236  41

# creates referral to x committee dummies. Includes REFERRED TO HACIENDA COMMITTEE (IE., NEEDS APPROPRIATION)
bills$info$drefAgric <- 0
bills$info$drefCiencia <- 0
bills$info$drefComunic <- 0
bills$info$drefConst <- 0
bills$info$drefCultura <- 0
bills$info$drefDefensa <- 0
bills$info$drefDDHH <- 0
bills$info$drefEco <- 0
bills$info$drefEduc <- 0
bills$info$drefFamilia <- 0
bills$info$drefHda <- 0
bills$info$drefInterior <- 0
bills$info$drefMedAmb <- 0
bills$info$drefMineria <- 0
bills$info$drefObras <- 0
bills$info$drefPesca <- 0
bills$info$drefRREE <- 0
bills$info$drefRecNatur <- 0
bills$info$drefRegimen <- 0
bills$info$drefSalud <- 0
bills$info$drefSegurid <- 0
bills$info$drefTrabajo <- 0
bills$info$drefVivienda <- 0
bills$info$drefZonas <- 0
bills$info$dcomUnidas <- 0
bills$info$dcomEspec <- 0
bills$info$dcomMixta <- 0
## # deprecated 10jul17: drop if next block works ok
## for (i in 1:I){
##     tmp <- bills$hitos[[i]]$action[grep(".*[Pp]asa a [Cc]omisi[óo]n.*", bills$hitos[[i]]$action)]
##     if (length(grep("[Hh]acienda", tmp))>0) bills$info$drefHda[i] <- 1
## }
for (i in 1:I){
    message(sprintf("iteración %s of %s", i, I))
    #i <- i+1#i <- 27 # debug
    tmp <- bills$hitos[[i]]$action[grep(".*pasa a Comisi[óo]n.*", bills$hitos[[i]]$action, ignore.case = TRUE)]  # busca hitos de paso a comisión
    tmp <- gsub(pattern = "^(?:.*)Pasa a (?:la )*", replacement = "", tmp, ignore.case = TRUE)     # elimina texto precedente
    #tmp # debug
    if (length(grep("Agricultura", tmp, ignore.case = TRUE))>0)
        bills$info$drefAgric[i] <- 1
    if (length(grep("Ciencias*", tmp, ignore.case = TRUE))>0)
        bills$info$drefCiencia[i] <- 1
    if (length(grep("(?:Tele)*comunicaciones", tmp, ignore.case = TRUE))>0)
        bills$info$drefComunic[i] <- 1
    if (length(grep("Constitución", tmp, ignore.case = TRUE))>0)
        bills$info$drefConst[i] <- 1
    if (length(grep("(?!Es*pecial)Cultura", tmp, ignore.case = TRUE, perl = TRUE))>0)
        bills$info$drefCultura[i] <- 1
    if (length(grep("Defensa", tmp, ignore.case = TRUE))>0)
        bills$info$drefDefensa[i] <- 1
    if (length(grep("Derechos Humanos|DD.HH", tmp, ignore.case = TRUE))>0)
        bills$info$drefDDHH[i] <- 1
    if (length(grep("Economía", tmp, ignore.case = TRUE))>0)
        bills$info$drefEco[i] <- 1
    if (length(grep("Educación", tmp, ignore.case = TRUE))>0)
        bills$info$drefEduc[i] <- 1
    if (length(grep("Familia", tmp, ignore.case = TRUE))>0)
        bills$info$drefFamilia[i] <- 1
    if (length(grep("Hacienda", tmp, ignore.case = TRUE))>0)
        bills$info$drefHda[i] <- 1
    if (length(grep("Gobierno", tmp, ignore.case = TRUE))>0)
        bills$info$drefInterior[i] <- 1
    if (length(grep("Medio Ambiente", tmp, ignore.case = TRUE))>0)
        bills$info$drefMedAmb[i] <- 1
    if (length(grep("Minería", tmp, ignore.case = TRUE))>0)
        bills$info$drefMineria[i] <- 1
    if (length(grep("Obras", tmp, ignore.case = TRUE))>0)
        bills$info$drefObras[i] <- 1
    if (length(grep("Intereses Marítimos", tmp, ignore.case = TRUE))>0)
        bills$info$drefPesca[i] <- 1
    if (length(grep("Relaciones Exteriores", tmp, ignore.case = TRUE))>0)
        bills$info$drefRREE[i] <- 1
    if (length(grep("Recursos", tmp, ignore.case = TRUE))>0)
        bills$info$drefRecNatur[i] <- 1
    if (length(grep("Régimen", tmp, ignore.case = TRUE))>0)
        bills$info$drefRegimen[i] <- 1
    if (length(grep("Salud", tmp, ignore.case = TRUE))>0)
        bills$info$drefSalud[i] <- 1
    if (length(grep("Seguridad", tmp, ignore.case = TRUE))>0)
        bills$info$drefSegurid[i] <- 1
    if (length(grep("Trabajo", tmp, ignore.case = TRUE))>0)
        bills$info$drefTrabajo[i] <- 1
    if (length(grep("Vivienda", tmp, ignore.case = TRUE))>0)
        bills$info$drefVivienda[i] <- 1
    if (length(grep("Zonas(?: Extremas)*", tmp, ignore.case = TRUE))>0)
        bills$info$drefZonas[i] <- 1
    # fix few cases where committee name is misleading
    if (length(grep("Senador Horvath.*Comisión de Intereses Marítimos Pesca y Acuicultura", tmp))>0) bills$info$drefPesca[i] <- 1
    if (length(grep("Senador Horvath.*Comisión de Intereses Marítimos Pesca y Acuicultura", tmp))>0) bills$info$drefMedAmb[i] <- 0
    if (length(grep("Senador Horvath.*Comisión de Intereses Marítimos Pesca y Acuicultura", tmp))>0) bills$info$drefMedAmb[i] <- 0
    # comisiones especiales o mixtas
    if (length(grep("u[nb]idas*", tmp, ignore.case = TRUE))>0) bills$info$dcomUnidas[i] <- 1
    if (length(grep("es*pecial|PYMES|Hídricos|Turismo|TLC|Deporte|Presupuesto", tmp, ignore.case = TRUE))>0) bills$info$dcomEspec[i] <- 1
    if (length(grep("Mixta", tmp, ignore.case = TRUE))>0) bills$info$dcomMixta[i] <- 1
}
#table(bills$info$drefHda)  # debug
#table(bills$info$drefEduc) # debug
#
tmp <-
    bills$info$drefAgric +
    bills$info$drefCiencia +
    bills$info$drefComunic +
    bills$info$drefConst +
    bills$info$drefCultura +
    bills$info$drefDefensa +
    bills$info$drefDDHH +
    bills$info$drefEco +
    bills$info$drefEduc +
    bills$info$drefFamilia +
    bills$info$drefInterior +
    bills$info$drefMedAmb +
    bills$info$drefMineria +
    bills$info$drefObras +
    bills$info$drefPesca +
    bills$info$drefRREE +
    bills$info$drefRecNatur +
    bills$info$drefRegimen +
    bills$info$drefSalud +
    bills$info$drefSegurid +
    bills$info$drefTrabajo +
    bills$info$drefVivienda +
    bills$info$dcomEspec +
    bills$info$dcomMixta +
    bills$info$dcomUnidas +
    bills$info$drefHda +
    bills$info$drefZonas 
#table(tmp) #debug
sel <- which(tmp==0) # misses many. next loop relies on softer search to hit missing
for (i in sel){
    message(sprintf("iteración %s of %s", i, max(sel)))
    #i <- i+1#i <- 4 # debug
    tmp <- bills$hitos[[i]]$action[grep(".*comisi[óo]n.*", bills$hitos[[i]]$action, ignore.case = TRUE)]  # busca hitos de paso a comisión
    tmp <- gsub(pattern = "^(?:.*)Pasa a (?:la )*", replacement = "", tmp, ignore.case = TRUE)     # elimina texto precedente
    #tmp # debug
    if (length(grep("Agricultura", tmp, ignore.case = TRUE))>0)
        bills$info$drefAgric[i] <- 1
    if (length(grep("Ciencias*", tmp, ignore.case = TRUE))>0)
        bills$info$drefCiencia[i] <- 1
    if (length(grep("(?:Tele)*comunicaciones", tmp, ignore.case = TRUE))>0)
        bills$info$drefComunic[i] <- 1
    if (length(grep("Constitución", tmp, ignore.case = TRUE))>0)
        bills$info$drefConst[i] <- 1
    if (length(grep("(?!Es*pecial)Cultura", tmp, ignore.case = TRUE, perl = TRUE))>0)
        bills$info$drefCultura[i] <- 1
    if (length(grep("Defensa", tmp, ignore.case = TRUE))>0)
        bills$info$drefDefensa[i] <- 1
    if (length(grep("Derechos Humanos|DD.HH", tmp, ignore.case = TRUE))>0)
        bills$info$drefDDHH[i] <- 1
    if (length(grep("Economía", tmp, ignore.case = TRUE))>0)
        bills$info$drefEco[i] <- 1
    if (length(grep("Educación", tmp, ignore.case = TRUE))>0)
        bills$info$drefEduc[i] <- 1
    if (length(grep("Familia", tmp, ignore.case = TRUE))>0)
        bills$info$drefFamilia[i] <- 1
    if (length(grep("Hacienda", tmp, ignore.case = TRUE))>0)
        bills$info$drefHda[i] <- 1
    if (length(grep("Gobierno", tmp, ignore.case = TRUE))>0)
        bills$info$drefInterior[i] <- 1
    if (length(grep("Medio Ambiente", tmp, ignore.case = TRUE))>0)
        bills$info$drefMedAmb[i] <- 1
    if (length(grep("Minería", tmp, ignore.case = TRUE))>0)
        bills$info$drefMineria[i] <- 1
    if (length(grep("Obras", tmp, ignore.case = TRUE))>0)
        bills$info$drefObras[i] <- 1
    if (length(grep("Intereses Marítimos", tmp, ignore.case = TRUE))>0)
        bills$info$drefPesca[i] <- 1
    if (length(grep("Relaciones Exteriores", tmp, ignore.case = TRUE))>0)
        bills$info$drefRREE[i] <- 1
    if (length(grep("Recursos", tmp, ignore.case = TRUE))>0)
        bills$info$drefRecNatur[i] <- 1
    if (length(grep("Régimen", tmp, ignore.case = TRUE))>0)
        bills$info$drefRegimen[i] <- 1
    if (length(grep("Salud", tmp, ignore.case = TRUE))>0)
        bills$info$drefSalud[i] <- 1
    if (length(grep("Seguridad", tmp, ignore.case = TRUE))>0)
        bills$info$drefSegurid[i] <- 1
    if (length(grep("Trabajo", tmp, ignore.case = TRUE))>0)
        bills$info$drefTrabajo[i] <- 1
    if (length(grep("Vivienda", tmp, ignore.case = TRUE))>0)
        bills$info$drefVivienda[i] <- 1
    if (length(grep("Zonas(?: Extremas)*", tmp, ignore.case = TRUE))>0)
        bills$info$drefZonas[i] <- 1
}
tmp <-
    bills$info$drefAgric +
    bills$info$drefCiencia +
    bills$info$drefComunic +
    bills$info$drefConst +
    bills$info$drefCultura +
    bills$info$drefDefensa +
    bills$info$drefDDHH +
    bills$info$drefEco +
    bills$info$drefEduc +
    bills$info$drefFamilia +
    bills$info$drefInterior +
    bills$info$drefMedAmb +
    bills$info$drefMineria +
    bills$info$drefObras +
    bills$info$drefPesca +
    bills$info$drefRREE +
    bills$info$drefRecNatur +
    bills$info$drefRegimen +
    bills$info$drefSalud +
    bills$info$drefSegurid +
    bills$info$drefTrabajo +
    bills$info$drefVivienda +
#    bills$info$drefHda +
    bills$info$drefZonas
bills$info$dmultiRef <- as.numeric(tmp>1) # excludes hacienda, that is controlled separately in the regressions
#table(bills$info$dmultiRef)
#
#### WILL NEED TO EXTRACT CASES WHERE PRES WITHDREW THE PROJECT... THIS HELPS TOWARDS THAT GOAL
sel <- grep("[Pp]residen.*[Rr]etir", bills$hitosRaw) # this needs more elaborate regexp to match other patterns
bills$hitos[sel[374]] # still problematic
bills$info$status[sel[374]]

########################################################################################
# SYSTEMATIZE COMMITTEE REPORTS AND PRODUCE WEEKLY FIGURES WITH URGENCIES FOR ANALYSIS #
# another line of analysis that I dropped mid-way                                      #
########################################################################################
bills$reportsRaw <- bills$reports
#                                  # add space here if re-running
#bills$reports <- bills$reportsRaw # if re-running, add this at start
#
sel <- which(bills$info$hasReport=="yes")
for (i in sel){
    #i <- sel[102] # debug
    message(sprintf("loop %s of %s", i, I))
    tmp <- bills$reportsRaw[[i]]
    tmp <- tmp[-1] # drop heading
    tmp1 <- rep(NA, length(tmp))
    #tmpRep <- data.frame(date=tmp1, chamber=tmp1, tramite=tmp1, informe=tmp1, comm=tmp1, bol=tmp1, dmensaje=tmp1) # ALL THIS CAN BE OBTAINED; WILL ONLY GET DATE, COMMITTEE, dmensaje, bol, chamber FOR NOW
    tmpRep <- data.frame(date=tmp1, chamber=tmp1, bol=tmp1, dmensaje=tmp1, comm=tmp1)
    tmp1 <- sub(pattern = "([0-9]{2}[a-zA-Z. ]+[0-9]{4}).*", replacement = "\\1", tmp)
    tmp1 <- gsub(pattern = " de ", replacement = "-", tmp1)
    tmp1 <- gsub(pattern = "Ene.", replacement = "01", x = tmp1)
    tmp1 <- gsub(pattern = "Feb.", replacement = "02", x = tmp1)
    tmp1 <- gsub(pattern = "Mar.", replacement = "03", x = tmp1)
    tmp1 <- gsub(pattern = "Abr.", replacement = "04", x = tmp1)
    tmp1 <- gsub(pattern = "May.", replacement = "05", x = tmp1)
    tmp1 <- gsub(pattern = "Jun.", replacement = "06", x = tmp1)
    tmp1 <- gsub(pattern = "Jul.", replacement = "07", x = tmp1)
    tmp1 <- gsub(pattern = "Ago.", replacement = "08", x = tmp1)
    tmp1 <- gsub(pattern = "Sep.", replacement = "09", x = tmp1)
    tmp1 <- gsub(pattern = "Oct.", replacement = "10", x = tmp1)
    tmp1 <- gsub(pattern = "Nov.", replacement = "11", x = tmp1)
    tmp1 <- gsub(pattern = "Dic.", replacement = "12", x = tmp1)
    tmpRep$date <- dmy(tmp1)
    tmp1 <- sub(pattern = ".*(Senado|Diputados).*", replacement = "\\1", tmp)
    tmpRep$chamber <- tmp1
    tmpRep$chamber[tmpRep$chamber=="Senado"] <- "sen"
    tmpRep$chamber[tmpRep$chamber=="Diputados"] <- "dip"
    tmpRep$bol <- bills$info$bol[i]
    tmpRep$dmensaje <- bills$info$dmensaje[i]
    tmp1 <- sub(pattern = ".*[Cc]omisi[óo]n(.*) Ver", replacement = "\\1", tmp)
    tmpRep$comm <- tmp1
    #
    bills$reports[[i]] <- tmpRep
}
#
# CREATE ONE LARGE DATA FRAME WITH ALL REPORTS
tmp <- bills$reports
tmp <- tmp[sel] # drop elements with no report
allRep <- ldply(tmp, data.frame) # unlist the data.frames into one large data.frame
#
# distinguish Hacienda reports
allRep$dHda <- 0
allRep$dHda[grep("Hacienda", allRep$comm)] <- 1
#
# sort by date
#allRep$ordOrig <- 1:nrow(allRep)
allRep <- allRep[order(allRep$date, allRep$bol),]
# drop reports before and after these dates
sel <- which(allRep$date<dmy("11-03-1998") | allRep$date>dmy("10-03-2014"))
allRep <- allRep[-sel,]
#
# add moción dummy for aggregates
allRep$dmocion <- 1-allRep$dmensaje
#
#
RepDataNegBin <- list(weeklyHaciendaReportsAndUrgenciasToMensajesInHaciendaComm = NA, 
                      weeklyHaciendaReportsAndUrgenciasToMocionesInHaciendaComm = NA, 
                      weeklyHaciendaReportsAndUrgenciasToAllBillsInHaciendaComm = NA, 
                      weeklyReportsAndUrgenciasToMensajes                       = NA, 
                      weeklyReportsAndUrgenciasToMociones                       = NA, 
                      weeklyReportsAndUrgenciasToAllBills                       = NA)


##########################################################################################################################
##########################################################################################################################
## PREPARE WEEKLY REPORTS FROM HACIENDA COMMITTEE WITH WEEKLY URGENCIES TO EX-INIT BILLS REFERRED TO HACIENDA COMMITTEE ##
##########################################################################################################################
##########################################################################################################################
# dip and sen separate
dipRep <- allRep[allRep$chamber=="dip",]
senRep <- allRep[allRep$chamber=="sen",]
#
# add week
dipRep$week <- year(dipRep$date)+week(dipRep$date)/100
senRep$week <- year(senRep$date)+week(senRep$date)/100
#                                                             ################
# consolidate weekly reports (from Hacienda reported only)    <---  OJO   ####
#                                                             ################
tmp <- ddply(dipRep, .(week), mutate, nrepMenDip=sum(dmensaje*dHda), nrepMocDip=sum(dmocion*dHda)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
dipRep <- tmp
tmp <- ddply(senRep, .(week), mutate, nrepMenSen=sum(dmensaje*dHda), nrepMocSen=sum(dmocion*dHda)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
senRep <- tmp
#
# object with all weeks in period
tmp <- data.frame(date = seq(from = dmy("11-03-1990"), to = dmy("10-03-2014"), by = 'weeks'))
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
weekRepUrg <- tmp
#
weekRepUrg <- merge(x = weekRepUrg, y = dipRep, by = "week", all=TRUE)
weekRepUrg <- merge(x = weekRepUrg, y = senRep, by = "week", all=TRUE)
#
# Add weekly urgencias
tmp1 <- allUrg
### added 22Aug2015: problem here, allUrg is now a list of messages and chains... need to select one before continuing...
tmp1 <- allUrg$messages
tmp1$week <- year(tmp1$on) + week(tmp1$on)/100
# distinguish mensaje urgencia from mocion urgencias
tmp1$dmensaje <- NA
tmp1$dHda <- NA
for (i in 1:nrow(tmp1)){
    message(sprintf("loop %s of %s", i, nrow(tmp1)))
    j <- which(bills$info$bol==tmp1$bol[i])
    tmp1$dmensaje[i] <- bills$info$dmensaje[j]
    tmp1$dHda[i] <- bills$info$drefHda[j] # bill was referred to Hacienda committee
}
tmp1$count <- 1
#                                                      ###################################################################################
tmp1$count <- tmp1$count * tmp1$dHda * tmp1$dmensaje   ###### COUNT ONLY URGENCIAS ON HACIENDA MENSAJES OR ON HACIENDA MOCIONES!!!  ######
#                                                      ###################################################################################
#
# aggregate dip discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IF GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg1Dip=sum(count)); tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg1Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg2Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg2Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg3Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg3Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset to discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg41Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg41Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg42Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg42Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg43Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg43Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg51Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg51Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg52Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg52Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg53Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg53Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# add session
tmp <- bills$sessions
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
tmp <- ddply(tmp, .(week), mutate, ndipses=sum(ddip), nsenses=sum(dsen)); tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$ddip <- tmp$dsen <- NULL
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
weekRepUrg[is.na(weekRepUrg)] <- 0 # fill zeroes
head(weekRepUrg)
#
# WHEN DATE HAD NO SESSION, ADD NUMBERS TO NEXT SESSION
tmp <- weekRepUrg
# dip
sel <- which(tmp$ndipses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$ndipses>0])) # closest next session date
    tmp[j, grep("Dip", colnames(tmp))] <- tmp[j, grep("Dip", colnames(tmp))] + tmp[i, grep("Dip", colnames(tmp))] # add data to that row
    tmp[i, grep("Dip", colnames(tmp))] <- 0 # erase it from week with no session
}
# sen
sel <- which(tmp$nsenses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug 
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$nsenses>0])) # closest next session date
    tmp[j, grep("Sen", colnames(tmp))] <- tmp[j, grep("Sen", colnames(tmp))] + tmp[i, grep("Sen", colnames(tmp))] # add data to that row
    tmp[i, grep("Sen", colnames(tmp))] <- 0 # erase it from week with no session
}
#
weekRepUrg <- tmp
#
# Add president's maj status in chamber
weekRepUrg$dmajDip <- weekRepUrg$dmajSen <- 0
# sen
tmp <- dmy(c("11-03-1990", "22-01-1999", "11-03-2000", "11-01-2002", "27-01-2005", "30-08-2005", "11-03-2006", "11-03-2010") )
tmp <- year(tmp) + week(tmp)/100
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dmajSen[sel] <- 1 
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[6] & weekRepUrg$week < tmp[7]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[7] & weekRepUrg$week < tmp[8]); weekRepUrg$dmajSen[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[8] & weekRepUrg$week < tmp[9]); weekRepUrg$dmajSen[sel] <- 0
#
# dip: always maj=1 (2010-14 50%, coded 1)
weekRepUrg$dmajDip <- 1
#
# weeks to end of pdtl term
tmp <- dmy(c("11-03-1994", "11-03-2000", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$pterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$pterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$pterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$pterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$pterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$pterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of dip term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dterm[sel] <- round((tmp[6] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of sen term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2006", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$sterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$sterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$sterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$sterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$sterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
#
# legislature dummies (periodo)
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dleg90 <- weekRepUrg$dleg94 <- weekRepUrg$dleg98 <- weekRepUrg$dleg02 <- weekRepUrg$dleg06 <- weekRepUrg$dleg10 <- 0
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dleg90[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dleg94[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dleg98[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dleg02[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dleg06[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dleg10[sel] <- 1
#
# plug to data object
RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMensajesInHaciendaComm <- weekRepUrg
#
###########################################################################################################################
###########################################################################################################################
## PREPARE WEEKLY REPORTS FROM HACIENDA COMMITTEE WITH WEEKLY URGENCIES TO LEG-INIT BILLS REFERRED TO HACIENDA COMMITTEE ##
###########################################################################################################################
###########################################################################################################################
# dip and sen separate
dipRep <- allRep[allRep$chamber=="dip",]
senRep <- allRep[allRep$chamber=="sen",]
#
# add week
dipRep$week <- year(dipRep$date)+week(dipRep$date)/100
senRep$week <- year(senRep$date)+week(senRep$date)/100
#                                                             ################
# consolidate weekly reports (from Hacienda reported only)    <---  OJO   ####
#                                                             ################
tmp <- ddply(dipRep, .(week), mutate, nrepMenDip=sum(dmensaje*dHda), nrepMocDip=sum(dmocion*dHda)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
dipRep <- tmp
tmp <- ddply(senRep, .(week), mutate, nrepMenSen=sum(dmensaje*dHda), nrepMocSen=sum(dmocion*dHda)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
senRep <- tmp
#
# object with all weeks in period
tmp <- data.frame(date = seq(from = dmy("11-03-1990"), to = dmy("10-03-2014"), by = 'weeks'))
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
weekRepUrg <- tmp
#
weekRepUrg <- merge(x = weekRepUrg, y = dipRep, by = "week", all=TRUE)
weekRepUrg <- merge(x = weekRepUrg, y = senRep, by = "week", all=TRUE)
#
# Add weekly urgencias
# old tmp1 <- allUrg
### added 22Aug2015: problem here, allUrg is now a list of messages and chains... need to select one before continuing...
tmp1 <- allUrg$messages
tmp1$week <- year(tmp1$on) + week(tmp1$on)/100
# distinguish mensaje urgencia from mocion urgencias
tmp1$dmensaje <- NA
tmp1$dHda <- NA
for (i in 1:nrow(tmp1)){
    message(sprintf("loop %s of %s", i, nrow(tmp1)))
    j <- which(bills$info$bol==tmp1$bol[i])
    tmp1$dmensaje[i] <- bills$info$dmensaje[j]
    tmp1$dHda[i] <- bills$info$drefHda[j] # bill was referred to Hacienda committee
}
tmp1$count <- 1
#                                                          ###############################################################################
tmp1$count <- tmp1$count * tmp1$dHda * (1 - tmp1$dmensaje) #### COUNT ONLY URGENCIAS ON HACIENDA MENSAJES OR ON HACIENDA MOCIONES!!!  ####
#                                                          ###############################################################################
#
# aggregate dip discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IF GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg1Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg1Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg2Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg2Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg3Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg3Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset to discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg41Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg41Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg42Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg42Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg43Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg43Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg51Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg51Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg52Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg52Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg53Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg53Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# add session
tmp <- bills$sessions
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
tmp <- ddply(tmp, .(week), mutate, ndipses=sum(ddip), nsenses=sum(dsen)); tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$ddip <- tmp$dsen <- NULL
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
weekRepUrg[is.na(weekRepUrg)] <- 0 # fill zeroes
#
# WHEN DATE HAD NO SESSION, ADD NUMBERS TO NEXT SESSION
tmp <- weekRepUrg
# dip
sel <- which(tmp$ndipses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$ndipses>0])) # closest next session date
    tmp[j, grep("Dip", colnames(tmp))] <- tmp[j, grep("Dip", colnames(tmp))] + tmp[i, grep("Dip", colnames(tmp))] # add data to that row
    tmp[i, grep("Dip", colnames(tmp))] <- 0 # erase it from week with no session
}
# sen
sel <- which(tmp$nsenses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug 
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$nsenses>0])) # closest next session date
    tmp[j, grep("Sen", colnames(tmp))] <- tmp[j, grep("Sen", colnames(tmp))] + tmp[i, grep("Sen", colnames(tmp))] # add data to that row
    tmp[i, grep("Sen", colnames(tmp))] <- 0 # erase it from week with no session
}
#
weekRepUrg <- tmp
#
# Add president's maj status in chamber
weekRepUrg$dmajDip <- weekRepUrg$dmajSen <- 0
# sen
tmp <- dmy(c("11-03-1990", "22-01-1999", "11-03-2000", "11-01-2002", "27-01-2005", "30-08-2005", "11-03-2006", "11-03-2010") )
tmp <- year(tmp) + week(tmp)/100
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dmajSen[sel] <- 1 
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[6] & weekRepUrg$week < tmp[7]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[7] & weekRepUrg$week < tmp[8]); weekRepUrg$dmajSen[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[8] & weekRepUrg$week < tmp[9]); weekRepUrg$dmajSen[sel] <- 0
#
# dip: always maj=1 (2010-14 50%, coded 1)
weekRepUrg$dmajDip <- 1
#
# weeks to end of pdtl term
tmp <- dmy(c("11-03-1994", "11-03-2000", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$pterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$pterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$pterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$pterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$pterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$pterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of dip term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dterm[sel] <- round((tmp[6] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of sen term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2006", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$sterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$sterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$sterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$sterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$sterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
#
# legislature dummies (periodo)
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dleg90 <- weekRepUrg$dleg94 <- weekRepUrg$dleg98 <- weekRepUrg$dleg02 <- weekRepUrg$dleg06 <- weekRepUrg$dleg10 <- 0
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dleg90[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dleg94[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dleg98[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dleg02[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dleg06[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dleg10[sel] <- 1
#
# plug to data object
RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMocionesInHaciendaComm <- weekRepUrg
#
######################################################################################################################
######################################################################################################################
## PREPARE WEEKLY REPORTS FROM HACIENDA COMMITTEE WITH WEEKLY URGENCIES TO ALL BILLS REFERRED TO HACIENDA COMMITTEE ##
######################################################################################################################
######################################################################################################################
# dip and sen separate
dipRep <- allRep[allRep$chamber=="dip",]
senRep <- allRep[allRep$chamber=="sen",]
#
# add week
dipRep$week <- year(dipRep$date)+week(dipRep$date)/100
senRep$week <- year(senRep$date)+week(senRep$date)/100
#                                                             ################
# consolidate weekly reports (from Hacienda reported only)    <---  OJO   ####
#                                                             ################
tmp <- ddply(dipRep, .(week), mutate, nrepMenDip=sum(dmensaje*dHda), nrepMocDip=sum(dmocion*dHda)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
dipRep <- tmp
tmp <- ddply(senRep, .(week), mutate, nrepMenSen=sum(dmensaje*dHda), nrepMocSen=sum(dmocion*dHda)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
senRep <- tmp
#
# object with all weeks in period
tmp <- data.frame(date = seq(from = dmy("11-03-1990"), to = dmy("10-03-2014"), by = 'weeks'))
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
weekRepUrg <- tmp
#
weekRepUrg <- merge(x = weekRepUrg, y = dipRep, by = "week", all=TRUE)
weekRepUrg <- merge(x = weekRepUrg, y = senRep, by = "week", all=TRUE)
#
# Add weekly urgencias
# old tmp1 <- allUrg
### added 22Aug2015: problem here, allUrg is now a list of messages and chains... need to select one before continuing...
tmp1 <- allUrg$messages
tmp1$week <- year(tmp1$on) + week(tmp1$on)/100
# distinguish mensaje urgencia from mocion urgencias
tmp1$dmensaje <- NA
tmp1$dHda <- NA
for (i in 1:nrow(tmp1)){
    message(sprintf("loop %s of %s", i, nrow(tmp1)))
    j <- which(bills$info$bol==tmp1$bol[i])
    tmp1$dmensaje[i] <- bills$info$dmensaje[j]
    tmp1$dHda[i] <- bills$info$drefHda[j] # bill was referred to Hacienda committee
}
tmp1$count <- 1
#                                                   ###################################################################################
tmp1$count <- tmp1$count * tmp1$dHda                ###### COUNT ONLY URGENCIAS ON HACIENDA MENSAJES OR ON HACIENDA MOCIONES!!!  ######
#                                                   ###################################################################################
#
# aggregate dip discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IF GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg1Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg1Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg2Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg2Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg3Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg3Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset to discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg41Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg41Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg42Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg42Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg43Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg43Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg51Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg51Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg52Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg52Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg53Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg53Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# add session
tmp <- bills$sessions
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
tmp <- ddply(tmp, .(week), mutate, ndipses=sum(ddip), nsenses=sum(dsen)); tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$ddip <- tmp$dsen <- NULL
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
weekRepUrg[is.na(weekRepUrg)] <- 0 # fill zeroes
#
# WHEN DATE HAD NO SESSION, ADD NUMBERS TO NEXT SESSION
tmp <- weekRepUrg
# dip
sel <- which(tmp$ndipses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$ndipses>0])) # closest next session date
    tmp[j, grep("Dip", colnames(tmp))] <- tmp[j, grep("Dip", colnames(tmp))] + tmp[i, grep("Dip", colnames(tmp))] # add data to that row
    tmp[i, grep("Dip", colnames(tmp))] <- 0 # erase it from week with no session
}
# sen
sel <- which(tmp$nsenses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug 
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$nsenses>0])) # closest next session date
    tmp[j, grep("Sen", colnames(tmp))] <- tmp[j, grep("Sen", colnames(tmp))] + tmp[i, grep("Sen", colnames(tmp))] # add data to that row
    tmp[i, grep("Sen", colnames(tmp))] <- 0 # erase it from week with no session
}
#
weekRepUrg <- tmp
#
# Add president's maj status in chamber
weekRepUrg$dmajDip <- weekRepUrg$dmajSen <- 0
# sen
tmp <- dmy(c("11-03-1990", "22-01-1999", "11-03-2000", "11-01-2002", "27-01-2005", "30-08-2005", "11-03-2006", "11-03-2010") )
tmp <- year(tmp) + week(tmp)/100
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dmajSen[sel] <- 1 
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[6] & weekRepUrg$week < tmp[7]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[7] & weekRepUrg$week < tmp[8]); weekRepUrg$dmajSen[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[8] & weekRepUrg$week < tmp[9]); weekRepUrg$dmajSen[sel] <- 0
#
# dip: always maj=1 (2010-14 50%, coded 1)
weekRepUrg$dmajDip <- 1
#
# weeks to end of pdtl term
tmp <- dmy(c("11-03-1994", "11-03-2000", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$pterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$pterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$pterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$pterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$pterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$pterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of dip term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dterm[sel] <- round((tmp[6] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of sen term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2006", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$sterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$sterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$sterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$sterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$sterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
#
# legislature dummies (periodo)
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dleg90 <- weekRepUrg$dleg94 <- weekRepUrg$dleg98 <- weekRepUrg$dleg02 <- weekRepUrg$dleg06 <- weekRepUrg$dleg10 <- 0
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dleg90[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dleg94[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dleg98[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dleg02[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dleg06[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dleg10[sel] <- 1
#
# plug to data object
RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToAllBillsInHaciendaComm <- weekRepUrg
#
#######################################################################################
#######################################################################################
## PREPARE WEEKLY REPORTS FROM ANY COMMITTEE WITH WEEKLY URGENCIES TO EX-INIT BILLS  ##
#######################################################################################
#######################################################################################
# dip and sen separate
dipRep <- allRep[allRep$chamber=="dip",]
senRep <- allRep[allRep$chamber=="sen",]
#
# add week
dipRep$week <- year(dipRep$date)+week(dipRep$date)/100
senRep$week <- year(senRep$date)+week(senRep$date)/100
#                                                             ################
# consolidate weekly reports                                  <---  OJO   ####
#                                                             ################
tmp <- ddply(dipRep, .(week), mutate, nrepMenDip=sum(dmensaje     ), nrepMocDip=sum(dmocion     )) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
dipRep <- tmp
tmp <- ddply(senRep, .(week), mutate, nrepMenSen=sum(dmensaje     ), nrepMocSen=sum(dmocion     )) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
senRep <- tmp
#
# object with all weeks in period
tmp <- data.frame(date = seq(from = dmy("11-03-1990"), to = dmy("10-03-2014"), by = 'weeks'))
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
weekRepUrg <- tmp
#
weekRepUrg <- merge(x = weekRepUrg, y = dipRep, by = "week", all=TRUE)
weekRepUrg <- merge(x = weekRepUrg, y = senRep, by = "week", all=TRUE)
#
# Add weekly urgencias
# old tmp1 <- allUrg
### added 22Aug2015: problem here, allUrg is now a list of messages and chains... need to select one before continuing...
tmp1 <- allUrg$messages
tmp1$week <- year(tmp1$on) + week(tmp1$on)/100
# distinguish mensaje urgencia from mocion urgencias
tmp1$dmensaje <- NA
tmp1$dHda <- NA
for (i in 1:nrow(tmp1)){
    message(sprintf("loop %s of %s", i, nrow(tmp1)))
    j <- which(bills$info$bol==tmp1$bol[i])
    tmp1$dmensaje[i] <- bills$info$dmensaje[j]
    tmp1$dHda[i] <- bills$info$drefHda[j] # bill was referred to Hacienda committee
}
tmp1$count <- 1
#                                                         ###############################################################
tmp1$count <- tmp1$count              * tmp1$dmensaje     ###### COUNT ONLY URGENCIAS ON MENSAJES OR MOCIONES!!!  #######
#                                                         ###############################################################
#
# aggregate dip discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IF GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg1Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg1Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg2Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg2Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg3Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg3Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset to discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg41Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg41Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg42Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg42Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg43Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg43Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg51Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg51Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg52Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg52Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg53Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg53Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# add session
tmp <- bills$sessions
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
tmp <- ddply(tmp, .(week), mutate, ndipses=sum(ddip), nsenses=sum(dsen)); tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$ddip <- tmp$dsen <- NULL
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
weekRepUrg[is.na(weekRepUrg)] <- 0 # fill zeroes
#
# WHEN DATE HAD NO SESSION, ADD NUMBERS TO NEXT SESSION
tmp <- weekRepUrg
# dip
sel <- which(tmp$ndipses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$ndipses>0])) # closest next session date
    tmp[j, grep("Dip", colnames(tmp))] <- tmp[j, grep("Dip", colnames(tmp))] + tmp[i, grep("Dip", colnames(tmp))] # add data to that row
    tmp[i, grep("Dip", colnames(tmp))] <- 0 # erase it from week with no session
}
# sen
sel <- which(tmp$nsenses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug 
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$nsenses>0])) # closest next session date
    tmp[j, grep("Sen", colnames(tmp))] <- tmp[j, grep("Sen", colnames(tmp))] + tmp[i, grep("Sen", colnames(tmp))] # add data to that row
    tmp[i, grep("Sen", colnames(tmp))] <- 0 # erase it from week with no session
}
#
weekRepUrg <- tmp
#
# Add president's maj status in chamber
weekRepUrg$dmajDip <- weekRepUrg$dmajSen <- 0
# sen
tmp <- dmy(c("11-03-1990", "22-01-1999", "11-03-2000", "11-01-2002", "27-01-2005", "30-08-2005", "11-03-2006", "11-03-2010") )
tmp <- year(tmp) + week(tmp)/100
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dmajSen[sel] <- 1 
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[6] & weekRepUrg$week < tmp[7]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[7] & weekRepUrg$week < tmp[8]); weekRepUrg$dmajSen[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[8] & weekRepUrg$week < tmp[9]); weekRepUrg$dmajSen[sel] <- 0
#
# dip: always maj=1 (2010-14 50%, coded 1)
weekRepUrg$dmajDip <- 1
#
# weeks to end of pdtl term
tmp <- dmy(c("11-03-1994", "11-03-2000", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$pterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$pterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$pterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$pterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$pterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$pterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of dip term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dterm[sel] <- round((tmp[6] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of sen term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2006", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$sterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$sterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$sterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$sterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$sterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
#
# legislature dummies (periodo)
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dleg90 <- weekRepUrg$dleg94 <- weekRepUrg$dleg98 <- weekRepUrg$dleg02 <- weekRepUrg$dleg06 <- weekRepUrg$dleg10 <- 0
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dleg90[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dleg94[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dleg98[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dleg02[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dleg06[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dleg10[sel] <- 1
#
# plug to data object
RepDataNegBin$weeklyReportsAndUrgenciasToMensajes <- weekRepUrg
#
########################################################################################
########################################################################################
## PREPARE WEEKLY REPORTS FROM ANY COMMITTEE WITH WEEKLY URGENCIES TO LEG-INIT BILLS  ##
########################################################################################
########################################################################################
# dip and sen separate
dipRep <- allRep[allRep$chamber=="dip",]
senRep <- allRep[allRep$chamber=="sen",]
#
# add week
dipRep$week <- year(dipRep$date)+week(dipRep$date)/100
senRep$week <- year(senRep$date)+week(senRep$date)/100
#                                                             ################
# consolidate weekly reports                                  <---  OJO   ####
#                                                             ################
tmp <- ddply(dipRep, .(week), mutate, nrepMenDip=sum(dmensaje     ), nrepMocDip=sum(dmocion     )) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
dipRep <- tmp
tmp <- ddply(senRep, .(week), mutate, nrepMenSen=sum(dmensaje     ), nrepMocSen=sum(dmocion     )) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
senRep <- tmp
#
# object with all weeks in period
tmp <- data.frame(date = seq(from = dmy("11-03-1990"), to = dmy("10-03-2014"), by = 'weeks'))
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
weekRepUrg <- tmp
#
weekRepUrg <- merge(x = weekRepUrg, y = dipRep, by = "week", all=TRUE)
weekRepUrg <- merge(x = weekRepUrg, y = senRep, by = "week", all=TRUE)
#
# Add weekly urgencias
# old tmp1 <- allUrg
### added 22Aug2015: problem here, allUrg is now a list of messages and chains... need to select one before continuing...
tmp1 <- allUrg$messages
tmp1$week <- year(tmp1$on) + week(tmp1$on)/100
# distinguish mensaje urgencia from mocion urgencias
tmp1$dmensaje <- NA
tmp1$dHda <- NA
for (i in 1:nrow(tmp1)){
    message(sprintf("loop %s of %s", i, nrow(tmp1)))
    j <- which(bills$info$bol==tmp1$bol[i])
    tmp1$dmensaje[i] <- bills$info$dmensaje[j]
    tmp1$dHda[i] <- bills$info$drefHda[j] # bill was referred to Hacienda committee
}
tmp1$count <- 1
#                                                         ###############################################################
tmp1$count <- tmp1$count        * (1 - tmp1$dmensaje)     ###### COUNT ONLY URGENCIAS ON MENSAJES OR MOCIONES!!!  #######
#                                                         ###############################################################
#
# aggregate dip discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IF GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg1Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg1Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg2Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg2Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg3Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg3Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset to discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg41Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg41Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg42Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg42Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg43Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg43Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg51Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg51Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg52Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg52Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg53Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg53Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# add session
tmp <- bills$sessions
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
tmp <- ddply(tmp, .(week), mutate, ndipses=sum(ddip), nsenses=sum(dsen)); tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$ddip <- tmp$dsen <- NULL
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
weekRepUrg[is.na(weekRepUrg)] <- 0 # fill zeroes
#
# WHEN DATE HAD NO SESSION, ADD NUMBERS TO NEXT SESSION
tmp <- weekRepUrg
# dip
sel <- which(tmp$ndipses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$ndipses>0])) # closest next session date
    tmp[j, grep("Dip", colnames(tmp))] <- tmp[j, grep("Dip", colnames(tmp))] + tmp[i, grep("Dip", colnames(tmp))] # add data to that row
    tmp[i, grep("Dip", colnames(tmp))] <- 0 # erase it from week with no session
}
# sen
sel <- which(tmp$nsenses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug 
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$nsenses>0])) # closest next session date
    tmp[j, grep("Sen", colnames(tmp))] <- tmp[j, grep("Sen", colnames(tmp))] + tmp[i, grep("Sen", colnames(tmp))] # add data to that row
    tmp[i, grep("Sen", colnames(tmp))] <- 0 # erase it from week with no session
}
#
weekRepUrg <- tmp
#
# Add president's maj status in chamber
weekRepUrg$dmajDip <- weekRepUrg$dmajSen <- 0
# sen
tmp <- dmy(c("11-03-1990", "22-01-1999", "11-03-2000", "11-01-2002", "27-01-2005", "30-08-2005", "11-03-2006", "11-03-2010") )
tmp <- year(tmp) + week(tmp)/100
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dmajSen[sel] <- 1 
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[6] & weekRepUrg$week < tmp[7]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[7] & weekRepUrg$week < tmp[8]); weekRepUrg$dmajSen[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[8] & weekRepUrg$week < tmp[9]); weekRepUrg$dmajSen[sel] <- 0
#
# dip: always maj=1 (2010-14 50%, coded 1)
weekRepUrg$dmajDip <- 1
#
# weeks to end of pdtl term
tmp <- dmy(c("11-03-1994", "11-03-2000", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$pterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$pterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$pterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$pterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$pterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$pterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of dip term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dterm[sel] <- round((tmp[6] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of sen term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2006", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$sterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$sterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$sterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$sterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$sterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
#
# legislature dummies (periodo)
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dleg90 <- weekRepUrg$dleg94 <- weekRepUrg$dleg98 <- weekRepUrg$dleg02 <- weekRepUrg$dleg06 <- weekRepUrg$dleg10 <- 0
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dleg90[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dleg94[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dleg98[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dleg02[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dleg06[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dleg10[sel] <- 1
#
# plug to data object
RepDataNegBin$weeklyReportsAndUrgenciasToMociones <- weekRepUrg
#
###################################################################################
###################################################################################
## PREPARE WEEKLY REPORTS FROM ANY COMMITTEE WITH WEEKLY URGENCIES TO ALL BILLS  ##
###################################################################################
###################################################################################
# dip and sen separate
dipRep <- allRep[allRep$chamber=="dip",]
senRep <- allRep[allRep$chamber=="sen",]
#
# add week
dipRep$week <- year(dipRep$date)+week(dipRep$date)/100
senRep$week <- year(senRep$date)+week(senRep$date)/100
#                                                             ################
# consolidate weekly reports                                  <---  OJO   ####
#                                                             ################
tmp <- ddply(dipRep, .(week), mutate, nrepMenDip=sum(dmensaje     ), nrepMocDip=sum(dmocion     )) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
dipRep <- tmp
tmp <- ddply(senRep, .(week), mutate, nrepMenSen=sum(dmensaje     ), nrepMocSen=sum(dmocion     )) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$dmocion <- tmp$dmensaje <- tmp$bol <- tmp$chamber <- tmp$date <- tmp$comm <- tmp$dHda <- NULL
senRep <- tmp
#
# object with all weeks in period
tmp <- data.frame(date = seq(from = dmy("11-03-1990"), to = dmy("10-03-2014"), by = 'weeks'))
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
weekRepUrg <- tmp
#
weekRepUrg <- merge(x = weekRepUrg, y = dipRep, by = "week", all=TRUE)
weekRepUrg <- merge(x = weekRepUrg, y = senRep, by = "week", all=TRUE)
#
# Add weekly urgencias
# old tmp1 <- allUrg
### added 22Aug2015: problem here, allUrg is now a list of messages and chains... need to select one before continuing...
tmp1 <- allUrg$messages
tmp1$week <- year(tmp1$on) + week(tmp1$on)/100
# distinguish mensaje urgencia from mocion urgencias
tmp1$dmensaje <- NA
tmp1$dHda <- NA
for (i in 1:nrow(tmp1)){
    message(sprintf("loop %s of %s", i, nrow(tmp1)))
    j <- which(bills$info$bol==tmp1$bol[i])
    tmp1$dmensaje[i] <- bills$info$dmensaje[j]
    tmp1$dHda[i] <- bills$info$drefHda[j] # bill was referred to Hacienda committee
}
#                                                ################################################################
tmp1$count <- 1                                  ###### COUNT URGENCIAS ON BOTH MENSAJES AND MOCIONES!!!  #######
#                                                ################################################################
#
# aggregate dip discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IF GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg1Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg1Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg2Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen suma and plug into week object
tmp <- tmp1[tmp1$typeN==2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg2Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg3Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen simple and plug into week object
tmp <- tmp1[tmp1$typeN==3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg3Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset to discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg41Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==4.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg41Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg42Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset suma and plug into week object
tmp <- tmp1[tmp1$typeN==4.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg42Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg43Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen reset simple and plug into week object
tmp <- tmp1[tmp1$typeN==4.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg43Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg51Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire discusión inmediata and plug into week object
tmp <- tmp1[tmp1$typeN==5.1 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg51Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg52Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire suma and plug into week object
tmp <- tmp1[tmp1$typeN==5.2 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg52Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# aggregate dip retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="dip"),] # | tmp1$tramite=="conf"),] ## LEAVING CONF OUT OF PICTURE; UNCLEAR IT GOES 2 COMM IN CASE OF URGENCY
tmp <- ddply(tmp, .(week), mutate, nurg53Dip=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
# aggregate sen retire simple and plug into week object
tmp <- tmp1[tmp1$typeN==5.3 & (tmp1$tramite=="sen"),] # | tmp1$tramite=="conf"),]
tmp <- ddply(tmp, .(week), mutate, nurg53Sen=sum(count)) ; tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp <- tmp[,grep("week|nurg", colnames(tmp))]
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
# add session
tmp <- bills$sessions
tmp$week <- year(tmp$date) + week(tmp$date)/100; tmp$date <- NULL
tmp <- ddply(tmp, .(week), mutate, ndipses=sum(ddip), nsenses=sum(dsen)); tmp <- tmp[duplicated(tmp$week)==FALSE,]; tmp$ddip <- tmp$dsen <- NULL
weekRepUrg <- merge(x = weekRepUrg, y = tmp, by = "week", all=TRUE)
#
weekRepUrg[is.na(weekRepUrg)] <- 0 # fill zeroes
#
# WHEN DATE HAD NO SESSION, ADD NUMBERS TO NEXT SESSION
tmp <- weekRepUrg
# dip
sel <- which(tmp$ndipses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$ndipses>0])) # closest next session date
    tmp[j, grep("Dip", colnames(tmp))] <- tmp[j, grep("Dip", colnames(tmp))] + tmp[i, grep("Dip", colnames(tmp))] # add data to that row
    tmp[i, grep("Dip", colnames(tmp))] <- 0 # erase it from week with no session
}
# sen
sel <- which(tmp$nsenses==0) # select weeks with no session in dip
for (i in sel){
    #i <- sel[2] # debug 
    tmp1 <- tmp$week - tmp$week[i] # difference from week of interest to other weeks
    j <- which(tmp1==min(tmp1[tmp1>0 & tmp$nsenses>0])) # closest next session date
    tmp[j, grep("Sen", colnames(tmp))] <- tmp[j, grep("Sen", colnames(tmp))] + tmp[i, grep("Sen", colnames(tmp))] # add data to that row
    tmp[i, grep("Sen", colnames(tmp))] <- 0 # erase it from week with no session
}
#
weekRepUrg <- tmp
#
# Add president's maj status in chamber
weekRepUrg$dmajDip <- weekRepUrg$dmajSen <- 0
# sen
tmp <- dmy(c("11-03-1990", "22-01-1999", "11-03-2000", "11-01-2002", "27-01-2005", "30-08-2005", "11-03-2006", "11-03-2010") )
tmp <- year(tmp) + week(tmp)/100
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dmajSen[sel] <- 1 
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dmajSen[sel] <- 0
sel <- which(weekRepUrg$week >= tmp[6] & weekRepUrg$week < tmp[7]); weekRepUrg$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(weekRepUrg$week >= tmp[7] & weekRepUrg$week < tmp[8]); weekRepUrg$dmajSen[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[8] & weekRepUrg$week < tmp[9]); weekRepUrg$dmajSen[sel] <- 0
#
# dip: always maj=1 (2010-14 50%, coded 1)
weekRepUrg$dmajDip <- 1
#
# weeks to end of pdtl term
tmp <- dmy(c("11-03-1994", "11-03-2000", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$pterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$pterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$pterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$pterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 6, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$pterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$pterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of dip term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dterm[sel] <- round((tmp[5] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dterm[sel] <- round((tmp[6] - weekRepUrg$week[sel]) * 100 / 4, digits = 0)
#
# weeks to end of sen term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2006", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$sterm <- NA
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$sterm[sel] <- round((tmp[1] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$sterm[sel] <- round((tmp[2] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$sterm[sel] <- round((tmp[3] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$sterm[sel] <- round((tmp[4] - weekRepUrg$week[sel]) * 100 / 8, digits = 0)
#
# legislature dummies (periodo)
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
tmp <- year(tmp) + week(tmp)/100
weekRepUrg$dleg90 <- weekRepUrg$dleg94 <- weekRepUrg$dleg98 <- weekRepUrg$dleg02 <- weekRepUrg$dleg06 <- weekRepUrg$dleg10 <- 0
sel <- which(                            weekRepUrg$week < tmp[1]); weekRepUrg$dleg90[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[1] & weekRepUrg$week < tmp[2]); weekRepUrg$dleg94[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[2] & weekRepUrg$week < tmp[3]); weekRepUrg$dleg98[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[3] & weekRepUrg$week < tmp[4]); weekRepUrg$dleg02[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[4] & weekRepUrg$week < tmp[5]); weekRepUrg$dleg06[sel] <- 1
sel <- which(weekRepUrg$week >= tmp[5] & weekRepUrg$week < tmp[6]); weekRepUrg$dleg10[sel] <- 1
#
# plug to data object
RepDataNegBin$weeklyReportsAndUrgenciasToAllBills <- weekRepUrg
#
# keep source to re-manipulate
RepDataNegBin$weekRepUrg <- weekRepUrg
#
summary(RepDataNegBin)
#
rm(i, dipRep, j, sel, senRep, tmp, tmp1, tmpRep,weekRepUrg)
################################################################
##  COMMITTEE REPORT DATA PREP USED IN REGRESSIONS ENDS HERE  ##
################################################################

# incorpora importancia a bills
dim(bills$info)
dim(importancia.vale)
bills$importancia <- as.data.frame(importancia.vale)
summary(bills)
rm(importancia.vale)

tmp <- bills$info
sel <- which(tmp$drefSalud==1 & tmp$drefHda==1)
grep("[Mm]unicip", tmp$titulo[sel])
tmp$bol[sel[23]]

### summarize weekly reports
#tmp <- RepDataNegBin$weeklyReportsAndUrgenciasToAllBills
tmp <- RepDataNegBin$weeklyReportsAndUrgenciasToAllBills
colnames(tmp)
date1 <- dmy("10-03-1998")
date2 <- dmy("10-03-2014")
sel <- which(tmp$week >= year(date1) + week(date1)/100  & tmp$week < year(date2) + week(date2)/100)
round(table(tmp$nrepMenDip[sel] + tmp$nrepMocDip[sel])/length(sel),2)
median(tmp$nrepMenDip[sel] + tmp$nrepMocDip[sel])
mad(tmp$nrepMenDip[sel] + tmp$nrepMocDip[sel])    # median absolute deviation
rm(tmp, date1, date2)

# add presidential approval data (semester measures approx, so midpoints taken to compute from-to intervals)
from <- c("01/01/1998", "03/17/1998", "11/15/1998", "07/16/1999", "05/01/2000", "03/02/2001", "09/16/2001", "04/01/2002", "09/15/2002", "03/17/2003", "09/15/2003", "03/16/2004", "09/15/2004", "03/17/2005", "08/31/2005", "03/02/2006", "09/15/2006", "03/02/2007", "08/31/2007", "03/01/2008", "08/31/2008", "03/02/2009", "07/01/2009", "08/31/2009", "02/14/2010", "09/15/2010", "03/17/2011", "09/15/2011", "01/31/2012", "06/01/2012", "10/01/2012", "04/01/2013", "08/31/2013")
to <- c("03/17/1998", "11/15/1998", "07/16/1999", "05/01/2000", "03/02/2001", "09/16/2001", "04/01/2002", "09/15/2002", "03/17/2003", "09/15/2003", "03/16/2004", "09/15/2004", "03/17/2005", "08/31/2005", "03/02/2006", "09/15/2006", "03/02/2007", "08/31/2007", "03/01/2008", "08/31/2008", "03/02/2009", "07/01/2009", "08/31/2009", "02/14/2010", "09/15/2010", "03/17/2011", "09/15/2011", "01/31/2012", "06/01/2012", "10/01/2012", "04/01/2013", "08/31/2013", "03/21/2014")
from <- mdy(from); to <- mdy(to)
approv <- data.frame(from=from, to=to); rm(from,to)
approv$ap <- c(33.3, 37, 32, 28, 48.6, 40.6, 44.4, 42.5, 41.1, 46, 47.4, 57.5, 60.7, 61.2, 58.9, 45.8, 51.7, 40.9, 38.8, 39.6, 43, 66.5, 72.3, 77.6, 45, 44.3, 26.3, 22.8, 23.6, 27.4, 31.4, 31.5, 34.2)
approv$dis <- c(37.1, 36, 40, 44, 26.3, 36.4, 31.2, 29.4, 30.4, 26.2, 23.7, 21.7, 17.6, 18, 22.7, 30.5, 31.2, 40.7, 42.1, 42.9, 38.1, 17.6, 14.5, 11.3, 28.7, 33.6, 52.6, 62, 59.2, 52.3, 50.6, 43.9, 46.2)
approv$net <- approv$ap - approv$dis
approv$fromWeek <- year(approv$from) + week(approv$from)/100
approv$toWeek <- year(approv$to) + week(approv$to)/100
tail(approv)

# paste approval data to weekly urgency data
RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMensajesInHaciendaComm$netApprov <- NA
for (i in 1:nrow(approv)){
    #i <- 1 # debug
    sel <- which(RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMensajesInHaciendaComm$week >= approv$fromWeek[i] & RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMensajesInHaciendaComm$week < approv$toWeek[i])
    RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMensajesInHaciendaComm$netApprov[sel] <- approv$net[i]
}
#
RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMocionesInHaciendaComm$netApprov <- NA
for (i in 1:nrow(approv)){
    #i <- 1 # debug
    sel <- which(RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMocionesInHaciendaComm$week >= approv$fromWeek[i] & RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMocionesInHaciendaComm$week < approv$toWeek[i])
    RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMocionesInHaciendaComm$netApprov[sel] <- approv$net[i]
}
#
RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToAllBillsInHaciendaComm$netApprov <- NA
for (i in 1:nrow(approv)){
    #i <- 1 # debug
    sel <- which(RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToAllBillsInHaciendaComm$week >= approv$fromWeek[i] & RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToAllBillsInHaciendaComm$week < approv$toWeek[i])
    RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToAllBillsInHaciendaComm$netApprov[sel] <- approv$net[i]
}
#
RepDataNegBin$weeklyReportsAndUrgenciasToMensajes$netApprov <- NA
for (i in 1:nrow(approv)){
    #i <- 1 # debug
    sel <- which(RepDataNegBin$weeklyReportsAndUrgenciasToMensajes$week >= approv$fromWeek[i] & RepDataNegBin$weeklyReportsAndUrgenciasToMensajes$week < approv$toWeek[i])
    RepDataNegBin$weeklyReportsAndUrgenciasToMensajes$netApprov[sel] <- approv$net[i]
}
#
RepDataNegBin$weeklyReportsAndUrgenciasToMociones$netApprov <- NA
for (i in 1:nrow(approv)){
    #i <- 1 # debug
    sel <- which(RepDataNegBin$weeklyReportsAndUrgenciasToMociones$week >= approv$fromWeek[i] & RepDataNegBin$weeklyReportsAndUrgenciasToMociones$week < approv$toWeek[i])
    RepDataNegBin$weeklyReportsAndUrgenciasToMociones$netApprov[sel] <- approv$net[i]
}
#
RepDataNegBin$weeklyReportsAndUrgenciasToAllBills$netApprov <- NA
for (i in 1:nrow(approv)){
    #i <- 1 # debug
    sel <- which(RepDataNegBin$weeklyReportsAndUrgenciasToAllBills$week >= approv$fromWeek[i] & RepDataNegBin$weeklyReportsAndUrgenciasToAllBills$week < approv$toWeek[i])
    RepDataNegBin$weeklyReportsAndUrgenciasToAllBills$netApprov[sel] <- approv$net[i]
}
#
# paste approval data to bills' info
bills$info$netApprov <- NA
for (i in 1:nrow(bills$info)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(bills$info)))
    sel <- which(bills$info$dateIn >= approv$from[i] & bills$info$dateIn < approv$to[i])
    bills$info$netApprov[sel] <- approv$net[i]
}
#
# paste approval data to urgency chains
allUrg$chains$netApprov <- NA
for (i in 1:nrow(allUrg$chains)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(allUrg$chains)))
    sel <- which(allUrg$chains$on >= approv$from[i] & allUrg$chains$on < approv$to[i])
    allUrg$chains$netApprov[sel] <- approv$net[i]
}

#############################################################################################
## Regression of Hda Reports to Exec bills on Urgencies to Exec bills ref to Hda  DIP      ##
## line of analysis I dropped early on---kept because some codings could still be relevant ##
#############################################################################################
library(DataCombine) # easy lags
library(MASS)        # neg bin reg
#
msg <- "Regression of Hda Reports to Exec bills on Urgencies to Exec bills ref to Hda  DIP"
dat <- RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMensajesInHaciendaComm
#
# re-categorize urgencias
dat$nNowDip <- dat$nurg1Dip
dat$n2wkDip <- dat$nurg2Dip
dat$n4wkDip <- dat$nurg3Dip
dat$nShortenDip <- dat$nurg41Dip
dat$nExtendDip <- dat$nurg42Dip + dat$nurg43Dip
dat$nRetireDip <- dat$nurg51Dip + dat$nurg52Dip + dat$nurg53Dip 
dat$nNowSen <- dat$nurg1Sen
dat$n2wkSen <- dat$nurg2Sen
dat$n4wkSen <- dat$nurg3Sen
dat$nShortenSen <- dat$nurg41Sen
dat$nExtendSen <- dat$nurg42Sen + dat$nurg43Sen
dat$nRetireSen <- dat$nurg51Sen + dat$nurg52Sen + dat$nurg53Sen
#
# remove original variables
dat$nurg1Dip <- dat$nurg2Dip <- dat$nurg2Dip <- NULL
dat$nurg1Sen <- dat$nurg2Sen <- dat$nurg2Sen <- NULL
dat$nurg3Dip <- dat$nurg3Sen <- NULL
dat$nurg41Dip <- dat$nurg42Dip <- dat$nurg43Dip <- dat$nurg51Dip <- dat$nurg52Dip <- dat$nurg53Dip <- NULL
dat$nurg41Sen <- dat$nurg42Sen <- dat$nurg43Sen <- dat$nurg51Sen <- dat$nurg52Sen <- dat$nurg53Sen <- NULL
#
# add post 2010 dummy (when urgencias relaxed)
dat$d2010on <- 0
sel <- which(dat$week > (year(dmy("11-07-2010")) + week(dmy("11-07-2010"))/100))
dat$d2010on[sel] <- 1
#
#                   #############
# drop sen or dip   <--- OJO  ###
#                   #############
dat <- dat[,-grep("[Ss]en", colnames(dat))]  # drop sen
#dat <- dat[,-grep("[Dd]ip", colnames(dat))]  # drop dip
# simpify remaining names
colnames(dat) <- sub("([Dd]ip|[Ss]en)", "", colnames(dat))
#
# drop weeks with no session
dat <- dat[-which(dat$nses==0),]
#
# lags
dat <- dat[order(dat$week),] # verify sorted before lags
dat <- slide(dat, Var = "nNow",     slideBy = -1)
dat <- slide(dat, Var = "n2wk",     slideBy = -1)
dat <- slide(dat, Var = "n4wk",     slideBy = -1)
dat <- slide(dat, Var = "nExtend",  slideBy = -1)
dat <- slide(dat, Var = "nShorten", slideBy = -1)
dat <- slide(dat, Var = "nRetire",  slideBy = -1)
dat <- slide(dat, Var = "nNow",     slideBy = -2)
dat <- slide(dat, Var = "n2wk",     slideBy = -2)
dat <- slide(dat, Var = "n4wk",     slideBy = -2)
dat <- slide(dat, Var = "nExtend",  slideBy = -2)
dat <- slide(dat, Var = "nShorten", slideBy = -2)
dat <- slide(dat, Var = "nRetire",  slideBy = -2)
dat <- slide(dat, Var = "nNow",     slideBy = -3)
dat <- slide(dat, Var = "n2wk",     slideBy = -3)
dat <- slide(dat, Var = "n4wk",     slideBy = -3)
dat <- slide(dat, Var = "nExtend",  slideBy = -3)
dat <- slide(dat, Var = "nShorten", slideBy = -3)
dat <- slide(dat, Var = "nRetire",  slideBy = -3)
dat <- slide(dat, Var = "nNow",     slideBy = -4)
dat <- slide(dat, Var = "n2wk",     slideBy = -4)
dat <- slide(dat, Var = "n4wk",     slideBy = -4)
dat <- slide(dat, Var = "nExtend",  slideBy = -4)
dat <- slide(dat, Var = "nShorten", slideBy = -4)
dat <- slide(dat, Var = "nRetire",  slideBy = -4)
# replace "-" in lag names (glm doesnt like it)
colnames(dat) <- sub(pattern = "-", replacement = "l", colnames(dat))
#
#                                                         ################
# drop weeks before and after these dates in regression   <--- OJO   #####
#                                                         ################
sel <- which(dat$week < year(dmy("11-03-2006")) + week(dmy("11-03-2006"))/100
           | dat$week > year(dmy("10-03-2014")) + week(dmy("10-03-2014"))/100 )
dat <- dat[-sel,]
#dat.bak <- dat
#dat <- dat.bak
#
# ESTE JALA PARA EL EJECUTIVO EN DIP
fit <- glm.nb(nrepMen ~ nNow      + nNowl1     + nNowl2     
                                  + n2wkl1     + n2wkl2     + n2wkl3   
                                               + n4wkl2     + n4wkl3   + n4wkl4   
                      + nShorten  + nShortenl1 + nShortenl2 
#                                               + nExtendl2  + nExtendl3 + nExtendl4 
#                                  + nRetirel1  + nRetirel2  + nRetirel3 
#                      + dmaj
                      + dterm
                      + netApprov
                      + dleg10
                        , data=dat)
#
summary.glm(fit)$coefficients
#summary.glm(fit)
message(msg); rm(msg)
data.frame( coef=ifelse(coef(fit) > 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "+ ",
                 ifelse(coef(fit) > 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "++",
                 ifelse(coef(fit) < 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "- ",
                 ifelse(coef(fit) < 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "--",
            ". ")))) )
#
# SPACE HERE TO RERUN NEGBIN
fit11 <- fit

#
############################################################################################
## Regression of All Reports to Exec bills on Urgencies to Exec bills ref anywhere  DIP   ##
############################################################################################
msg <- "Regression of All Reports to Exec bills on Urgencies to Exec bills ref anywhere  DIP"
dat <- RepDataNegBin$weeklyReportsAndUrgenciasToMensajes
#
# re-categorize urgencias
dat$nNowDip <- dat$nurg1Dip
dat$n2wkDip <- dat$nurg2Dip
dat$n4wkDip <- dat$nurg3Dip
dat$nShortenDip <- dat$nurg41Dip
dat$nExtendDip <- dat$nurg42Dip + dat$nurg43Dip
dat$nRetireDip <- dat$nurg51Dip + dat$nurg52Dip + dat$nurg53Dip 
dat$nNowSen <- dat$nurg1Sen
dat$n2wkSen <- dat$nurg2Sen
dat$n4wkSen <- dat$nurg3Sen
dat$nShortenSen <- dat$nurg41Sen
dat$nExtendSen <- dat$nurg42Sen + dat$nurg43Sen
dat$nRetireSen <- dat$nurg51Sen + dat$nurg52Sen + dat$nurg53Sen
#
# remove original variables
dat$nurg1Dip <- dat$nurg2Dip <- dat$nurg2Dip <- NULL
dat$nurg1Sen <- dat$nurg2Sen <- dat$nurg2Sen <- NULL
dat$nurg3Dip <- dat$nurg3Sen <- NULL
dat$nurg41Dip <- dat$nurg42Dip <- dat$nurg43Dip <- dat$nurg51Dip <- dat$nurg52Dip <- dat$nurg53Dip <- NULL
dat$nurg41Sen <- dat$nurg42Sen <- dat$nurg43Sen <- dat$nurg51Sen <- dat$nurg52Sen <- dat$nurg53Sen <- NULL
#
# add post 2010 dummy (when urgencias relaxed)
dat$d2010on <- 0
sel <- which(dat$week > (year(dmy("11-07-2010")) + week(dmy("11-07-2010"))/100))
dat$d2010on[sel] <- 1
#
#                   #############
# drop sen or dip   <--- OJO  ###
#                   #############
dat <- dat[,-grep("[Ss]en", colnames(dat))]  # drop sen
#dat <- dat[,-grep("[Dd]ip", colnames(dat))]  # drop dip
# simpify remaining names
colnames(dat) <- sub("([Dd]ip|[Ss]en)", "", colnames(dat))
#
# drop weeks with no session
dat <- dat[-which(dat$nses==0),]
#
# lags
dat <- dat[order(dat$week),] # verify sorted before lags
dat <- slide(dat, Var = "nNow",     slideBy = -1)
dat <- slide(dat, Var = "n2wk",     slideBy = -1)
dat <- slide(dat, Var = "n4wk",     slideBy = -1)
dat <- slide(dat, Var = "nExtend",  slideBy = -1)
dat <- slide(dat, Var = "nShorten", slideBy = -1)
dat <- slide(dat, Var = "nRetire",  slideBy = -1)
dat <- slide(dat, Var = "nNow",     slideBy = -2)
dat <- slide(dat, Var = "n2wk",     slideBy = -2)
dat <- slide(dat, Var = "n4wk",     slideBy = -2)
dat <- slide(dat, Var = "nExtend",  slideBy = -2)
dat <- slide(dat, Var = "nShorten", slideBy = -2)
dat <- slide(dat, Var = "nRetire",  slideBy = -2)
dat <- slide(dat, Var = "nNow",     slideBy = -3)
dat <- slide(dat, Var = "n2wk",     slideBy = -3)
dat <- slide(dat, Var = "n4wk",     slideBy = -3)
dat <- slide(dat, Var = "nExtend",  slideBy = -3)
dat <- slide(dat, Var = "nShorten", slideBy = -3)
dat <- slide(dat, Var = "nRetire",  slideBy = -3)
dat <- slide(dat, Var = "nNow",     slideBy = -4)
dat <- slide(dat, Var = "n2wk",     slideBy = -4)
dat <- slide(dat, Var = "n4wk",     slideBy = -4)
dat <- slide(dat, Var = "nExtend",  slideBy = -4)
dat <- slide(dat, Var = "nShorten", slideBy = -4)
dat <- slide(dat, Var = "nRetire",  slideBy = -4)
# replace "-" in lag names (glm doesnt like it)
colnames(dat) <- sub(pattern = "-", replacement = "l", colnames(dat))
#
#                                                         ################
# drop weeks before and after these dates in regression   <--- OJO   #####
#                                                         ################
sel <- which(dat$week < year(dmy("11-03-2006")) + week(dmy("11-03-2006"))/100
           | dat$week > year(dmy("10-03-2014")) + week(dmy("10-03-2014"))/100 )
dat <- dat[-sel,]
#dat.bak <- dat
#dat <- dat.bak
#
# ESTE JALA PARA EL EJECUTIVO EN DIP
fit <- glm.nb(nrepMen ~ nNow      + nNowl1     + nNowl2     
                                  + n2wkl1     + n2wkl2     + n2wkl3   
                                               + n4wkl2     + n4wkl3   + n4wkl4   
                      + nShorten  + nShortenl1 + nShortenl2 
#                                               + nExtendl2  + nExtendl3 + nExtendl4 
#                                  + nRetirel1  + nRetirel2  + nRetirel3 
#                      + dmaj
                      + dterm
                      + netApprov
                      + dleg10
                        , data=dat)
#
summary.glm(fit)$coefficients
#summary.glm(fit)
message(msg); rm(msg)
data.frame( coef=ifelse(coef(fit) > 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "+ ",
                 ifelse(coef(fit) > 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "++",
                 ifelse(coef(fit) < 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "- ",
                 ifelse(coef(fit) < 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "--",
            ". ")))) )
#
# SPACE HERE TO RERUN NEGBIN
fit15 <- fit

#
#########################################################################################
## Regression of Hda Reports to Leg bills on Urgencies to Exec bills ref to Hda  DIP   ##
#########################################################################################
msg <- "Regression of Hda Reports to Leg bills on Urgencies to Exec bills ref to Hda  DIP"
dat <- RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMensajesInHaciendaComm
#
# re-categorize urgencias
dat$nNowDip <- dat$nurg1Dip
dat$n2wkDip <- dat$nurg2Dip
dat$n4wkDip <- dat$nurg3Dip
dat$nShortenDip <- dat$nurg41Dip
dat$nExtendDip <- dat$nurg42Dip + dat$nurg43Dip
dat$nRetireDip <- dat$nurg51Dip + dat$nurg52Dip + dat$nurg53Dip 
dat$nNowSen <- dat$nurg1Sen
dat$n2wkSen <- dat$nurg2Sen
dat$n4wkSen <- dat$nurg3Sen
dat$nShortenSen <- dat$nurg41Sen
dat$nExtendSen <- dat$nurg42Sen + dat$nurg43Sen
dat$nRetireSen <- dat$nurg51Sen + dat$nurg52Sen + dat$nurg53Sen
#
# remove original variables
dat$nurg1Dip <- dat$nurg2Dip <- dat$nurg2Dip <- NULL
dat$nurg1Sen <- dat$nurg2Sen <- dat$nurg2Sen <- NULL
dat$nurg3Dip <- dat$nurg3Sen <- NULL
dat$nurg41Dip <- dat$nurg42Dip <- dat$nurg43Dip <- dat$nurg51Dip <- dat$nurg52Dip <- dat$nurg53Dip <- NULL
dat$nurg41Sen <- dat$nurg42Sen <- dat$nurg43Sen <- dat$nurg51Sen <- dat$nurg52Sen <- dat$nurg53Sen <- NULL
#
# add post 2010 dummy (when urgencias relaxed)
dat$d2010on <- 0
sel <- which(dat$week > (year(dmy("11-07-2010")) + week(dmy("11-07-2010"))/100))
dat$d2010on[sel] <- 1
#
#                   #############
# drop sen or dip   <--- OJO  ###
#                   #############
dat <- dat[,-grep("[Ss]en", colnames(dat))]  # drop sen
#dat <- dat[,-grep("[Dd]ip", colnames(dat))]  # drop dip
# simpify remaining names
colnames(dat) <- sub("([Dd]ip|[Ss]en)", "", colnames(dat))
#
# drop weeks with no session
dat <- dat[-which(dat$nses==0),]
#
# lags
dat <- dat[order(dat$week),] # verify sorted before lags
dat <- slide(dat, Var = "nNow",     slideBy = -1)
dat <- slide(dat, Var = "n2wk",     slideBy = -1)
dat <- slide(dat, Var = "n4wk",     slideBy = -1)
dat <- slide(dat, Var = "nExtend",  slideBy = -1)
dat <- slide(dat, Var = "nShorten", slideBy = -1)
dat <- slide(dat, Var = "nRetire",  slideBy = -1)
dat <- slide(dat, Var = "nNow",     slideBy = -2)
dat <- slide(dat, Var = "n2wk",     slideBy = -2)
dat <- slide(dat, Var = "n4wk",     slideBy = -2)
dat <- slide(dat, Var = "nExtend",  slideBy = -2)
dat <- slide(dat, Var = "nShorten", slideBy = -2)
dat <- slide(dat, Var = "nRetire",  slideBy = -2)
dat <- slide(dat, Var = "nNow",     slideBy = -3)
dat <- slide(dat, Var = "n2wk",     slideBy = -3)
dat <- slide(dat, Var = "n4wk",     slideBy = -3)
dat <- slide(dat, Var = "nExtend",  slideBy = -3)
dat <- slide(dat, Var = "nShorten", slideBy = -3)
dat <- slide(dat, Var = "nRetire",  slideBy = -3)
dat <- slide(dat, Var = "nNow",     slideBy = -4)
dat <- slide(dat, Var = "n2wk",     slideBy = -4)
dat <- slide(dat, Var = "n4wk",     slideBy = -4)
dat <- slide(dat, Var = "nExtend",  slideBy = -4)
dat <- slide(dat, Var = "nShorten", slideBy = -4)
dat <- slide(dat, Var = "nRetire",  slideBy = -4)
# replace "-" in lag names (glm doesnt like it)
colnames(dat) <- sub(pattern = "-", replacement = "l", colnames(dat))
#
#                                                         ################
# drop weeks before and after these dates in regression   <--- OJO   #####
#                                                         ################
sel <- which(dat$week < year(dmy("11-03-2006")) + week(dmy("11-03-2006"))/100
           | dat$week > year(dmy("10-03-2014")) + week(dmy("10-03-2014"))/100 )
dat <- dat[-sel,]

#dat.bak <- dat
#dat <- dat.bak
#
#dat$dtermR <- scale(dat$dterm) # rescale/center continuous variables to aid identification
fit <- glm.nb(nrepMoc ~ nNow      + nNowl1     #+ nNowl2     
                      + n2wk      + n2wkl1     + n2wkl2     #+ n2wkl3   
#                      + n4wkl1   
                                  + nShortenl1 #+ nShortenl2 
#                                               + nExtendl2  + nExtendl3 + nExtendl4 
#                                  + nRetirel1  + nRetirel2  + nRetirel3 
#                      + dmaj
                      + dterm
#                      + netApprov
#                      + dleg10
                        , data=dat)
#
summary.glm(fit)$coefficients
#summary.glm(fit)
message(msg); rm(msg)
data.frame( coef=ifelse(coef(fit) > 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "+ ",
                 ifelse(coef(fit) > 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "++",
                 ifelse(coef(fit) < 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "- ",
                 ifelse(coef(fit) < 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "--",
            ". ")))) )
#
# SPACE HERE TO RERUN NEGBIN
fit12 <- fit

#
###########################################################################################
## Regression of All Reports to Leg bills on Urgencies to Exec bills ref anywhere  DIP   ##
###########################################################################################
msg <- "Regression of All Reports to Leg bills on Urgencies to Exec bills ref anywhere  DIP"
dat <- RepDataNegBin$weeklyReportsAndUrgenciasToMensajes
#
# re-categorize urgencias
dat$nNowDip <- dat$nurg1Dip
dat$n2wkDip <- dat$nurg2Dip
dat$n4wkDip <- dat$nurg3Dip
dat$nShortenDip <- dat$nurg41Dip
dat$nExtendDip <- dat$nurg42Dip + dat$nurg43Dip
dat$nRetireDip <- dat$nurg51Dip + dat$nurg52Dip + dat$nurg53Dip 
dat$nNowSen <- dat$nurg1Sen
dat$n2wkSen <- dat$nurg2Sen
dat$n4wkSen <- dat$nurg3Sen
dat$nShortenSen <- dat$nurg41Sen
dat$nExtendSen <- dat$nurg42Sen + dat$nurg43Sen
dat$nRetireSen <- dat$nurg51Sen + dat$nurg52Sen + dat$nurg53Sen
#
# remove original variables
dat$nurg1Dip <- dat$nurg2Dip <- dat$nurg2Dip <- NULL
dat$nurg1Sen <- dat$nurg2Sen <- dat$nurg2Sen <- NULL
dat$nurg3Dip <- dat$nurg3Sen <- NULL
dat$nurg41Dip <- dat$nurg42Dip <- dat$nurg43Dip <- dat$nurg51Dip <- dat$nurg52Dip <- dat$nurg53Dip <- NULL
dat$nurg41Sen <- dat$nurg42Sen <- dat$nurg43Sen <- dat$nurg51Sen <- dat$nurg52Sen <- dat$nurg53Sen <- NULL
#
# add post 2010 dummy (when urgencias relaxed)
dat$d2010on <- 0
sel <- which(dat$week > (year(dmy("11-07-2010")) + week(dmy("11-07-2010"))/100))
dat$d2010on[sel] <- 1
#
#                   #############
# drop sen or dip   <--- OJO  ###
#                   #############
dat <- dat[,-grep("[Ss]en", colnames(dat))]  # drop sen
#dat <- dat[,-grep("[Dd]ip", colnames(dat))]  # drop dip
# simpify remaining names
colnames(dat) <- sub("([Dd]ip|[Ss]en)", "", colnames(dat))
#
# drop weeks with no session
dat <- dat[-which(dat$nses==0),]
#
# lags
dat <- dat[order(dat$week),] # verify sorted before lags
dat <- slide(dat, Var = "nNow",     slideBy = -1)
dat <- slide(dat, Var = "n2wk",     slideBy = -1)
dat <- slide(dat, Var = "n4wk",     slideBy = -1)
dat <- slide(dat, Var = "nExtend",  slideBy = -1)
dat <- slide(dat, Var = "nShorten", slideBy = -1)
dat <- slide(dat, Var = "nRetire",  slideBy = -1)
dat <- slide(dat, Var = "nNow",     slideBy = -2)
dat <- slide(dat, Var = "n2wk",     slideBy = -2)
dat <- slide(dat, Var = "n4wk",     slideBy = -2)
dat <- slide(dat, Var = "nExtend",  slideBy = -2)
dat <- slide(dat, Var = "nShorten", slideBy = -2)
dat <- slide(dat, Var = "nRetire",  slideBy = -2)
dat <- slide(dat, Var = "nNow",     slideBy = -3)
dat <- slide(dat, Var = "n2wk",     slideBy = -3)
dat <- slide(dat, Var = "n4wk",     slideBy = -3)
dat <- slide(dat, Var = "nExtend",  slideBy = -3)
dat <- slide(dat, Var = "nShorten", slideBy = -3)
dat <- slide(dat, Var = "nRetire",  slideBy = -3)
dat <- slide(dat, Var = "nNow",     slideBy = -4)
dat <- slide(dat, Var = "n2wk",     slideBy = -4)
dat <- slide(dat, Var = "n4wk",     slideBy = -4)
dat <- slide(dat, Var = "nExtend",  slideBy = -4)
dat <- slide(dat, Var = "nShorten", slideBy = -4)
dat <- slide(dat, Var = "nRetire",  slideBy = -4)
# replace "-" in lag names (glm doesnt like it)
colnames(dat) <- sub(pattern = "-", replacement = "l", colnames(dat))
#
#                                                         ################
# drop weeks before and after these dates in regression   <--- OJO   #####
#                                                         ################
sel <- which(dat$week < year(dmy("11-03-2006")) + week(dmy("11-03-2006"))/100
           | dat$week > year(dmy("10-03-2014")) + week(dmy("10-03-2014"))/100 )
dat <- dat[-sel,]
#dat.bak <- dat
#dat <- dat.bak
#
# ESTE JALA
fit <- glm.nb(nrepMoc ~ nNow      + nNowl1                  
                      + n2wk      + n2wkl1     + n2wkl2     + n2wkl3   + n2wkl4
                                  + n4wkl1     + n4wkl2     + n4wkl3   + n4wkl4   
                      + nShorten  + nShortenl1 + nShortenl2 
#                                               + nExtendl2  + nExtendl3 + nExtendl4 
#                                  + nRetirel1  + nRetirel2  + nRetirel3 
#                      + dmaj
                      + dterm
#                      + netApprov
                      + dleg10
                        , data=dat)
#
#summary.glm(fit)$coefficients
#summary.glm(fit)
message(msg); rm(msg)
data.frame( coef=ifelse(coef(fit) > 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "+ ",
                 ifelse(coef(fit) > 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "++",
                 ifelse(coef(fit) < 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "- ",
                 ifelse(coef(fit) < 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "--",
            ". ")))) )
#
# SPACE HERE TO RERUN NEGBIN
fit16 <- fit

#
##########################################################################################
## Regression of Hda Reports to Leg bills on Urgencies to Leg bills ref Hda Comm  DIP   ##
##########################################################################################
msg <- "Regression of Hda Reports to Leg bills on Urgencies to Leg bills ref Hda Comm  DIP"
dat <- RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMocionesInHaciendaComm
#
# re-categorize urgencias
dat$nNowDip <- dat$nurg1Dip
dat$n2wkDip <- dat$nurg2Dip
dat$n4wkDip <- dat$nurg3Dip
dat$nShortenDip <- dat$nurg41Dip
dat$nExtendDip <- dat$nurg42Dip + dat$nurg43Dip
dat$nRetireDip <- dat$nurg51Dip + dat$nurg52Dip + dat$nurg53Dip 
dat$nNowSen <- dat$nurg1Sen
dat$n2wkSen <- dat$nurg2Sen
dat$n4wkSen <- dat$nurg3Sen
dat$nShortenSen <- dat$nurg41Sen
dat$nExtendSen <- dat$nurg42Sen + dat$nurg43Sen
dat$nRetireSen <- dat$nurg51Sen + dat$nurg52Sen + dat$nurg53Sen
#
# remove original variables
dat$nurg1Dip <- dat$nurg2Dip <- dat$nurg2Dip <- NULL
dat$nurg1Sen <- dat$nurg2Sen <- dat$nurg2Sen <- NULL
dat$nurg3Dip <- dat$nurg3Sen <- NULL
dat$nurg41Dip <- dat$nurg42Dip <- dat$nurg43Dip <- dat$nurg51Dip <- dat$nurg52Dip <- dat$nurg53Dip <- NULL
dat$nurg41Sen <- dat$nurg42Sen <- dat$nurg43Sen <- dat$nurg51Sen <- dat$nurg52Sen <- dat$nurg53Sen <- NULL
#
# add post 2010 dummy (when urgencias relaxed)
dat$d2010on <- 0
sel <- which(dat$week > (year(dmy("11-07-2010")) + week(dmy("11-07-2010"))/100))
dat$d2010on[sel] <- 1
#
#                   #############
# drop sen or dip   <--- OJO  ###
#                   #############
dat <- dat[,-grep("[Ss]en", colnames(dat))]  # drop sen
#dat <- dat[,-grep("[Dd]ip", colnames(dat))]  # drop dip
# simpify remaining names
colnames(dat) <- sub("([Dd]ip|[Ss]en)", "", colnames(dat))
#
# drop weeks with no session
dat <- dat[-which(dat$nses==0),]
#
# lags
dat <- dat[order(dat$week),] # verify sorted before lags
dat <- slide(dat, Var = "nNow",     slideBy = -1)
dat <- slide(dat, Var = "n2wk",     slideBy = -1)
dat <- slide(dat, Var = "n4wk",     slideBy = -1)
dat <- slide(dat, Var = "nExtend",  slideBy = -1)
dat <- slide(dat, Var = "nShorten", slideBy = -1)
dat <- slide(dat, Var = "nRetire",  slideBy = -1)
dat <- slide(dat, Var = "nNow",     slideBy = -2)
dat <- slide(dat, Var = "n2wk",     slideBy = -2)
dat <- slide(dat, Var = "n4wk",     slideBy = -2)
dat <- slide(dat, Var = "nExtend",  slideBy = -2)
dat <- slide(dat, Var = "nShorten", slideBy = -2)
dat <- slide(dat, Var = "nRetire",  slideBy = -2)
dat <- slide(dat, Var = "nNow",     slideBy = -3)
dat <- slide(dat, Var = "n2wk",     slideBy = -3)
dat <- slide(dat, Var = "n4wk",     slideBy = -3)
dat <- slide(dat, Var = "nExtend",  slideBy = -3)
dat <- slide(dat, Var = "nShorten", slideBy = -3)
dat <- slide(dat, Var = "nRetire",  slideBy = -3)
dat <- slide(dat, Var = "nNow",     slideBy = -4)
dat <- slide(dat, Var = "n2wk",     slideBy = -4)
dat <- slide(dat, Var = "n4wk",     slideBy = -4)
dat <- slide(dat, Var = "nExtend",  slideBy = -4)
dat <- slide(dat, Var = "nShorten", slideBy = -4)
dat <- slide(dat, Var = "nRetire",  slideBy = -4)
# replace "-" in lag names (glm doesnt like it)
colnames(dat) <- sub(pattern = "-", replacement = "l", colnames(dat))
#
#                                                         ################
# drop weeks before and after these dates in regression   <--- OJO   #####
#                                                         ################
sel <- which(dat$week < year(dmy("11-03-2006")) + week(dmy("11-03-2006"))/100
           | dat$week > year(dmy("10-03-2014")) + week(dmy("10-03-2014"))/100 )
dat <- dat[-sel,]
#data.frame(dat$nrepMoc, dat$nNow)
#dat.bak <- dat
#dat <- dat.bak
#
# ESTE JALÓ EN ALGUN MOMENTO...
fit <- glm.nb(nrepMoc ~ nNow      + nNowl1                  
                                  + n2wkl1     + n2wkl2     + n2wkl3
                                               + n4wkl2     + n4wkl3   + n4wkl4   
#                      + nShorten  + nShortenl1 + nShortenl2 
#                                               + nExtendl2  + nExtendl3 + nExtendl4 
#                                  + nRetirel1  + nRetirel2  + nRetirel3 
#                      + dmaj
                      + dterm
                      + netApprov
                      + dleg10
                        , data=dat)
#
summary.glm(fit)$coefficients
#summary.glm(fit)
message(msg); rm(msg)
data.frame( coef=ifelse(coef(fit) > 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "+ ",
                 ifelse(coef(fit) > 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "++",
                 ifelse(coef(fit) < 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "- ",
                 ifelse(coef(fit) < 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "--",
            ". ")))) )
#
# SPACE HERE TO RERUN NEGBIN
fit14 <- fit

#
##########################################################################################
## Regression of Hda Reports to Ex bills on Urgencies to Leg bills ref Hda Comm  DIP   ##
##########################################################################################
msg <- "Regression of Hda Reports to Ex bills on Urgencies to Leg bills ref Hda Comm  DIP"
dat <- RepDataNegBin$weeklyHaciendaReportsAndUrgenciasToMocionesInHaciendaComm
#
# re-categorize urgencias
dat$nNowDip <- dat$nurg1Dip
dat$n2wkDip <- dat$nurg2Dip
dat$n4wkDip <- dat$nurg3Dip
dat$nShortenDip <- dat$nurg41Dip
dat$nExtendDip <- dat$nurg42Dip + dat$nurg43Dip
dat$nRetireDip <- dat$nurg51Dip + dat$nurg52Dip + dat$nurg53Dip 
dat$nNowSen <- dat$nurg1Sen
dat$n2wkSen <- dat$nurg2Sen
dat$n4wkSen <- dat$nurg3Sen
dat$nShortenSen <- dat$nurg41Sen
dat$nExtendSen <- dat$nurg42Sen + dat$nurg43Sen
dat$nRetireSen <- dat$nurg51Sen + dat$nurg52Sen + dat$nurg53Sen
#
# remove original variables
dat$nurg1Dip <- dat$nurg2Dip <- dat$nurg2Dip <- NULL
dat$nurg1Sen <- dat$nurg2Sen <- dat$nurg2Sen <- NULL
dat$nurg3Dip <- dat$nurg3Sen <- NULL
dat$nurg41Dip <- dat$nurg42Dip <- dat$nurg43Dip <- dat$nurg51Dip <- dat$nurg52Dip <- dat$nurg53Dip <- NULL
dat$nurg41Sen <- dat$nurg42Sen <- dat$nurg43Sen <- dat$nurg51Sen <- dat$nurg52Sen <- dat$nurg53Sen <- NULL
#
# add post 2010 dummy (when urgencias relaxed)
dat$d2010on <- 0
sel <- which(dat$week > (year(dmy("11-07-2010")) + week(dmy("11-07-2010"))/100))
dat$d2010on[sel] <- 1
#
#                   #############
# drop sen or dip   <--- OJO  ###
#                   #############
dat <- dat[,-grep("[Ss]en", colnames(dat))]  # drop sen
#dat <- dat[,-grep("[Dd]ip", colnames(dat))]  # drop dip
# simpify remaining names
colnames(dat) <- sub("([Dd]ip|[Ss]en)", "", colnames(dat))
#
# drop weeks with no session
dat <- dat[-which(dat$nses==0),]
#
# lags
dat <- dat[order(dat$week),] # verify sorted before lags
dat <- slide(dat, Var = "nNow",     slideBy = -1)
dat <- slide(dat, Var = "n2wk",     slideBy = -1)
dat <- slide(dat, Var = "n4wk",     slideBy = -1)
dat <- slide(dat, Var = "nExtend",  slideBy = -1)
dat <- slide(dat, Var = "nShorten", slideBy = -1)
dat <- slide(dat, Var = "nRetire",  slideBy = -1)
dat <- slide(dat, Var = "nNow",     slideBy = -2)
dat <- slide(dat, Var = "n2wk",     slideBy = -2)
dat <- slide(dat, Var = "n4wk",     slideBy = -2)
dat <- slide(dat, Var = "nExtend",  slideBy = -2)
dat <- slide(dat, Var = "nShorten", slideBy = -2)
dat <- slide(dat, Var = "nRetire",  slideBy = -2)
dat <- slide(dat, Var = "nNow",     slideBy = -3)
dat <- slide(dat, Var = "n2wk",     slideBy = -3)
dat <- slide(dat, Var = "n4wk",     slideBy = -3)
dat <- slide(dat, Var = "nExtend",  slideBy = -3)
dat <- slide(dat, Var = "nShorten", slideBy = -3)
dat <- slide(dat, Var = "nRetire",  slideBy = -3)
dat <- slide(dat, Var = "nNow",     slideBy = -4)
dat <- slide(dat, Var = "n2wk",     slideBy = -4)
dat <- slide(dat, Var = "n4wk",     slideBy = -4)
dat <- slide(dat, Var = "nExtend",  slideBy = -4)
dat <- slide(dat, Var = "nShorten", slideBy = -4)
dat <- slide(dat, Var = "nRetire",  slideBy = -4)
# replace "-" in lag names (glm doesnt like it)
colnames(dat) <- sub(pattern = "-", replacement = "l", colnames(dat))
#
#                                                         ################
# drop weeks before and after these dates in regression   <--- OJO   #####
#                                                         ################
sel <- which(dat$week < year(dmy("11-03-2006")) + week(dmy("11-03-2006"))/100
           | dat$week > year(dmy("10-03-2014")) + week(dmy("10-03-2014"))/100 )
dat <- dat[-sel,]
#dat.bak <- dat
#dat <- dat.bak
#
# ESTE JALA
fit <- glm.nb(nrepMoc ~ nNow      + nNowl1                  
                                  + n2wkl1     + n2wkl2     + n2wkl3
                                               + n4wkl2     + n4wkl3   + n4wkl4   
#                      + nShorten  + nShortenl1 + nShortenl2 
#                                               + nExtendl2  + nExtendl3 + nExtendl4 
#                                  + nRetirel1  + nRetirel2  + nRetirel3 
#                      + dmaj
                      + dterm
                      + dleg10
                        , data=dat)
#
#summary.glm(fit)$coefficients
#summary.glm(fit)
message(msg); rm(msg)
data.frame( coef=ifelse(coef(fit) > 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "+ ",
                 ifelse(coef(fit) > 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "++",
                 ifelse(coef(fit) < 0 & summary.glm(fit)$coefficients[,4]>=.10  & summary.glm(fit)$coefficients[,4]<.20, "- ",
                 ifelse(coef(fit) < 0 &                                           summary.glm(fit)$coefficients[,4]<.10, "--",
            ". ")))) )
#
# SPACE HERE TO RERUN NEGBIN
fit13 <- fit

summary.glm(fit15)

# export to latex
library(stargazer)
stargazer(fit11, fit15, title="Regression results", align=TRUE, report = ('vc*p')#,
 ##          covariate.labels=
 ## c("ActNow",
 ##   "ActNow-lag1",
 ##   "ActNow-lag2",
 ##   "Member bill, mix.-sponsored",
 ##   "Member bill, pres. coal.-sp.",
 ##   "Hacienda referral",
 ##   "Senate majority",
 ##   "Introduced Senate",
 ##   "Pres.~term remaining",
 ##   "Year remaining",
 ##   "Relax deadlines",
 ##   "Pres.~approval",
 ##   "2002-06 Leg.",
 ##   "2006-10 Leg.",
 ##   "2010-14 Leg.",
 ##   "Constant")
          )


# drop bills initiated before 11/3/1998
library(lubridate)
drop <- -which(bills$info$dateIn<dmy("11/3/1998"))
bills <- lapply(bills, function(x, drop) {
      if (is.data.frame(x)) return(x[drop,])
      return(x[drop])
    },
    drop = drop
)
rm(drop)
#
I <- nrow(bills$info) # update tot obs

# ADD POLICY DOMAIN (CODED FROM BOLETIN)
bills$info$ndom <- as.numeric(sub(pattern = "[0-9]+-([0-9]+)", replacement = "\\1", bills$info$bol))
bills$info$dom[bills$info$ndom==1] <- "01-agricultura"
bills$info$dom[bills$info$ndom==2] <- "02-defensa"
bills$info$dom[bills$info$ndom==3] <- "03-economía"
bills$info$dom[bills$info$ndom==4] <- "04-educación"
bills$info$dom[bills$info$ndom==5] <- "05-hacienda"
bills$info$dom[bills$info$ndom==6] <- "06-elecciones"
bills$info$dom[bills$info$ndom==7] <- "07-constitución"
bills$info$dom[bills$info$ndom==8] <- "08-minería"
bills$info$dom[bills$info$ndom==9] <- "09-obras púb"
bills$info$dom[bills$info$ndom==10] <- "10-rree"
bills$info$dom[bills$info$ndom==11] <- "11-salud"
bills$info$dom[bills$info$ndom==12] <- "12-medio ambiente"
bills$info$dom[bills$info$ndom==13] <- "13-trabajo"
bills$info$dom[bills$info$ndom==14] <- "14-vivienda"
bills$info$dom[bills$info$ndom==15] <- "15-telecom"
bills$info$dom[bills$info$ndom==16] <- "16-corg"
bills$info$dom[bills$info$ndom==17] <- "17-ddhh"
bills$info$dom[bills$info$ndom==18] <- "18-familia"
bills$info$dom[bills$info$ndom==19] <- "19-ciencia internet"
bills$info$dom[bills$info$ndom==20] <- "20-narco"
bills$info$dom[bills$info$ndom==21] <- "21-pesca"
bills$info$dom[bills$info$ndom==24] <- "24-monumentos"
bills$info$dom[bills$info$ndom==25] <- "25-narco"
bills$info$dom[bills$info$ndom==29] <- "29-deporte"
# MERGE THESE INTO NARROW INTEREST
## bills$info$dom[bills$info$ndom==22] <- "bomberos"
## bills$info$dom[bills$info$ndom==23] <- "turismo"
## bills$info$dom[bills$info$ndom==26] <- "pymes"
## bills$info$dom[bills$info$ndom==27] <- "extremos"
## bills$info$dom[bills$info$ndom==28] <- "discapacitados"
## bills$info$dom[bills$info$ndom==30] <- "juventud"
## bills$info$dom[bills$info$ndom==31] <- "discapacitados"
## bills$info$dom[bills$info$ndom==32] <- "3a edad"
## bills$info$dom[bills$info$ndom==33] <- "subsuelo"
bills$info$dom[bills$info$ndom==22] <- "narrow"
bills$info$dom[bills$info$ndom==23] <- "narrow"
bills$info$dom[bills$info$ndom==26] <- "narrow"
bills$info$dom[bills$info$ndom==27] <- "narrow"
bills$info$dom[bills$info$ndom==28] <- "narrow"
bills$info$dom[bills$info$ndom==30] <- "narrow"
bills$info$dom[bills$info$ndom==31] <- "narrow"
bills$info$dom[bills$info$ndom==32] <- "narrow"
bills$info$dom[bills$info$ndom==33] <- "narrow"
#
table(bills$info$dom)
#
## # drop ndom (handy if selecting domains)
## bills$info$ndom <- NULL

# FIND SPONSORS
# FILL MISSING SPONSORS
i <- which(bills$info$bol=="2428-06") # missing sponsors
tmp <- c("Nombre", "Laura Soto G.", "Joaquín Palma I.", "Aldo Cornejo G.", "Juan Bustos R.")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="2866-06") # missing sponsors
tmp <- c("Nombre", "Carlos Recondo L.")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3085-11") # missing sponsors
tmp <- c("Nombre", "Nicolás Monckeberg D.", "Pedro Pablo Álvarez-Salamanca R.", "Rosauro Martínez L.")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3108-01") # missing sponsors
tmp <- c("Nombre", "Nicolás Monckeberg D.", "Pedro Pablo Álvarez-Salamanca R.", "Rosauro Martínez L.", "Galilea", "Bauer", "Cardemil", "Delmastro", "Palma", "Prieto", "Vargas")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3136-04") # missing sponsors
tmp <- c("Nombre", "Ulloa", "Dittborn", "Longueira", "Víctor Pérez", "Egaña", "Álvarez", "Hernández", "Navarro", "Norambuena")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3144-07") # missing sponsors
tmp <- c("Nombre", "Kast", "Varela", "Forni", "Alvarado", "Norambuena", "Ulloa", "Díaz", "Ibáñez", "Molina", "Cubillos")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3156-07") # missing sponsors
tmp <- c("Nombre", "Kast", "Cristi", "Paya", "Víctor Pérez", "Leay", "Molina", "Dittborn", "García-Huidobro", "Melero", "Uriarte", "Alvarado")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3362-07") # missing sponsors
tmp <- c("Nombre", "Montes", "Bustos", "Espinoza", "Burgos", "Aguiló", "Ceroni")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3363-07") # missing sponsors
tmp <- c("Nombre", "Montes", "Bustos", "Espinoza", "Burgos", "Aguiló", "Ceroni")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3364-07") # missing sponsors
tmp <- c("Nombre", "Errázuriz")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3365-15") # missing sponsors
tmp <- c("Nombre", "Accorsi")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3370-07") # missing sponsors
tmp <- c("Nombre", "Carmen Ibáñez", "Eliana Caraball", "Pía Guzmán", "Salaberry", "Moreira", "Uriarte", "Becker")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3372-04") # missing sponsors
tmp <- c("Nombre", "Muñoz")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3372-04") # missing sponsors
tmp <- c("Nombre", "Muñoz")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3375-04") # missing sponsors
tmp <- c("Nombre", "Ulloa", "Egaña", "Recondo", "Urrutia", "Pablo Galilea", "Vargas", "Melero")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3430-07") # missing sponsors
tmp <- c("Nombre", "Bayo", "Bertolino", "Delmastro", "García", "Hidalgo", "Vargas")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3773-06") # missing sponsors
tmp <- c("Nombre", "Hernán Larraín Fernandez", "Jaime Gazmuri Mujica")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
# FIND SPONSORS
bills$sponsorsRaw <- bills$sponsors # keep original info
tmplook <- rep(0,I) # dummy pointing to names without party info
sel <- which(bills$info$dmensaje==0)
for (i in sel){
    message(sprintf("loop %s of %s", i, I))
    #i <- sel[2] # debug
    tmp <- bills$sponsorsRaw[[i]]
    n <- length(tmp) - 1
    bills$sponsors[[i]] <- data.frame( name=rep(NA,n), region=rep(NA,n), disn=rep(NA,n), party=rep(NA,n), list=rep(NA,n), regex=rep(NA,n) ) # empty frame
    if (length(grep("Región", tmp[1]))==0){
        bills$sponsors[[i]]$name <- tmp[-1] # drop title
        tmplook[i] <- 1 # mark: party needs to be searched
    } else {
        tmp <- tmp[-1] # drop title
        #sub(pattern = "(.*)((?:I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV|RM) Región.*)N°([0-9+]) (.*)", replacement = "\\2", tmp)
        bills$sponsors[[i]]$name <- sub(pattern = "(.*) N°([0-9]+) (.*)", replacement = "\\1", tmp)
        bills$sponsors[[i]]$disn <- sub(pattern = "(.*) N°([0-9]+) (.*)", replacement = "\\2", tmp)
        bills$sponsors[[i]]$party <- sub(pattern = "(.*) N°([0-9]+) (.*)", replacement = "\\3", tmp)
    }
    # remove accents
    bills$sponsors[[i]]$name <- gsub(pattern = "á", replacement = "a", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "é", replacement = "e", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "í", replacement = "i", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "ó", replacement = "o", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "ú", replacement = "u", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "Á", replacement = "A", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "É", replacement = "E", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "Í", replacement = "I", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "Ó", replacement = "O", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "Ú", replacement = "U", bills$sponsors[[i]]$name)
    # regex matching names in any order
    tmp <- bills$sponsors[[i]]$name
    tmp <- sub(pattern = "^", replacement = "^(?=.*", x = tmp)
    tmp <- sub(pattern = "$", replacement = ").*?", x = tmp)
    tmp <- gsub(pattern = " ", replacement = ")(?=.*", x = tmp) 
    bills$sponsors[[i]]$regex <- tmp
}
#
# FILL LIST INFO
sel <- which(bills$info$dmensaje==0 & tmplook==0)
for (i in sel){
    #i <- sel[1] # debug
    bills$sponsors[[i]]$list <- mapvalues(bills$sponsors[[i]]$party, from = c("Independientes", "Izquierda Ciudadana", "Partido Comunista", "Partido Demócrata Cristiano", "Partido Por la Democracia", "Partido Radical Social Demócrata", "Partido Regionalista Independiente", "Partido Socialista", "Renovación Nacional", "Rubén Gajardo Chacón Región N° Partido Demócrata Cristiano", "Unión de Centro Progresista", "Unión Demócrata Independiente"), to = c("ind", "con", "con", "con", "con", "con", "reg", "con", "right", "con", "right", "right"), warn_missing = FALSE)
    bills$sponsors[[i]]$party <- mapvalues(bills$sponsors[[i]]$party, from = c("Independientes", "Izquierda Ciudadana", "Partido Comunista", "Partido Demócrata Cristiano", "Partido Por la Democracia", "Partido Radical Social Demócrata", "Partido Regionalista Independiente", "Partido Socialista", "Renovación Nacional", "Rubén Gajardo Chacón Región N° Partido Demócrata Cristiano", "Unión de Centro Progresista", "Unión Demócrata Independiente"), to = c("ind", "ic", "pcch", "dc", "ppd", "prsd", "pri", "ps", "rn", "dc", "ucp", "udi"), warn_missing = FALSE)
}
#
# import dip/sen names and parties to fill missing list
dip <- read.csv(paste("raw/dip.csv", sep = ""), stringsAsFactors = FALSE)
dip <- dip[,c("name","pty","list")]
sen <- read.csv(paste("raw/sen.csv", sep = ""), stringsAsFactors = FALSE)
sen <- sen[,c("name","pty","list")]
# merge
dip$from <- "dip"; sen$from <- "sen"; mcs <- rbind(dip, sen); rm(dip,sen)
# clean party labels
mcs$pty <- sub(pattern = "ind[-(: )]", replacement = "", mcs$pty) # make leaners party members
mcs$pty <- sub(pattern = "inst[,:] ", replacement = "", mcs$pty) # make leaners party members
mcs$pty[mcs$pty=="mas (ex ps)"] <- "ps"
mcs$pty[mcs$pty=="sd"] <- "prsd"
mcs$pty[mcs$pty=="ppd-ps"] <- "ps"
mcs$pty[mcs$pty=="psdp-pr"] <- "prsd"
# remove accents
mcs$name <- gsub(pattern = "á", replacement = "a", mcs$name)
mcs$name <- gsub(pattern = "é", replacement = "e", mcs$name)
mcs$name <- gsub(pattern = "í", replacement = "i", mcs$name)
mcs$name <- gsub(pattern = "ó", replacement = "o", mcs$name)
mcs$name <- gsub(pattern = "ú", replacement = "u", mcs$name)
mcs$name <- gsub(pattern = "Á", replacement = "A", mcs$name)
mcs$name <- gsub(pattern = "É", replacement = "E", mcs$name)
mcs$name <- gsub(pattern = "Í", replacement = "I", mcs$name)
mcs$name <- gsub(pattern = "Ó", replacement = "O", mcs$name)
mcs$name <- gsub(pattern = "Ú", replacement = "U", mcs$name)
# remove commas
mcs$name <- gsub(pattern = ",", replacement = "", mcs$name)
#

sel <- which(bills$info$dmensaje==0 & tmplook==1)
# find name, add party/list
for (i in sel){
    message(sprintf("loop %s of %s", i, I))
    #i <- sel[7]; j <- 1; bills$info$bol[i] # debug
    tmp <- bills$sponsors[[i]]$regex
    n <- length(tmp)
    for (j in 1:n){
        tmphits <- grep(pattern = tmp[j], x = mcs$name, perl = TRUE)
        if (length(tmphits)==0) next
        if (length(tmphits)==1){
            bills$sponsors[[i]]$pty <- mcs$pty[tmphits]
            bills$sponsors[[i]]$list <- mcs$list[tmphits]
            tmplook[i] <- 0
        } else {
            bills$sponsors[[i]]$pty <- paste(mcs$pty[tmphits], collapse = "-") # colapses many labels into one long
            bills$sponsors[[i]]$list <- paste(mcs$list[tmphits], collapse = "-") # colapses many labels into one long
            tmplook[i] <- 0
        }
    }
}

# COMPUTE CONCERTACIÓN AND RIGHT % SPONSORS
bills$info$pctcon <- bills$info$pctright <- NA
# exec-init 100 percent their list
sel <- which(bills$info$dmensaje==1)
bills$info$pctcon[sel] <- 100
bills$info$pctright[sel] <- 0
sel <- which(bills$info$dmensaje==1 & bills$info$dateIn>=dmy("1/3/2010") & bills$info$dateIn<dmy("1/3/2014")) # piñera
bills$info$pctcon[sel] <- 0
bills$info$pctright[sel] <- 100
# mc-init
sel <- which(bills$info$dmensaje==0)
for (i in sel){
    tmp <- bills$sponsors[[i]]$list
    n <- length(tmp)
    bills$info$pctcon[i] <- length(grep(pattern = "con", tmp)) * 100 / n
    bills$info$pctright[i] <- length(grep(pattern = "right", tmp)) * 100 / n
}
# round
bills$info$pctcon <- round(bills$info$pctcon, digits = 0)
bills$info$pctright <- round(bills$info$pctright, digits = 0)
#
rm(i, j, mcs, n, sel, tmp, tmphits, tmplook)
#
sel <- which(allUrg$chains$bol=="7007-18")
allUrg$chains[sel,]
sel <- which(allUrg$messages$bol=="7007-18")
allUrg$messages[sel,]

# summarize urgency chain types
# OJO: drops messages in period that referred to bills proposed before the period, in order to match the N with chain regressions
tmp <- dmy(c("11-03-1990", "11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") )
sel <- which(bills$info$dateIn >= tmp[3] & bills$info$dateIn < tmp[7]); target <- bills$info$bol[sel]
keep <- rep(0, nrow(allUrg$chains)); keep[which(allUrg$chains$bol %in% target)] <- 1; table(keep)
sel <- which(keep==1 & allUrg$chains$on >= tmp[3] & allUrg$chains$on < tmp[4])
round(table(allUrg$chains$typeN[sel])*100/length(sel),0); length(sel)
sel <- which(keep==1 & allUrg$chains$on >= tmp[4] & allUrg$chains$on < tmp[5])
round(table(allUrg$chains$typeN[sel])*100/length(sel),0); length(sel)
sel <- which(keep==1 & allUrg$chains$on >= tmp[5] & allUrg$chains$on < tmp[6])
round(table(allUrg$chains$typeN[sel])*100/length(sel),0); length(sel)
sel <- which(keep==1 & allUrg$chains$on >= tmp[6] & allUrg$chains$on < tmp[7])
round(table(allUrg$chains$typeN[sel])*100/length(sel),0); length(sel)
sel <- which(keep==1 & allUrg$chains$on >= tmp[3] & allUrg$chains$on < tmp[7])
round(table(allUrg$chains$typeN[sel])*100/length(sel),0); length(sel)
#
# chains dropped because targeted to bills started before the period
drop <- which(allUrg$chains$on < tmp[3])
length(keep[-drop]); sum(keep[-drop]); length(keep[-drop]) - sum(keep[-drop]); 

# recode leg year to indicate 1990 for 1990-91, etc
bills$info$legyr <- bills$info$legyr + 1989
#
table(bills$info$dmensaje, bills$info$legyr) # size of pdt's agenda, use to code variable

## cross committee and referral to code whether bill referred to committee where chair is same party as president
# LOAD COMMITTEE CHAIR DATA
comPres <- read.csv(file = "raw/comisiones1990-2014.csv", stringsAsFactors=FALSE)
comPres$comision <- gsub(pattern = "^ +(.*) +$", replacement = "\\1", comPres$comision)
comPres$comision <- gsub(pattern = "Á", replacement = "A", comPres$comision)
comPres$comision <- gsub(pattern = "É", replacement = "E", comPres$comision)
comPres$comision <- gsub(pattern = "Í", replacement = "I", comPres$comision)
comPres$comision <- gsub(pattern = "Ó", replacement = "O", comPres$comision)
comPres$comision <- gsub(pattern = "Ú", replacement = "U", comPres$comision)
#
comPres$partido[grep("EN", comPres$partido)] <- "RN" # corrige error
comPres$partido[grep("PDC", comPres$partido)] <- "DC" # corrige error
#
comPres$partido[which(comPres$partido=="IND" & comPres$yr==1998)] <- "UDI" # indep later became UDI
comPres$partido[which(comPres$partido=="IND" & comPres$yr==2006)] <- "PPD" # indep later became close to PPD
#
comPres$date <- dmy(paste("1-3-", comPres$yr, sep = ""))
#
# presidente comisión mismo partido que presidente república
comPres$dsamePty <- as.numeric(  (comPres$partido=="DC" & comPres$yr==1990)
                               | (comPres$partido=="DC" & comPres$yr==1994)
                               | (comPres$partido=="DC" & comPres$yr==1998)
                               | (comPres$partido=="DC" & comPres$yr==2002)
                               | (comPres$partido=="PS" & comPres$yr==2006)
                               | (comPres$partido=="RN" & comPres$yr==2010)
                               | (comPres$partido=="PS" & comPres$yr==2014)
                               )
# presidente comisión misma coalición que presidente república
comPres$dsameCoal <- as.numeric(  (comPres$partido=="DC" & comPres$yr==1990)
                                | (comPres$partido=="PPD" & comPres$yr==1990)
                                | (comPres$partido=="PRSD" & comPres$yr==1990)
                                | (comPres$partido=="PS" & comPres$yr==1990)
                                | (comPres$partido=="DC" & comPres$yr==1994)
                                | (comPres$partido=="PPD" & comPres$yr==1994)
                                | (comPres$partido=="PRSD" & comPres$yr==1994)
                                | (comPres$partido=="PS" & comPres$yr==1994)
                                | (comPres$partido=="DC" & comPres$yr==1998)
                                | (comPres$partido=="PPD" & comPres$yr==1998)
                                | (comPres$partido=="PRSD" & comPres$yr==1998)
                                | (comPres$partido=="PS" & comPres$yr==1998)
                                | (comPres$partido=="DC" & comPres$yr==2002)
                                | (comPres$partido=="PPD" & comPres$yr==2002)
                                | (comPres$partido=="PRSD" & comPres$yr==2002)
                                | (comPres$partido=="PS" & comPres$yr==2002)
                                | (comPres$partido=="DC" & comPres$yr==2006)
                                | (comPres$partido=="PPD" & comPres$yr==2006)
                                | (comPres$partido=="PRSD" & comPres$yr==2006)
                                | (comPres$partido=="PS" & comPres$yr==2006)
                                | (comPres$partido=="PRI" & comPres$yr==2010)
                                | (comPres$partido=="RN" & comPres$yr==2010)
                                | (comPres$partido=="UDI" & comPres$yr==2010)
                                | (comPres$partido=="DC" & comPres$yr==2014)
                                | (comPres$partido=="PC" & comPres$yr==2014)
                                | (comPres$partido=="PPD" & comPres$yr==2014)
                                | (comPres$partido=="PRSD" & comPres$yr==2014)
                                | (comPres$partido=="PS" & comPres$yr==2014)
                                )
# presidente comisión de la oposición
comPres$doposCom <- as.numeric(  (comPres$partido=="RN" & comPres$yr==1990)
                               | (comPres$partido=="UDI" & comPres$yr==1990)
                               | (comPres$partido=="RN" & comPres$yr==1994)
                               | (comPres$partido=="UDI" & comPres$yr==1994)
                               | (comPres$partido=="RN" & comPres$yr==1998)
                               | (comPres$partido=="UDI" & comPres$yr==1998)
                               | (comPres$partido=="RN" & comPres$yr==2002)
                               | (comPres$partido=="UDI" & comPres$yr==2002)
                               | (comPres$partido=="RN" & comPres$yr==2006)
                               | (comPres$partido=="UDI" & comPres$yr==2006)
                               | (comPres$partido=="DC" & comPres$yr==2010)
                               | (comPres$partido=="PC" & comPres$yr==2010)
                               | (comPres$partido=="PPD" & comPres$yr==2010)
                               | (comPres$partido=="PRSD" & comPres$yr==2010)
                               | (comPres$partido=="PS" & comPres$yr==2010)
                               | (comPres$partido=="PRI" & comPres$yr==2014)
                               | (comPres$partido=="RN" & comPres$yr==2014)
                               | (comPres$partido=="UDI" & comPres$yr==2014)
                               )
#
comPres$comLong <- comPres$comision
comPres$comision[grep("agricultura", comPres$comLong, ignore.case = TRUE)]            <- "Agric"   
comPres$comision[grep("ciencias*", comPres$comLong, ignore.case = TRUE)]              <- "Ciencia" 
comPres$comision[grep("constitucion", comPres$comLong, ignore.case = TRUE)]           <- "Const"   
comPres$comision[grep("cultura y de las artes", comPres$comLong, ignore.case = TRUE)] <- "Cultura" 
comPres$comision[grep("defensa", comPres$comLong, ignore.case = TRUE)]                <- "Defensa" 
comPres$comision[grep("derechos", comPres$comLong, ignore.case = TRUE)]               <- "DDHH"    
comPres$comision[grep("desarrollo social,", comPres$comLong, ignore.case = TRUE)]     <- "Desarr"
comPres$comision[grep("economia", comPres$comLong, ignore.case = TRUE)]               <- "Eco"     
comPres$comision[grep("educacion", comPres$comLong, ignore.case = TRUE)]              <- "Educ"    
comPres$comision[grep("familia", comPres$comLong, ignore.case = TRUE)]                <- "Familia" 
comPres$comision[grep("interior", comPres$comLong, ignore.case = TRUE)]               <- "Interior"
comPres$comision[grep("hacienda", comPres$comLong, ignore.case = TRUE)]               <- "Hda"     
comPres$comision[grep("mineria", comPres$comLong, ignore.case = TRUE)]                <- "Mineria" 
comPres$comision[grep("obras", comPres$comLong, ignore.case = TRUE)]                  <- "Obras"   
comPres$comision[grep("pesca,", comPres$comLong, ignore.case = TRUE)]                 <- "Pesca"   
comPres$comision[grep("recursos", comPres$comLong, ignore.case = TRUE)]               <- "RecNatur"
comPres$comision[grep("relaciones", comPres$comLong, ignore.case = TRUE)]             <- "RREE"    
comPres$comision[grep("salud", comPres$comLong, ignore.case = TRUE)]                  <- "Salud"   
comPres$comision[grep("seguridad ciudadana", comPres$comLong, ignore.case = TRUE)]    <- "Segurid" 
comPres$comision[grep("trabajo", comPres$comLong, ignore.case = TRUE)]                <- "Trabajo" 
comPres$comision[grep("vivienda", comPres$comLong, ignore.case = TRUE)]               <- "Vivienda"
comPres$comision[grep("zonas", comPres$comLong, ignore.case = TRUE)]                  <- "Zonas"   
#
# subset by committee
Agric <- comPres[which(comPres$comision=="Agric"   )   ,]
Ciencia <- comPres[which(comPres$comision=="Ciencia" ),]
Const <- comPres[which(comPres$comision=="Const"   ),]
Cultura <- comPres[which(comPres$comision=="Cultura" ),]
Defensa <- comPres[which(comPres$comision=="Defensa" ),]
DDHH <- comPres[which(comPres$comision=="DDHH"    ),]
Desarr <- comPres[which(comPres$comision=="Desarr"  ),]
Eco <- comPres[which(comPres$comision=="Eco"     ),]
Educ <- comPres[which(comPres$comision=="Educ"    ),]
Familia <- comPres[which(comPres$comision=="Familia" ),]
Interior <- comPres[which(comPres$comision=="Interior"),]
Hda <- comPres[which(comPres$comision=="Hda"     ),]
Mineria <- comPres[which(comPres$comision=="Mineria" ),]
Obras <- comPres[which(comPres$comision=="Obras"   ),]
Pesca <- comPres[which(comPres$comision=="Pesca"   ),]
RecNatur <- comPres[which(comPres$comision=="RecNatur"),]
RREE <- comPres[which(comPres$comision=="RREE"    ),]
Salud <- comPres[which(comPres$comision=="Salud"   ),]
Segurid <- comPres[which(comPres$comision=="Segurid" ),]
Trabajo <- comPres[which(comPres$comision=="Trabajo" ),]
Vivienda <- comPres[which(comPres$comision=="Vivienda"),]
Zonas <- comPres[which(comPres$comision=="Zonas"   ),]
#
bills$info$dsamePty <- 0
bills$info$dsameCoal <- 0
bills$info$doposCom <- 0
tmp <- bills$info[which(bills$info$drefAgric   ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Agric$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Agric$dsamePty[sel]    # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Agric$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Agric$doposCom[sel]    # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefAgric   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefCiencia ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Ciencia$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Ciencia$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Ciencia$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Ciencia$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefCiencia   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefConst   ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- i # debug
    sel <- max(which(Const$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i] %in% 0) tmp$dsamePty[i] <- Const$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Const$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Const$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefConst   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefCultura ==1 & bills$info$dateIn>=dmy("1-3-2010") & bills$info$dateIn<dmy("1-3-2014")),] # cultura before 2010 was special committee
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- i+1#i <- 1 # debug
    sel <- max(which(Cultura$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Cultura$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Cultura$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Cultura$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefCultura ==1 & bills$info$dateIn>=dmy("1-3-2010") & bills$info$dateIn<dmy("1-3-2014")),] <- tmp
#
tmp <- bills$info[which(bills$info$drefDefensa ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Defensa$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Defensa$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Defensa$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Defensa$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefDefensa   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefDDHH    ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(DDHH$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- DDHH$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- DDHH$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- DDHH$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefDDHH   ==1),] <- tmp
#
## # no bills referred to desarrollo
## tmp <- bills$info[which(bills$info$drefDesarr  ==1),]
## for (i in 1:nrow(tmp)){
##     message(sprintf("iteración %s of %s", i, nrow(tmp)))
##     i <- 1 # debug
##     sel <- max(which(Desarr$date < tmp$dateIn[i]))
##     tmp$dsamePty[i] <- Desarr$dsamePty[sel]
##     tmp$dsameCoal[i] <- Desarr$dsameCoal[sel]
## }
## bills$info[which(bills$info$drefDesarr   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefEco     ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Eco$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Eco$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Eco$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Eco$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefEco   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefEduc    ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Educ$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Educ$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Educ$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Educ$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefEduc   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefFamilia ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Familia$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Familia$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Familia$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Familia$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefFamilia   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefInterior==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Interior$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Interior$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Interior$dsameCoal[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefInterior   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefHda     ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Hda$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Hda$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Hda$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Hda$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefHda   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefMineria ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Mineria$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Mineria$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Mineria$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Mineria$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefMineria   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefObras   ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Obras$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Obras$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Obras$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Obras$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefObras   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefPesca   ==1 & bills$info$dateIn>=dmy("1-3-2002") & bills$info$dateIn<dmy("1-3-2014")),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    i <- 1 # debug
    sel <- max(which(Pesca$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Pesca$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Pesca$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Pesca$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefPesca   ==1 & bills$info$dateIn>=dmy("1-3-2002") & bills$info$dateIn<dmy("1-3-2014")),] <- tmp
#
tmp <- bills$info[which(bills$info$drefRecNatur==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(RecNatur$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- RecNatur$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- RecNatur$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- RecNatur$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefRecNatur   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefRREE    ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(RREE$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- RREE$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- RREE$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- RREE$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefRREE   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefSalud   ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Salud$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Salud$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Salud$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Salud$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefSalud   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefSegurid ==1 & bills$info$dateIn>=dmy("1-3-2010") & bills$info$dateIn<dmy("1-3-2014")),] # seguridad before 2010 was special committee
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Segurid$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Segurid$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Segurid$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Segurid$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefSegurid ==1 & bills$info$dateIn>=dmy("1-3-2010") & bills$info$dateIn<dmy("1-3-2014")),] <- tmp
#
tmp <- bills$info[which(bills$info$drefTrabajo ==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Trabajo$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Trabajo$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Trabajo$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Trabajo$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefTrabajo   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefVivienda==1),]
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Vivienda$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Vivienda$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Vivienda$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Vivienda$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefVivienda   ==1),] <- tmp
#
tmp <- bills$info[which(bills$info$drefZonas   ==1 & bills$info$dateIn>=dmy("1-3-2010") & bills$info$dateIn<dmy("1-3-2014")),] # zonas before 2010 was special committee
for (i in 1:nrow(tmp)){
    message(sprintf("iteración %s of %s", i, nrow(tmp)))
    #i <- 1 # debug
    sel <- max(which(Zonas$date < tmp$dateIn[i]))
    if (tmp$dsamePty[i]==0) tmp$dsamePty[i] <- Zonas$dsamePty[sel] # if value is 1 from multiple referrals, skip
    if (tmp$dsameCoal[i]==0) tmp$dsameCoal[i] <- Zonas$dsameCoal[sel] # if value is 1 from multiple referrals, skip
    if (tmp$doposCom[i]==0) tmp$doposCom[i] <- Zonas$doposCom[sel] # if value is 1 from multiple referrals, skip
}
bills$info[which(bills$info$drefZonas   ==1 & bills$info$dateIn>=dmy("1-3-2010") & bills$info$dateIn<dmy("1-3-2014")),] <- tmp
#
rm(Agric, Ciencia, Const, Cultura, Defensa, DDHH, Desarr, Eco, Educ, Familia, Interior, Hda, Mineria, Obras, Pesca, RecNatur, RREE, Salud, Segurid, Trabajo, Vivienda, Zonas)
#
tmp <-
    bills$info$drefAgric +
    bills$info$drefCiencia +
    bills$info$drefComunic +
    bills$info$drefConst +
    bills$info$drefCultura +
    bills$info$drefDefensa +
    bills$info$drefDDHH +
    bills$info$drefEco +
    bills$info$drefEduc +
    bills$info$drefFamilia +
    bills$info$drefHda +
    bills$info$drefInterior +
    bills$info$drefMedAmb +
    bills$info$drefMineria +
    bills$info$drefObras +
    bills$info$drefPesca +
    bills$info$drefRREE +
    bills$info$drefRecNatur +
    bills$info$drefRegimen +
    bills$info$drefSalud +
    bills$info$drefSegurid +
    bills$info$drefTrabajo +
    bills$info$drefVivienda +
    bills$info$dcomEspec +
    bills$info$dcomMixta +
    bills$info$dcomUnidas +
    bills$info$drefZonas
round(table(tmp>1)/length(tmp), 2) # unlike dmultiref, this includes Hda referrals
################################################################
## reported in text: proportion bills with multiple referrals ##
################################################################
round(table(bills$info$dmultiRef)/length(bills$info$dmultiRef), 2)
#
rm(i,keep,target,sel,tmp,tmpHit,tmpPeriod,tmpVotIndices)

#describe dsamePres by legislatura...
table(comPres$yr, comPres$dsamePty)
table(comPres$yr, comPres$dsameCoal)
table(comPres$yr, comPres$doposCom)

#####################################################################
rm(drop, fit, fit11, fit12, fit13, fit14, fit15, fit16, RepDataNegBin)

# clean to export data for published analysis
rm(vp5, "%my_within%", dat, deadline, approv, datdir, sel.na)

save.image("dataForUrgencyRegressions.RData")





