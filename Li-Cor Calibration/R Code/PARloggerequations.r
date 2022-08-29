########################
# PARloggerequations.r #
########################
# created: 2021 Aug 30
# revised date: __
# Develops equations to convert Odyssey and HOBO pendant logger outputs to PAR  (PPFD)units using calibration deployments with LICOR
# UNITS: Photosynthetic Photon Flux Density (PPFD) in units of micromoles of photons per square meter per second (μmol/m^2/s)

library(hms)
#library(chron)
library(lubridate)

#--------- SECTION 1A: GENERATE CONVERSION EQUATIONS, based on MESOCOSM deployment ----------

setwd("Z:/LiCor/MesocosmCalibration")

#####
# Import and massage LICOR dataset:

licor <- read.table('LICOR_Trial 2_2021-08-27.TXT', skip=8, header=TRUE)

licor$Date2 <- as.Date(licor$Date, format='20%y-%m-%d')

licor$Time2 <- format(as.POSIXlt(round(as.double(as.POSIXlt(licor$Time, format='%H:%M:%S'))/(5*60))*(5*60), origin=as.POSIXlt('1970-01-01')), '%H:%M') # round licor time to nearest 5 min interval (for some reason interval is slightly less than 5 min)

#####
# Compare LiCor sensor 1 and 2

plot(licor$INPUT1, licor$INPUT2, pty='s')

curve(x^1, add=TRUE, lty=2, col=2) # plots 1:1 line
text(600,600, '1:1 line', col=2, pos=4)
t.test(licor$INPUT1, licor$INPUT2, paired=TRUE) # t-test says the 2 PAR sensors are not significantly diff than each other

licor$avPAR <- (licor$INPUT1+licor$INPUT2)/2 # calculate mean of sensor 1 and 2 to use for developing equations


#####
# Import & Massage LC datasets, merge with LICOR, and develop equations for each LC# in Trial2:

ODYeqns <- matrix(nrow=20, ncol=5) # create a table to fill in with equation values for each ODY LC#
colnames(ODYeqns) <- c('ODYunit','SerialNo','Slope','Intercept','Rsquared')

par(mfrow=c(4,5), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) # set up multipanel plotting window; Make sure to make plotting window as large as possible before running the following loop (otherwise graphs will be too tiny)

for (i in 1:20) #begin i FOR LOOP
{
LCname <- paste('LC',i, sep='')

LC <- read.csv(paste(LCname,'_T2_2021-08-27.csv', sep=''), skip=9, header=FALSE, strip.white=TRUE, col.names=c('ScanNo','Date','Time','RAW1','RAW2'))

SN <- as.character(read.csv(paste(LCname,'_T2_2021-08-27.csv', sep=''), header=FALSE)[4,2]) # extract serial number from logger file

LC$Date2 <- as.Date(LC$Date, format='%d/%m/20%y')

LC$Time2 <- format(as.POSIXlt(LC$Time, format='%H:%M:%S'), '%H:%M')

# Check formats of both Licor & LC datasets

head(licor)
head(LC)
tail(licor)
tail(LC)

# MERGE the LICOR and LC datasets to match date and time lines

PAR <- merge(licor, LC, by=c('Date2','Time2'), all.x=TRUE, all.y=TRUE)

head(PAR) # check that the merge happened correctly

# ODY fitting as linear regression:

PAReqn <- lm(PAR$avPAR ~ PAR$RAW1)
summary(PAReqn)

ODYeqns[i,1] <- LCname
ODYeqns[i,2] <- SN
ODYeqns[i,3] <- coef(PAReqn)[2]
ODYeqns[i,4] <- coef(PAReqn)[1]
ODYeqns[i,5] <- summary(PAReqn)$adj.r.squared

# Plot with equation & R^2
plot(PAR$avPAR ~ PAR$RAW1, main=LCname, pch='.', ylab='LICOR PPFD units', xlab='ODY units')

abline(PAReqn, col=2, lty=2) # plots best fit line

legend('topleft', legend = c(paste('PAR = ',round(coef(PAReqn)[2],4),'*ODY + ',round(coef(PAReqn)[1],4), sep=''), paste('R^2 =',round(summary(PAReqn)$adj.r.squared,2))), bty='n', text.col=4)

} # end i FOR LOOP

(ODYeqns <- as.data.frame(ODYeqns)) # output table of equation coefficients for each logger

write.csv(ODYeqns, file='PARequations_SPMCTrial220210827.csv', row.names=FALSE) # exports table of coefficients to the working directory for future use

# NOTE: something funky going on with LC8 (serial 6433), see graph.



#--------- SECTION 1B: GENERATE CONVERSION EQUATIONS, based on FIELD deployment ----------
# deployed with only 1 PAR sensor using the longer cable into INPUT2

setwd("Z:/LiCor/FieldCalibration")

#####
# Import and massage LICOR dataset:

Flicor <- read.csv('FIELD_LICOR_2021Sept_edited.csv')
head(Flicor)
Flicor$Date2 <- as.Date(Flicor$Date, format='%m/%d/20%y')

Flicor$Time2 <- format(as.POSIXlt(round(as.double(as.POSIXlt(Flicor$Time, format='%H:%M:%S'))/(5*60))*(5*60), origin=as.POSIXlt('1970-01-01')), '%H:%M') # round licor time to nearest 5 min interval


##### FOR ODYSSEY LOGGERS
# Import & Massage Odyssey LC datasets, merge with LICOR, and develop equations for each LC#:

FODYeqns <- matrix(nrow=20, ncol=5) # create a table to fill in with equation values for each ODY LC#
colnames(FODYeqns) <- c('ODYunit','SerialNo','Slope','Intercept','Rsquared')

par(mfrow=c(4,5), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) # set up multipanel plotting window; Make sure to make plotting window as large as possible before running the following loop (otherwise graphs will be too tiny)

for (n in c(1:7,9:20)) #begin n FOR LOOP, skips LC8 (has been retired)
{
  FLCname <- paste('LC',n, sep='')
  
  FLC <- read.csv(paste(FLCname,'_Field_2021-09-03.csv', sep=''), skip=9, header=FALSE, strip.white=TRUE, col.names=c('ScanNo','Date','Time','RAW1','RAW2'))
  
  FSN <- as.character(read.csv(paste(FLCname,'_Field_2021-09-03.csv', sep=''), header=FALSE)[4,2]) # extract serial number from logger file
  
  FLC$Date2 <- as.Date(FLC$Date, format='%d/%m/20%y')
  
  FLC$Time2 <- format(as.POSIXlt(FLC$Time, format='%H:%M:%S'), '%H:%M')
  
  # Check formats of both Licor & LC datasets
  
  head(Flicor)
  head(FLC)
  tail(Flicor)
  tail(FLC)
  
  # MERGE the LICOR and LC datasets to match date and time lines
  
  FPAR <- merge(Flicor, FLC, by=c('Date2','Time2'), all.x=TRUE, all.y=TRUE)
  
  head(FPAR) # check that the merge happened correctly
  
  # ODY fitting as linear regression:
  
  FPAReqn <- lm(FPAR$INPUT2 ~ FPAR$RAW1)
  summary(FPAReqn)
  
  FODYeqns[n,1] <- FLCname
  FODYeqns[n,2] <- FSN
  FODYeqns[n,3] <- coef(FPAReqn)[2]
  FODYeqns[n,4] <- coef(FPAReqn)[1]
  FODYeqns[n,5] <- summary(FPAReqn)$adj.r.squared
  
  # Plot with equation & R^2
  plot(FPAR$INPUT2~ FPAR$RAW1, main=FLCname, pch='.', ylab='LICOR PPFD units', xlab='ODY units')
  
  abline(FPAReqn, col=2, lty=2) # plots best fit line
  
  legend('topleft', legend = c(paste('PAR = ',round(coef(FPAReqn)[2],4),'*ODY + ',round(coef(FPAReqn)[1],4), sep=''), paste('R^2 =',round(summary(FPAReqn)$adj.r.squared,2))), bty='n', text.col=4)
  
} # end n FOR LOOP

FODYeqns <- as.data.frame(FODYeqns) # output table of equation coefficients for each logger

(FODYeqns <- FODYeqns[complete.cases(FODYeqns),])

write.csv(FODYeqns, file='PARequations_Ody_Field20210903.csv', row.names=FALSE) # exports table of coefficients to the working directory for future use


##### FOR HOBO LOGGERS
# Import & Massage Hobo files, merge with LICOR, and develop equations for each serial number:
hoboeqns <- matrix(nrow=20, ncol=4) # create a table to fill in with equation values for each Hobo SN

colnames(hoboeqns) <- c('SerialNo','A1','t1','y0')

par(mfrow=c(4,5), mgp=c(1.5,0.5,0), mar=c(4,3,3,1)) # set up multipanel plotting window; Make sure to make plotting window as large as possible before running the following loop (otherwise graphs will be too tiny)

HFiles <- substr(Sys.glob("*.hobo"),1,10) # all csv files in the working directory will be converted

for (k in 1:length(HFiles))  # begin j FOR LOOP
{
  hobo <- read.csv(paste(HFiles[k],'_20210903.csv', sep=''), skip=1, header=FALSE, strip.white=TRUE, col.names=c('Date','Time','Intensity_Lux'))

  hoboSN <- substr(HFiles[k],3,10)
  
  hobo$Date2 <- as.Date(hobo$Date, format='%m/%d/%y')
  
  hobo$Time2 <- format(as.POSIXlt(hobo$Time, format='%H:%M:%S'), '%H:%M')
  
  # Check formats of both Licor & hobo datasets
  
  head(Flicor)
  head(hobo)
  tail(Flicor)
  tail(hobo)
  
  # MERGE the LICOR and hobo datasets to match date and time lines
  
  hoboPAR <- merge(Flicor, hobo, by=c('Date2','Time2'), all.x=TRUE, all.y=TRUE)
  
  head(hoboPAR) # check that the merge happened correctly
  tail(hoboPAR)

  # Plot with equation & R^2
  plot(hoboPAR$INPUT2~ hoboPAR$Intensity_Lux, pch='.', main=hoboSN, ylab='LICOR - PPFD units', xlab='HOBO - Intensity (Lux)')

  # Hobo fitting as exponential decay:
  lic <- hoboPAR$INPUT2
  hob <- hoboPAR$Intensity_Lux
  
  m <- nls(lic ~ A1*exp(-hob/t1) + y0, start = list(A1 = -800, t1 = 10500, y0 = 800)) # nonlinear fit using exponential decay from Long et al 2012
  
  curve(coef(m)['A1']*exp(-x/coef(m)['t1']) + coef(m)['y0'], col=2, add=TRUE)  
  
  abline(lm(hoboPAR$INPUT2 ~ hoboPAR$Intensity_Lux), col=4) # also plots linear regression in blue, which is not as good of a fit
  
  hoboeqns[k,1] <- hoboSN
  hoboeqns[k,2:4] <- coef(m)
  
} # end k loop

(hoboeqns <- as.data.frame(hoboeqns)) # output table of equation coefficients for each logger

write.csv(hoboeqns, file='PARequations_Hobo_Field20210903.csv', row.names=FALSE) # exports table of coefficients to the working directory for future use





#--------- SECTION 2: APPLY EQUATIONS ----------
## CONVERT FIELD DEPLOYMENT of ODY LOGGERS TO PAR UNITS 

calib <- read.csv('PARequations_Field20210903.csv') # import data table of equation coefficients to be used for PAR conversion

setwd("Z:/LiCor/FieldLoggers") # set directory with raw logger files to be converted to PAR
dir.create("Z:/LiCor/FieldLoggers/ConvertedPAR") # create output folder for converted files within the working directory

Files <- Sys.glob("*.CSV") # all csv files in the working directory will be converted

for (j in 1:length(Files))  # begin j FOR LOOP
{
loggerfile <- Files[j]

data <- read.csv(loggerfile, skip=9, header=FALSE, strip.white=TRUE, col.names=c('ScanNo','Date','Time','RAW1','RAW2')) # import logger data to be converted to PAR

dataSN <- as.numeric(as.character(read.csv(loggerfile, header=FALSE)[4,2])) # extract serial number of logger

print(dataSN)

data$PAR <- calib[calib$SerialNo == dataSN,3]*data$RAW1+calib[calib$SerialNo == dataSN,4] # use coefficients associated with matching serial number for conversion of raw data to PAR

write.csv(data, paste('ConvertedPAR/cal_',loggerfile, sep=''), row.names=FALSE) # export to the output folder

} # END j FOR LOOP 
