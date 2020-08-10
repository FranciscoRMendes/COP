##### read.MPM.tsep.well.1.r

### read multiphase meter data
### read test separator data
### read data for individual wells
### join all files by DATE.TIME, WELL.ID
### loess smooth all data

require(stringr)
require(dplyr)
require(lubridate)
require(data.table)
require(ggplot2)

# dir.dat <- '/shared/AcousticsSensors/Acoustics_Alaska/Acoustics_R/Data_R'
dir.dat <-  './Data'

############# helper functions############# 

f.make.names <- function(df){
  for( i in 1:length(df)){
    names(df)[i] <- str_trim(paste(df[1:3, i], collapse=' '))
  }
  names(df) <- str_replace_all(names(df), '-| ', '.')
  names(df) <- str_replace_all(names(df), '[()]', '')
  names(df) <- str_replace_all(names(df), '\\.+', '\\.')
  df<- head(df[-(1:3), -length(df)], -2)
  ############ Check date-time format in the original Meter data files
  # df$DATE.TIME <- strptime(df$DATE.TIME, format='%d-%b-%y %T', tz='UTC')
  # df$DATE.TIME <- mdy_hm(df$DATE.TIME) %>% floor_date('minute')
  df$DATE.TIME <- dmy_hms(df$DATE.TIME) %>% floor_date('minute')
  return(df)
}

f.time.fill <- function(df, time.start, time.end, dt.step=seconds(1), fill.value=0,.. ){
  df.out <- data.frame(Date = seq(from=time.start, to=time.end, by=dt.step))
  df.out[,names(df)[-1]] <- fill.value  # assumes the first column is df$Date
  df.out[df.out$Date %in% df$Date, -1] <- df[df$Date %in% df.out$Date,-1]
  return(df.out)
}


############# READ MULTIPHASE METER DATA ####################

# dir.meter <- '/shared/AcousticsSensors/Acoustics_Alaska/Acoustics_R/Data_R'
dir.meter <-  "C:/Users/frmendes/Desktop/Conoco Phillps/Meter Data"

sfx.mpm <- 'MultiphaseMeter_alltags_exceptAI'
files.mpm <- dir(path = dir.meter, pattern = sfx.mpm, all.files = FALSE,
                 full.names = TRUE)

d.mpm <- read.csv(files.mpm[1], stringsAsFactors=FALSE, skip=1, header=F) %>% 
                                                                    f.make.names
for(file in files.mpm[-1]){
  d.temp <- read.csv(file, stringsAsFactors=FALSE, skip=1, header=F) %>% 
                                                                    f.make.names 
  d.mpm <- rbind(d.mpm, d.temp)
}
rm(d.temp)

names.mpm.keep <- c('DATE.TIME',
                    'FI.1E1425.G.1E.MPM.Gas.V.olumetric.Flow.Std',
                    'TF1E3S_FORMGOR.1E.TSEP.MPM.Std.FORMATION.GOR',    
                    'TF1E3G_STDGAS.1E.TSEP.MPM.ActG.STD.GAS.FLOW.RATE',
                    'TF1E3G_STDH2O.1E.TSEP.MPM.ActG.STD.H2O.FLOW.RATE',
                    'TF1E3G_STDOIL.1E.TSEP.MPM.ActG.STD.OIL.FLOW.RATE',
                    'FI.1E1425.P.1E.MPM.PRES.USED',                    
                    'FI.1E1425.T.1E.MPM.Temperature.Used')             

d.mpm <- d.mpm[,names(d.mpm) %in% names.mpm.keep]
names(d.mpm)[grep('MPM.Gas.V.olumetric.Flow.Std',names(d.mpm))] <- 'MPM.STD.GAS.VOLUMETRIC.FLOW'
names(d.mpm)[grep('MPM.Std.FORMATION.GOR',names(d.mpm))] <- 'MPM.STD.FORMATION.GOR'
names(d.mpm)[grep('MPM.ActG.STD.GAS.FLOW.RATE',names(d.mpm))] <- 'MPM.STD.GAS.FLOW.RATE'
names(d.mpm)[grep('MPM.ActG.STD.H2O.FLOW.RATE',names(d.mpm))] <- 'MPM.STD.WATER.FLOW.RATE'
names(d.mpm)[grep('MPM.ActG.STD.OIL.FLOW.RATE',names(d.mpm))] <- 'MPM.STD.OIL.FLOW.RATE'
names(d.mpm)[grep('MPM.PRES.USED',names(d.mpm))] <- 'MPM.PRES.USED'
names(d.mpm)[grep('MPM.Temperature.Used',names(d.mpm))] <- 'MPM.TEMP.USED'

d.mpm[,-1] <- sapply(d.mpm[,-1], as.numeric)
d.mpm <- data.table:: setorder(d.mpm,DATE.TIME)


############# READ Test Separator  DATA ####################

# dir.tsep <- '/shared/AcousticsSensors/Acoustics_Alaska/Acoustics_R/Data_R'
# dir.tsep <- dir.dat
dir.tsep <-  "C:/Users/frmendes/Desktop/Conoco Phillps/Meter Data"

sfx.tsep <- 'well_test_1E'
files.tsep <- dir(path = dir.tsep, pattern = sfx.tsep, all.files = FALSE,
                  full.names = TRUE)

d.tsep <- read.csv(files.tsep[1], stringsAsFactors=FALSE, skip=1, header=F) %>% 
                                                                    f.make.names
d.tsep <- d.tsep[, -grep('BLANK|Does.Not.Exist',names(d.tsep))] # remove empty columns
for(file in files.tsep[-1]){
  d.temp <- read.csv(file,stringsAsFactors=FALSE, skip=1, header=F) %>% 
                                                                    f.make.names 
  d.temp <- d.temp[,  -grep('BLANK|Does.Not.Exist',names(d.temp))]
  d.tsep <- rbind(d.tsep, d.temp)
}
rm(d.temp)

names.tsep.keep <- c('DATE.TIME',
                     'TF1E_FORMGOR.1E.TSEP.PD.FORMATION.GOR',                   
                     'TF1E_GLR.1E.TSEP.PD.GLR',                                 
                     'TF1E_GOR.1E.TSEP.PD.GOR',                                 
                     'TF1E_STDGAS.1E.TSEP.PD.STD.GAS.FLOW.RATE',                
                     'TF1E_STDH2O.1E.TSEP.PD.STD.H2O.FLOW.RATE',                
                     'TF1E_STDOIL.1E.TSEP.PD.STD.OIL.FLOW.RATE',
                     'PI.1E5301.166.1E166.PROD.FLOWLINE.PRES',
                     'TI.1E7062.166.1E166.PROD.FLOWLINE.TEMP',                
                     'TF1E_WELL2TSEP.1E.TSEP.PD.WELL.IN.TEST' )                  

d.tsep <- d.tsep[,names(d.tsep) %in% names.tsep.keep]
names(d.tsep)[grep('TSEP.PD.FORMATION.GOR',names(d.tsep))] <- 'TSEP.PD.FORMATION.GOR'
names(d.tsep)[grep('TSEP.PD.GOR',names(d.tsep))] <- 'TSEP.PD.GOR'
names(d.tsep)[grep('TSEP.PD.GLR',names(d.tsep))] <- 'TSEP.PD.GLR'
names(d.tsep)[grep('TSEP.PD.STD.GAS.FLOW.RATE',names(d.tsep))] <- 'TSEP.PD.STD.GAS.FLOW.RATE'
names(d.tsep)[grep('TSEP.PD.STD.H2O.FLOW.RATE',names(d.tsep))] <- 'TSEP.PD.STD.WATER.FLOW.RATE'
names(d.tsep)[grep('TSEP.PD.STD.OIL.FLOW.RATE',names(d.tsep))] <- 'TSEP.PD.STD.OIL.FLOW.RATE'
names(d.tsep)[grep('PROD.FLOWLINE.PRES',names(d.tsep))] <- 'TSEP.PROD.FLOWLINE.PRES'
names(d.tsep)[grep('PROD.FLOWLINE.TEMP',names(d.tsep))] <- 'TSEP.PROD.FLOWLINE.TEMP'
names(d.tsep)[grep('WELL2TSEP.1E.TSEP.PD.WELL.IN.TEST',names(d.tsep))] <- 'WELL.ID'

d.tsep <- filter(d.tsep, !(grepl('\\.',WELL.ID))) ## remove fractional well.id

d.tsep[,-1] <- sapply(d.tsep[,-1], as.numeric)
d.tsep <- data.table:: setorder(d.tsep,DATE.TIME)


############# join MPM and TSEP #############

d.mpm.tsep <- inner_join(d.mpm, d.tsep, by='DATE.TIME')
d.mpm.tsep <- unique(d.mpm.tsep) # remove duplicated rows

saveRDS(d.mpm.tsep, 'Generated_R_Data/d.mpm.tsep.rds')



















#############WE DO NOT USE ANY WELL CONTROL VARIABLES################
# IGNORE FROM HERE TILL END
############# READ WELL DATA ############# 

# dir.well <- '/shared/AcousticsSensors/Acoustics_Alaska/Acoustics_R/Data_R'
dir.well <- "C:/Users/frmendes/Desktop/Conoco Phillps/Meter Data/well"

sfx.well <- 'well_[0-9]'
files.well <- dir(path = dir.well, pattern = sfx.well, all.files = FALSE,
                  full.names = TRUE)

## make initial d.well
d.well <- read.csv(files.well[1],stringsAsFactors = FALSE, header = F , skip =1) %>% f.make.names

## keep only PRESSURE and TEMPERATURE variables
cols.keep.str <- 'PROD.FLOWLINE.PRES|PROD.FLOWLINE.TEMP|INJ.PRES|INJ.TEMP|DATE.TIME'
d.well <- d.well[grep(cols.keep.str,names(d.well))]
names(d.well)[grep('PROD.FLOWLINE.PRES|INJ.PRES',names(d.well))] <- 'WELL.PROD.FLOWLINE.PRES'
names(d.well)[grep('PROD.FLOWLINE.TEMP|INJ.TEMP',names(d.well))] <- 'WELL.PROD.FLOWLINE.TEMP'

## extract well.id from filename
well.id <- basename(files.well[1]) %>%  gsub('(^.+_)(\\d+)(_.+$)', '\\2',.) %>% as.numeric
d.well$WELL.ID <- well.id

## rbind all well data files, will join later by=c(DATE.TIME,WELL.ID) with d.mpm.tsep
if(length(files.well) > 1){
  for(file in files.well[-1])
  {
    well.temp <- read.csv(file,stringsAsFactors = FALSE, header = F , skip =1) %>% f.make.names
    well.temp <- well.temp[grep(cols.keep.str,names(well.temp))]
    
    names(well.temp)[grep('PROD.FLOWLINE.PRES|INJ.PRES',names(well.temp))] <- 'WELL.PROD.FLOWLINE.PRES'
    names(well.temp)[grep('PROD.FLOWLINE.TEMP|INJ.TEMP',names(well.temp))] <- 'WELL.PROD.FLOWLINE.TEMP'
    
    well.id <- basename(file) %>%  gsub('(^.+_)(\\d+)(_.+$)', '\\2',.) %>% as.numeric
    well.temp$WELL.ID <- well.id
    
    d.well <- dplyr:: bind_rows(d.well, well.temp)
  }
}
d.well[,-1] <- sapply(d.well[,-1], as.numeric)


############# join MPM, tsep, and well data ############# 

d.mpm.tsep.well <- left_join(d.mpm.tsep, d.well, by=c('DATE.TIME','WELL.ID'))
d.mpm.tsep.well <- unique(d.mpm.tsep.well) # remove duplicated rows


## well data may be missing for some dates for which  mpm and test data are available, 
## get a joined file with complete rows only
d.mpm.tsep.well.complete <- d.mpm.tsep.well[complete.cases(d.mpm.tsep.well),]

saveRDS(d.mpm.tsep.well, 'Generated_R_data/d.mpm.tsep.well.rds')
saveRDS(d.mpm.tsep.well.complete, 'Generated_R_data/d.mpm.tsep.well.complete.rds')


############# loess smooth joined data ############# 

d.mpm.tsep <- readRDS('d.mpm.tsep.rds')

## smooth joined MPM and tsep data
d.mpm.tsep.smooth <- d.mpm.tsep

span <- 0.0025

ptm <- proc.time()
for(iname in names(d.mpm.tsep[-grep('DATE.TIME|WELL.ID', names(d.mpm.tsep))])){
  
  smooth.loess <- loess(y~x, data=data.frame(y=d.mpm.tsep[,iname], 
                                             x=as.numeric(d.mpm.tsep$DATE.TIME)),
                                                                    span=span)
  smooth.y <- smooth.loess$fitted
  smooth.y[smooth.y<0] <- 0
  
  # i.plot=48000 : 55000
  # plot(x=d.mpm.tsep$DATE.TIME[i.plot], y=d.mpm.tsep[i.plot,iname], type='l', col='black', main=iname)
  # lines(x=d.mpm.tsep$DATE.TIME[i.plot], y=smooth.y[i.plot], type='l', col='blue')
  
  d.mpm.tsep.smooth[,iname] <- smooth.y
}
proc.time() - ptm

saveRDS(d.mpm.tsep.smooth, paste0('d.mpm.tsep.smooth.span',toString(span),'.rds'))


## loess smooth joined file with complete cases

d.mpm.tsep.well.smooth <- d.mpm.tsep.well.complete

for(iname in names(d.mpm.tsep.well.smooth[-grep('DATE.TIME|WELL.ID',
                                                      names(d.mpm.tsep.well.smooth))])){
  
  smooth.loess <- loess(y~x, data=data.frame(y=d.mpm.tsep.well.complete[,iname], 
                                             x=as.numeric(d.mpm.tsep.well.complete$DATE.TIME)), 
                                                                    span=span)
  smooth.y <- smooth.loess$fitted
  smooth.y[smooth.y<0] <- 0
  
  # i=10000 : 11000
  # plot(x=d.mpm.tsep.well.complete$DATE.TIME[i], y=d.mpm.tsep.well.complete[i,iname], type='l', col='black', main=iname)
  # lines(x=d.mpm.tsep.well.complete$DATE.TIME[i], y=smooth.y[i], type='l', col='blue')
  
  d.mpm.tsep.well.smooth[,iname] <- smooth.y
}

saveRDS(d.mpm.tsep.well.smooth, paste0('d.mpm.tsep.well.smooth.span',toString(span),'.rds'))

df.y.test