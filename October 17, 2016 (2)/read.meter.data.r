#### read.meter.data.R

require(stringr)
require(dplyr)
require(lubridate)
require(data.table)
require(ggplot2)

dir.dat <-  './Data'

f.make.names <- function(df){
  for( i in 1:length(df)){
    names(df)[i] <- str_trim(paste(df[1:3, i], collapse=' '))
  }
  names(df) <- str_replace_all(names(df), '-', '.')
  names(df) <- str_replace_all(names(df), '[()]', '')  
  df<- head(df[-(1:3), -length(df)], -2) 
  df$DATE.TIME <- mdy_hm(df$DATE.TIME)
  return(df)
}

f.time.fill <- function(df, time.start, time.end, dt.step=seconds(1), fill.value=0,.. ){
  df.out <- data.frame(Date = seq(from=time.start, to=time.end, by=dt.step))
  df.out[,names(df)[-1]] <- fill.value  # assumes the first column is df$Date
  df.out[df.out$Date %in% df$Date, -1] <- df[df$Date %in% df.out$Date,-1]
  return(df.out)
}

name.test <- 'well_on_test.csv'
fpath.test <- file.path(dir.dat, name.test, fsep = .Platform$file.sep)

test <- read.csv(fpath.test, stringsAsFactors = FALSE, skip=1)
test <- f.make.names(test)
names(test)[2] <- 'WELL.NO'
test <- filter(test, !(grepl('\\.',WELL.NO))) 
test$WELL.NO <-  as.integer(test$WELL.NO)


name.well <- 'well_33_sep14.csv'
fpath.well <- file.path(dir.dat, name.well, fsep = .Platform$file.sep)

well <- read.csv(fpath.well, stringsAsFactors = FALSE, skip=1)
well <- f.make.names(well)

name.mpm <- 'MultiphaseMeter_alltags_exceptAI.csv'
fpath.mpm <- file.path(dir.dat, name.mpm, fsep = .Platform$file.sep)

mpm <- read.csv(fpath.mpm, stringsAsFactors = FALSE, skip=1)
mpm <- f.make.names(mpm)

name.tsep <- 'testSep_16sep.csv'
fpath.tsep <- file.path(dir.dat, name.tsep, fsep = .Platform$file.sep)

tsep <- read.csv(fpath.tsep, stringsAsFactors = FALSE, skip=1)
tsep <- f.make.names(tsep)

d.mpm.tsep <- left_join(mpm, tsep, by='DATE.TIME')

mpm.tsep.33 <- dplyr:: filter(d.mpm.tsep, DATE.TIME %in% test$DATE.TIME[test$WELL.NO==33])
well.33 <- dplyr:: filter(well, DATE.TIME %in% test$DATE.TIME[test$WELL.NO==33])

d.33 <- left_join(mpm.tsep.33, well.33, by='DATE.TIME')
saveRDS(well.33, 'well.33.rds')

# edited using regexp, then copied and hard coded
names.edit <- c( "DATE.TIME","MPM.DENSITY.GAMMA" ,             
                 "MPM.GAS.VOLUMETRIC.FLOW.STD", "MPM.LIQUID.FLOW.ACT" ,           
                 "MPM.OIL.FLOW.ACT", "MPM.PRES.USED" ,           
                 "MPM.TEMPERATURE.USED", "MPM.WATERCUT.STD" ,               
                 "MPM.WATER.ACT", "MTR.STANDARD.OIL.VOLUME" ,        
                 "MTR.STANDARD.H2O.VOLUME", "MTR.STANDARD.GAS.VOLUME" ,        
                 "TSEP.MPM.GLR", "TSEP.MPM.GOR" ,                   
                 "TSEP.MPM.FORMATION.GOR", "TSEP.MPM.ACTG.STD.GAS.FLOW.RATE" ,
                 "TSEP.MPM.ACTG.STD.H2O.FLOW.RATE", "TSEP.MPM.ACTG.STD.OIL.FLOW.RATE" ,
                 "TSEP.WATERCUT", "TSEP.WELL.IN.TEST" ,             
                 "TSEP.GAS.FLOW.TOTAL", "TSEP.TOTAL.LIQUID.FLOW" ,         
                 "TSEP.DENSITY", "TSEP.WATER.FLOW" ,                
                 "TSEP.OIL.FLOW", "TSEP.MTR.STANDARD.GAS.VOLUME" ,   
                 "TSEP.MTR.STANDARD.H2O.VOLUME", "TSEP.MTR.STANDARD.OIL.VOLUME" ,   
                 "TSEP.LIFT.GAS.HDR.PRES", "TSEP.GAS.PRES" ,                  
                 "TSEP.TOTAL.GLR", "TSEP.GOR" ,                       
                 "TSEP.STD.GAS.FLOW.RATE", "TSEP.STD.WATER.FLOW.RATE" ,       
                 "TSEP.STD.OIL.FLOW.RATE", "TSEP.LIQ.TEMP" ,                  
                 "WL.GL.CPRIME", "WL.GL.ORIFICE.DP" ,               
                 "WL.GL.FLOW", "WL.PROD.FLOWLINE.PRES" ,          
                 "WL.LIFT.GAS.PRES", "WL.OA.PRES" ,                     
                 "WL.PROD.FLOWLINE.TEMP", "WL.ORIFICE.SIZE" ,                
                 "WL.CHOKE.POSITION.MANUAL.ENTRY") 

names(d.33) <- names.edit

# names.foo <- names(d.33)[-1 * c(10:15, 20, 26:28,21,24,25,5,9)]
# names.keep <- c("DATE.TIME",
#                 "MPM.DENSITY.GAMMA",			"TSEP.DENSITY" ,
#                 "TSEP.MPM.ACTG.STD.OIL.FLOW.RATE",	"TSEP.STD.OIL.FLOW.RATE" ,
#                 "TSEP.MPM.ACTG.STD.H2O.FLOW.RATE",	"TSEP.STD.WATER.FLOW.RATE" ,
#                 "MPM.LIQUID.FLOW.ACT",			"TSEP.TOTAL.LIQUID.FLOW" ,
#                 "MPM.WATERCUT.STD",			"TSEP.WATERCUT" ,
#                 "MPM.GAS.VOLUMETRIC.FLOW.STD",		"TSEP.MPM.ACTG.STD.GAS.FLOW.RATE" ,
#                 "TSEP.STD.GAS.FLOW.RATE",		"TSEP.TOTAL.GLR" ,
#                 "MPM.TEMPERATURE.USED",		"TSEP.LIQ.TEMP" ,
#                 "TSEP.LIFT.GAS.HDR.PRES",		"TSEP.GAS.PRES" ,
#                 "MPM.PRES.USED",	 	"TSEP.GOR",
#                 "WL.GL.CPRIME", "WL.GL.ORIFICE.DP", "WL.GL.FLOW" ,
#                 "WL.PROD.FLOWLINE.PRES", "WL.LIFT.GAS.PRES", "WL.OA.PRES" ,
#                 "WL.PROD.FLOWLINE.TEMP", "WL.ORIFICE.SIZE", "WL.CHOKE.POSITION.MANUAL.ENTRY")
                
names.keep <- c("DATE.TIME",
                "TSEP.DENSITY" ,
                "TSEP.STD.OIL.FLOW.RATE" ,
                "TSEP.STD.WATER.FLOW.RATE" ,
                "TSEP.TOTAL.LIQUID.FLOW" ,
                "TSEP.WATERCUT" ,
                "TSEP.STD.GAS.FLOW.RATE",		
                "TSEP.TOTAL.GLR" ,
                "TSEP.GAS.PRES" ,
                "TSEP.GOR",
                "MPM.DENSITY.GAMMA",
                "TSEP.MPM.ACTG.STD.OIL.FLOW.RATE",
                "TSEP.MPM.ACTG.STD.H2O.FLOW.RATE",
                "MPM.LIQUID.FLOW.ACT",
                "MPM.WATERCUT.STD",
                "TSEP.MPM.ACTG.STD.GAS.FLOW.RATE" ,
                "WL.GL.FLOW",
                "WL.PROD.FLOWLINE.PRES",
                "WL.PROD.FLOWLINE.TEMP",
                "WL.GL.CPRIME")

d.33[, -1] <- as.data.frame(lapply(d.33[, -1], as.numeric))

d.33.1 <- d.33[,names.keep]
d.33.1$TSEP.TOTAL.GLR[d.33.1$TSEP.TOTAL.GLR >25000] <- 0
d.33.2 <- d.33.1
## oil and water flow vars are misaligned from total flow by 2 minutes, realign
names.shift <- c( "TSEP.STD.OIL.FLOW.RATE" ,  # "TSEP.MPM.ACTG.STD.OIL.FLOW.RATE",
                  "TSEP.STD.WATER.FLOW.RATE"  # , "TSEP.MPM.ACTG.STD.H2O.FLOW.RATE",
                 )

d.33.2[1:(length(d.33.2[,1])-2), names.shift] <- d.33.2[3:(length(d.33.2[,1])), names.shift]
names(d.33.2) <- gsub('TSEP.MPM', 'MPM', names(d.33.2))
saveRDS(d.33.2, 'd.33.2.rds')
d.33.2 <- readRDS('d.33.2.rds')

d.33.smooth <- d.33.2

iname <- names(d.33.smooth)[2]
for(iname in names(d.33.smooth)[-1]){
  
  smooth.loess <- loess(y~x, data=data.frame(y=d.33.2[,iname], 
                                             x=as.numeric(d.33.2$DATE.TIME)), span=0.015)
  # plot(x=d.33.2$DATE.TIME, y=d.33.2[,iname], type='l', col='blue', main=iname)
  # lines(lowess(x=d.33.2$DATE.TIME, y=d.33.2[,iname], f=0.025), col='green', lwd=2)
  # lines(x=smooth.loess$x, y=smooth.loess$fitted, col='red',type='l', lwd=2)
  # 
  # l.var <- lowess(x=d.33.2$DATE.TIME, y=d.33.2[,iname], f=0.025)
  # smooth.y <- l.var$y
  # d.33.smooth[,name.smooth] <- l.var$y
  smooth.y <- smooth.loess$fitted

  d.33.smooth[,iname] <- smooth.y
}

saveRDS(d.33.smooth, paste0('d.33.smooth.LOESS_1min.rds'))
write.csv(d.33.smooth, paste0('d.33.smooth.LOESS_1min.csv'), row.names=FALSE)

#########################################################################################
#########################################################################################
#########################################################################################
## time.filled data
dt <- as.period(d.33.smooth$DATE.TIME[2] - d.33.smooth$DATE.TIME[1], '%OS3') 

tsamp <- 1 # seconds
dt.new <- seconds(tsamp) #sec
n <- ceiling(dt/dt.new) ## expand n-fold
d.33.fill <- d.33.smooth
d.33.fill[,] <- NA
d.33.fill <- as.data.frame(lapply(d.33.fill, function(x) rep(x,n)))

foo <- proc.time()
for(i in 1 : (length(d.33.smooth$DATE.TIME)-1) ){
  ### using list() preserves time zone info, unlike c() that changes to local time zone
  ## date.range <- list(d.33.smooth$DATE.TIME[i], d.33.smooth$DATE.TIME[i+1])
  time.start <- d.33.smooth$DATE.TIME[i]
  time.end <- d.33.smooth$DATE.TIME[i+1]
  ## in test separator, current minute reading reflects the past interval
  ## therefore fill the current interval with next minute values
  values.fill <- sapply(d.33.smooth[i+1, -1], function(x) rep(x,n)) 
  time.fill <- seq(time.start, time.end, by=dt.new)
  index.fill <- ((i-1)*n + 1) : (i*n) +1
  d.33.fill[index.fill,] <- cbind(time.fill[-1], values.fill)
}
proc.time() - foo

d.33.fill[1,] <- d.33.smooth[1,] 
d.33.fill <- d.33.fill[complete.cases(d.33.fill), ]
## convert to POSIX after complete.cases, otherwise error 
## "not all arguments have the same length"
d.33.fill$DATE.TIME <- as.POSIXlt(d.33.fill$DATE.TIME, origin='1970-01-01', tz=NULL)
if(as.numeric(dt.new)<1){
  d.33.fill$DATE.TIME.char <- str_sub(d.33.fill$DATE.TIME+0.1*dt.new, end=-2)
}else{
  d.33.fill$DATE.TIME.char <- as.character(d.33.fill$DATE.TIME)
}

saveRDS(d.33.fill, paste0('d.33.smooth.TSEP.MPM_', tsamp,'sec.rds'))
write.csv(d.33.fill, paste0('d.33.smooth.TSEP.MPM_', tsamp,'sec.csv'), row.names=FALSE)

# d.33.1s <- readRDS(paste0('d.33.TSEP.MPM.filled_', tsamp,'sec.rds'))
# 
# d.smooth <- d.33.fill
# 
# for(iname in names(d.smooth)[-1]){
# # plot(x=d.33.fill$DATE.TIME, y=d.33.fill[,iname], type='l', col='blue')
# # lines(lowess(x=d.33.fill$DATE.TIME, y=d.33.fill[,iname], f=0.025), col='green', lwd=2)
# l.var <- lowess(x=d.33.fill$DATE.TIME, y=d.33.fill[,iname], f=0.025)
# d.smooth[,iname] <- as.data.frame(l.var[2])
# }
# 
# saveRDS(d.smooth, paste0('d.smooth.TSEP.MPM.filled_', tsamp,'sec.rds'))
# write.csv(d.smooth, paste0('d.smooth.TSEP.MPM.filled_', tsamp,'sec.csv'), row.names=FALSE)
# 
# ### test stats
# tb.test <- as.data.table(test)
# well.dur <- tb.test[, .N, by=WELL.NO]
# p <- ggplot(well.dur, aes(x=factor(WELL.NO), y=N/60)) + 
#   geom_bar(stat='identity') +
#   geom_text(aes(label=as.integer(N/60)),position= position_dodge(width=0.9), vjust=-.5) +
#   geom_bar(data=well.dur[well.dur$WELL.NO==33,], aes(x=factor(WELL.NO), y=N/60), stat='identity', fill='blue') +
#   labs(title='Well on test (hrs)', x='Well number', y='Hours on test')
# 
# ggsave('Well.on.test.hrs.jpg', plot=p)
# 
# 
# ### all plots
# x.show <- 200:500 #1:length(d.33.1$DATE.TIME)
#   
# for( i in 2:length(names(d.33.1)) ){ #seq.int(from=2, to=18, by=2)){
# p <- ggplot(d.33.1[x.show,] , aes(x=DATE.TIME)) + 
#               geom_line(aes(y = d.33.1[x.show, i]), colour="blue") + 
# #              geom_line(aes(y = d.33.1[x.show,14]), colour='red') #+d.33.1[x.show+2, 9]), colour = "red") + 
#               labs(x='DATE.TIME', y=names(d.33.1)[i],
#                    title=paste0(names(d.33.1)[i]))
# #  invisible(readline(prompt="Press [enter] to continue"))
#                    
#   ggsave(paste0('PLOT.', names(d.33.1)[i] , '.jpg'), plot=p)
# }
# 
# 
# d.1s <- read.csv('final_smooth1sec.csv')
