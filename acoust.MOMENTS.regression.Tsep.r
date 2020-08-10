####  acoust.MOMENTS.regression.Tsep.r

#### fit Tsep, 
#### compare performance with MPM

require(R.matlab)
require(stringr)
require(dplyr)
require(lubridate)
require(glmnet)
require(ggplot2)
require(data.table)
require(zoo) # for rollmean
require(beepr)
library(doParallel)
library(doSNOW)
# source('f.loess.r')
# source('f.reg.well.out.r')
source('f.measure.error.r')

dir.mat<-'C:/Users/frmendes/Documents/dsamp_MOMENTS'

dir.mat<-'C:/Downsampled_100'

dir.mat<-'C:/Downsampled_100/Moments_100_ceps20_wl50'
dir.mat<-'C:/Users/frmendes/Documents/Downsampled_Moments_10/Downsampled_Moments'

# dir.mat <- 'C:/Users/mshapiro/Documents/MATLAB/Acoustic Sensors Matlab/Acoustics Matlab Sandbox/Data'
# dir.dat <-  './Data'

###################
#read in data here#
###################
d.reg<-readRDS('d.reg.moments.rds')

org.nrow<-nrow(d.reg)
nrow(d.reg)


###################
#DATA EDITING#
###################

# #REMOVE ALL BAD TESTS FROM TSEP (We do not use this for now!)
# test.qlty<-read.csv("C:/Users/frmendes/Desktop/Conoco Phillps/Meter Data/well_test2.csv")
# time<-strptime(test.qlty$DATETIME_TEST,format ="%d%b%Y:%T" )
# beg.time<-time - test.qlty$TEST_LENGTH*60*60
# 
# df<-d.reg
# for(i in 1:length(beg.time))
# {
#   df<- filter(df, !DATE.TIME %within% interval(beg.time[i], time[i] ))
#   print(i)
# }
# d.reg<-df

#Remove Oct 6th and 7th#########
nrow(d.reg)
d.reg<-filter(d.reg, !DATE.TIME %within% interval('2016-10-06 11:00:00','2016-10-07 8:20:00'  ))

print(paste0("We have removed ", org.nrow-nrow(d.reg)," rows"))
##################################################


## read d.mpm.tsep 
d.mpm.tsep <- readRDS('d.mpm.tsep.rds')



sfx <- 'MOMENTS'

files.mat <- dir(path = dir.mat, pattern = sfx, all.files = FALSE,
                 full.names = TRUE)

ix.names <- 1:21
mu.names <- sapply(ix.names, function(x) paste0('MOMENT.MU.',x))
sig.names <- sapply(ix.names, function(x) paste0('MOMENT.SIG.',x))
skew.names <- sapply(ix.names, function(x) paste0('MOMENT.SKEW.',x))
kurt.names <- sapply(ix.names, function(x) paste0('MOMENT.KURT.',x))

coeff.names <- c('DATE.TIME',mu.names,sig.names,skew.names,kurt.names)

################## read and rbind MOMENT files #####################
calculate.moments <- TRUE
if(calculate.moments){
  d.moments <- readMat(files.mat[1]) %>% as.data.frame(stringsAsFactors=FALSE)
  
  ptm <- proc.time()
  for(file in files.mat[-1]){
    d.temp <- readMat(file) %>% as.data.frame(stringsAsFactors=FALSE)
    # d.moments <- rbind(d.moments, d.temp)
    d.moments <- dplyr:: bind_rows(d.moments, d.temp)
  }
  proc.time() - ptm
  
  names(d.moments) <- coeff.names
  d.moments$DATE.TIME <- dmy_hms(d.moments$DATE.TIME) %>% floor_date('minute')
  d.moments <- d.moments[, c('DATE.TIME', head(names(d.moments),-1))]
  d.moments <- d.moments[!duplicated(d.moments$DATE.TIME),]
  
  saveRDS(d.moments, 'd.moments.10.rds')
} else {
  ## join mpm.tsep and moments data
  d.moments <- readRDS('d.moments.rds')
}

 
 d.moments<-readRDS('C:/Users/frmendes/Documents/d.moments.ceps20.100.wl1000.rds') 
  # d.moments<-readRDS('C:/Users/frmendes/Documents/cv.mfit.ceps21.10.wl1000.rds')
 
d.reg <- dplyr:: inner_join(d.moments, d.mpm.tsep, by='DATE.TIME')

nrow(d.reg)
d.reg<-filter(d.reg, !DATE.TIME %within% interval('2016-10-06 11:00:00','2016-10-07 8:20:00'  ))
nrow(d.reg)
print(paste0("We have removed ", org.nrow-nrow(d.reg)," rows"))
# saveRDS(d.reg, 'd.reg.moments.rds')

# i.plot = 45000:46000
# plot(d.mpm.tsep$DATE.TIME[i.plot], d.mpm.tsep$MPM.STD.OIL.FLOW.RATE[i.plot], 
#      main = 'MPM.STD.OIL.FLOW.RATE', type='l', col='black')
# plot(d.reg$DATE.TIME[i.plot], d.reg$MPM.STD.OIL.FLOW.RATE[i.plot], 
#       main = 'MPM.STD.OIL.FLOW.RATE', type='l', col='blue')

############## Regression

################## partition to train/validation, and final test

### set aside test data for 6in pipe from Aug-01 to Aug-04, 13:00 pm
d.test.6in <- filter(d.reg, DATE.TIME %within% interval('2016-08-01 00:00:00', '2016-08-04 12:50:00' )) 

### partition the 4in pipe data into Train/Validation and Test (10%, last 1 week)
d.4in <- filter(d.reg, DATE.TIME %within% interval('2016-08-04 14:00:00', tail(d.reg$DATE.TIME,1)))

ix.model <- 1:nrow(d.4in) # 1:floor(0.9 * nrow(d.4in))
d.model <- d.4in [ix.model,] ## use to train, cv
d.test.4in <- d.4in[-ix.model,] ## set aside for the final test

# saveRDS(d.test.6in, 'd.test.6in.rds')
# saveRDS(d.4in, 'd.4in.rds')
# saveRDS(d.model,'d.model.rds')
# saveRDS(d.test.4in,'d.test.4in.rds')
dev.new()
# d.model <- readRDS('d.model.rds')
i.plot = 40000:41000
plot(d.model$DATE.TIME[i.plot], d.model$MPM.STD.OIL.FLOW.RATE[i.plot], 
     main = 'MPM.STD.OIL.FLOW.RATE', type='l')
## sanity check that the d.model has the same data as d.reg
lines(d.reg$DATE.TIME[d.reg$DATE.TIME %in% d.model$DATE.TIME[i.plot]],
      d.reg$MPM.STD.OIL.FLOW.RATE[d.reg$DATE.TIME %in% d.model$DATE.TIME[i.plot]], 
      type='l', col='green')    

## splits % of the sample size
test.splits <- c(0.25, 0.20, 0.10)
# for(nsplit in test.splits[2]) {
nsplit <- test.splits[2]
n.train <- floor((1-nsplit) * nrow(d.model))
ix.train <- 1 : n.train

## set the seed to make the partition reproductible
# set.seed(42)
# ix.train <- sample(seq_len(nrow(d.model)), size = n.train) %>% sort

########## ##### Choose predictor variables ##### ##### ##### ##### 
### 'MOMENT.MU.','MOMENT.SIG.', 'MOMENT.SKEW.','MOMENT.KURT.'

bool <- c(1,1,1,1) %>% as.logical
text <- c("MOMENT.MU","MOMENT.SIG","MOMENT.SKEW","MOMENT.KURT")

# for(n.moments in 1:4){
#  bool[1:n.moments] <-TRUE
  sfx.x <- paste(text[bool],collapse="|")
  sfx.save <- gsub('\\|(MOMENT)',"",sfx.x)  # %>% paste0(.,"_span",span,sep="")
  sfx.x
  sfx.save
  
  names.x <- c(names(d.model)[grep(sfx.x,names(d.model))], 'TSEP.PROD.FLOWLINE.PRES', 'TSEP.PROD.FLOWLINE.TEMP')
  
  #### Choose response variables#### Choose response variables#### Choose response variables
  names.mpm.y <- c(
    'MPM.STD.GAS.FLOW.RATE',      
    'MPM.STD.WATER.FLOW.RATE',
    'MPM.STD.OIL.FLOW.RATE' ) 
  names.tsep.y <- c(
    'TSEP.PD.STD.GAS.FLOW.RATE',
    'TSEP.PD.STD.WATER.FLOW.RATE',
    'TSEP.PD.STD.OIL.FLOW.RATE'  )
  
  ####   ####   ####   ####   ####   #### 
  meter  <- 'tsep'
  names.y <- names.tsep.y ##[1]
  
  ## get y.lim for plotting Results
  d.ma <- rollmean(d.model[,names.y],241,align="right")
  y.lim.high <- setNames(apply(d.ma[,names.y],2,max), names.y)
  
  
  ###################### Plot all meter data d.4in
  ma.order <- 241 #moving average over 4 hours
  y.4in.ma <- rollmean(d.4in[,c(names.tsep.y,names.mpm.y)],ma.order,align="right")
  
  ## skip the first ma.order-1 points
  time.4in.ma <- d.4in$DATE.TIME %>%  tail(.,-(ma.order-1))
  d.4in.ma <- data.frame(DATE.TIME=time.4in.ma, y.4in.ma)
  y.lim.high.4in.mpm <- setNames(apply(d.4in[,c(names.mpm.y)],2,max), names.mpm.y)
  y.lim.high.4in.tsep <- setNames(y.lim.high.4in.mpm, names.tsep.y)
  y.lim.high.4in <- cbind(y.lim.high.4in.tsep, y.lim.high.4in.mpm)
  y.lim.high.4in.mpm.ma <- setNames(apply(d.4in.ma[,c(names.mpm.y)],2,max), names.mpm.y)
  # y.lim.high.4in.tsep.ma <- setNames(y.lim.high.4in.ma, names.tsep.y)
  y.lim.high.4in.tsep.ma <-setNames(apply(d.4in.ma[,c(names.tsep.y)],2,max),names.tsep.y)
  y.lim.high.4in.ma <- cbind(y.lim.high.4in.mpm.ma, y.lim.high.4in.tsep.ma)
  ### plot smoothed Tsep, MPM together
  for(iname in names.tsep.y){
    i.name.displ <-  gsub('.FLOW.RATE', '', iname)
    component <- gsub('(^.+STD.)(.+$)', '\\2',i.name.displ)
 
     ggplot(d.4in.ma,aes(x = DATE.TIME,y = d.4in.ma[,iname])) + 
      geom_line(color='green3',size=1) +
      geom_line(data = d.4in.ma, aes(x = DATE.TIME,y = d.4in.ma[,grep(component, names.mpm.y, value=T)]),
                color = 'black', size=0.55, alpha=0.8) + 
      ggtitle(paste0(i.name.displ,', Tsep(green), MPM(black)')) + 
      xlab('Date') + ylab(i.name.displ) + 
      ylim(0, y.lim.high.4in.ma[iname])
    
    # ggsave(p, filename=paste0('Plot.Tsep.MPM_ma_',i.name.displ,'.png'), width = 7, height = 5)
  }
  ### plot individual compnents for Tsep, MPM
  for(iname in c(names.tsep.y,names.mpm.y)){
    i.name.displ <-  gsub('.FLOW.RATE', '', iname)
    component <- gsub('(^.+STD.)(.+$)', '\\2',i.name.displ)
    if(grepl('MPM', i.name.displ)){
      color <- 'black'
      size <- 0.75
      alpha <- 0.8
    } else {
      color <- 'green3'
      size <- 1
      alpha <- 1
    }
    
    p <- ggplot(d.4in.ma,aes(x = DATE.TIME,y = d.4in.ma[,iname])) + 
      geom_line(data = d.4in, aes(x = DATE.TIME,y = d.4in[,iname]),
                color = color, size=0.5, alpha=0.25) +
      geom_line(data = d.4in.ma, aes(x = DATE.TIME,y = d.4in.ma[,grep(component, names.mpm.y, value=T)]),
                color = color, size=size, alpha=alpha) + 
      ggtitle(i.name.displ) + 
      xlab('Date') + ylab(i.name.displ) + 
      ylim(0, y.lim.high.4in[grep(component,iname)])
    
    ggsave(p, filename=paste0('Plot.',i.name.displ,'.png'), width = 7, height = 5)
  }
  
  
  ####### call function for One-Well-Out CV
  wells <- table(d.model$WELL.ID) %>% sort(., decreasing=T) %>% dimnames %>% unlist %>% as.numeric
  names.fit.well <- c('WELL.OUT', paste0('R2_', names.y), paste0('MAPE_', names.y), 
                      paste0('RMSE_', names.y))
  d.fit.well.out <- setNames(data.frame(matrix(ncol = length(names.fit.well), nrow = 0)),
                             names.fit.well)
  
  
  ##################### Well.out CV  CHECK
  ##################### Well.out CV  CHECK
  if(nsplit==test.splits[1]){
    test.well.out <- TRUE  # calculate well.out once
  } else {
    test.well.out <- FALSE
  }
  
  test.well.out <- FALSE
  
  if(test.well.out){
    for(well.out in wells[wells!=31]){ ## well 31 has too short record
      print(well.out)
      ptm <- proc.time()
      d.fit.temp <- f.reg.well.out(d.model, well.out, sfx.x, sfx.save, 
                                   names.x, names.y, y.lim.high, meter)
      d.fit.well.out <- rbind(d.fit.well.out, d.fit.temp)
      proc.time() - ptm
      gc()
    }
    write.csv(d.fit.well.out, paste0('Results/FIT_Well.Out.ALL_',toupper(meter),
                                     '_', sfx.save,'.csv'), row.names=FALSE)
    rm(d.fit.temp)
  }
  
  # beep(sound = 8)
  ##################### Well.out CV  CHECK
  #####################  ##################### Well.out CV  CHECK
  ix.train<-as.integer(rownames(d.model[d.model$WELL.ID!=33,]))
  time.train <- d.model$DATE.TIME[ix.train]
  x.train <- d.model[ix.train, names.x] %>% as.matrix()
  y.train <- d.model[ix.train, names.y] %>% as.matrix()
  
  time.test <- d.model$DATE.TIME[-ix.train]
  x.test <- d.model[-ix.train, names.x] %>% as.matrix()
  y.test <- d.model[-ix.train, names.y] %>% as.matrix()
  
  # time.test<-time.train
  # x.test<-x.train
  # y.test<-y.train
  # 
  # nrow(x.test)
  # nrow(y.test)
  # length(time.test)
  
  
  for(name.var in names.y){
    plot(x=time.train, y=y.train[,name.var], main=name.var, col='blue', type='l')
  }
  # 
  # #####  fit glmnet
  # ptm <- proc.time()
  # if(length(names.y)>1){
  #   mfit <- glmnet(x.train, y.train , family='mgaussian')  ### multivariate
  #   # plot(mfit, xvar = 'lambda', label = TRUE, type.coef = '2norm')
  #   plot(mfit, xvar='dev', label = TRUE, type.coef = '2norm')
  # }else{
  #   mfit <- glmnet(x.train, y.train)   ### single
  #   # plot(mfit, xvar = 'lambda', label = TRUE)
  #   plot(mfit, xvar='dev', label = TRUE)
  # }
  # mfit
  # proc.time() - ptm
  
  # use description to add any additonal info to the filename
  description<- ""
  #### fit cv.glmnet model
  calculate.cv.mfit = FALSE
  
  if(calculate.cv.mfit){
    ptm <- proc.time()
    if(length(names.y)>1){
      cv.mfit <- cv.glmnet(x.train, y.train , family='mgaussian')  ### multivariate
    }else{
      cv.mfit <- cv.glmnet(x.train, y.train )  ### single
    }
    plot(cv.mfit)
    proc.time() - ptm
    saveRDS(cv.mfit, paste0('cv.mfit_', toupper(meter),
                            '_nsplit',nsplit,'_',sfx.save,description,'.rds'))
  } else {
    cv.mfit <- readRDS(paste0('Fit/cv.mfit_', toupper(meter),
                              '_nsplit',nsplit,'_',sfx.save,description,'.rds'))
  }
  
  cv.mfit<-readRDS("C:/Users/frmendes/Documents/Acoustic New Model/Acoustic New Model/cv.mfit_TSEP_nsplit0.2_MOMENT.MU.SIG.SKEW.KURT.RDS")
  
  cv.mfit <- cv.glmnet(x.train, y.train , family='mgaussian')
  # saveRDS(cv.mfit,'cv.mfit.well33.downsampled.rds')
  cv.mfit<-readRDS('cv.mfit.ceps21.10.wl1000.rds')
  cv.mfit<-cv.mfit
  #### prediction using cv.glmnet
  ########## calculate prediction
  yhat.cv <- predict.cv.glmnet(cv.mfit, x.test, s='lambda.1se') 
  yhat.cv[yhat.cv<0] <- 0
 
  ## if peek into final tests
  time.test.4in <- d.test.4in$DATE.TIME
  x.test.4in <- d.test.4in[,names.x] %>% as.matrix()
  y.test.4in <- d.test.4in[,names.y] %>% as.matrix()
  
  time.test.6in <- d.test.6in$DATE.TIME
  x.test.6in <- d.test.6in[,names.x] %>% as.matrix()
  y.test.6in <- d.test.6in[,names.y] %>% as.matrix()
  
  test.name <- 'model'
  if(test.name=='4in'){
    time.test <- time.test.4in 
    x.test <- x.test.4in
    y.test <- y.test.4in
  }else if(test.name=='6in'){
    time.test <- time.test.6in 
    x.test <- x.test.6in
    y.test <- y.test.6in
  }
  
  ########## calculate prediction
  yhat.cv <- predict.cv.glmnet(cv.mfit, x.test, s='lambda.1se') 
  yhat.cv[yhat.cv<0] <- 0
  
  ## quick look at the fit
  if(length(names.y)>1){
    for(i in 1:ncol(y.test)){
      plot(y.test[,i], type='l', main=dimnames(y.test)[[2]][i],  col='green', lwd=2,
           ylim=c(min(y.test[,i],yhat.cv[,i,]), max(y.test[,i],yhat.cv[,i,])))
      lines(yhat.cv[,i,], type='l', col='blue', lwd=2)
    }
  }else{
    plot(y.test[,i], type='l', main=dimnames(y.test)[[2]][i],  col='green', lwd=2,
         ylim=c(min(y.test[,i],yhat.cv[,i,]), max(y.test[,i],yhat.cv[,i,])))
    lines(yhat.cv[,i], type='l', col='blue', lwd=2)
  }  
  
  ## really close look at the fit
  ii <- 1000:1100
  ivar <- 1
  plot(x=time.test[ii], y=y.test[ii,ivar], col='green', type='l', lwd=2,main=dimnames(y.test)[[2]][ivar],
       ylim=c(min(y.test[ii,ivar],yhat.cv[ii,ivar,]), max(y.test[ii,ivar],yhat.cv[ii,ivar,]))) 
  lines(x=time.test[ii], yhat.cv[ii,ivar,], col='blue', type='l',lwd=2)
  
  #### smooth both test and prediction
  
  d.y.test <- data.frame(DATE.TIME=time.test, y.test) # %>% f.loess(., span)
  d.yhat.cv <- data.frame(DATE.TIME=time.test, yhat.cv) # %>% f.loess(., span)
  names(d.yhat.cv) <- gsub('.\\d+$','',names(d.yhat.cv))
  
  ma.order <- 241 #moving average over 4 hours
  y.test.ma <- rollmean(d.y.test[,-1],ma.order,align="right")
  yhat.cv.ma <- rollmean(d.yhat.cv[,-1],ma.order,align="right")
  
  ## skip the first ma.order-1 points
  time.ma <- d.y.test$DATE.TIME %>%  tail(.,-(ma.order-1))
  d.y.test.ma <- data.frame(DATE.TIME=time.ma, y.test.ma)
  d.yhat.cv.ma <- data.frame(DATE.TIME=time.ma, yhat.cv.ma)
  
  d.y.test <- d.y.test.ma
  d.yhat.cv <- d.yhat.cv.ma
  
  #############################################
  # add 'mpm' to compare performance
  y.mpm <- rollmean(d.model[,names.mpm.y],ma.order,align="right")
  ## skip the first ma.order-1 points to align averaged y.mpm and time
  time.mpm <- d.model$DATE.TIME %>%  tail(.,-(ma.order-1))
  d.mpm <- data.frame(DATE.TIME=time.mpm, y.mpm) 
  d.mpm <- filter(d.mpm, DATE.TIME %in% d.y.test$DATE.TIME)
  
  ##### Rsquared
  Rsquared<-list() 
  MAPE <- list()
  RMSE <- list()
  names.fit <- c('R2.acoust', 'MAPE.acoust', 'RSME.acoust', 'R2.mpm', 'MAPE.mpm', 'RSME.mpm' )
  d.fit <- setNames(data.frame(matrix(ncol = length(names.fit)+1, nrow = 0)),
                    c('COMPONENT', names.fit))
  
  for(iname in names(d.y.test[-which(names(d.y.test) %in% 
                                     c('DATE.TIME', 'WELL.ID'))])){
    component <- gsub('(^.+STD.)(.+$)', '\\2',iname)
    ## Rsquared
    fit.yhat <- f.measure.error(d.y.test[ ,iname], 
                                d.yhat.cv [ ,iname]) %>% as.list %>% data.frame
    fit.mpm <-  f.measure.error(d.y.test[ ,iname], 
                                d.mpm [ ,grep(component, names(d.mpm), value=T)]) %>% as.list %>% data.frame
    names(fit.yhat) <- paste0(names(fit.yhat),'.acoust')
    names(fit.mpm) <- paste0(names(fit.mpm),'.mpm')
    d.fit.temp <- data.frame(COMPONENT=iname) %>% cbind(., fit.yhat, fit.mpm)
    d.fit <- rbind(d.fit, d.fit.temp)
    
    error<-sum((d.y.test[ ,iname] - d.yhat.cv [ ,iname])^2)
    tss <- sum((d.y.test[ ,iname] - mean(d.y.test[,iname]))^2)
    Rsquared[[iname]] <- 1-(error/tss)
    RMSE[[iname]] <- sqrt(mean((d.y.test[ ,iname] - d.yhat.cv [ ,iname])^2))
    ## MAPE
    err.mape <- sum(abs((d.y.test[ ,iname] - d.yhat.cv [ ,iname])))
    MAPE[[iname]] <- err.mape / sum(d.y.test[ ,iname]) *100
  }
  
  d.fit 
  
  write.csv(d.fit, paste0('Well.126.', 
                          nsplit,'_Compare.', toupper(meter),'_',sfx.save,'.csv'))
  ####### PLOT and SAVE
  
  for(iname in names(d.y.test[-which(names(d.y.test) %in% 
                                     c('DATE.TIME', 'WELL.ID'))])){
    
    i.name.displ <-  gsub('.FLOW.RATE', '', iname)
    
    component <- gsub('(^.+STD.)(.+$)', '\\2',i.name.displ)
    fit.row <- filter(d.fit, d.fit$COMPONENT==iname)
    
    p <- ggplot(d.y.test,aes(x = DATE.TIME,y = d.y.test[,iname])) + 
      geom_line(color='green',size=1.5) +
      geom_line(data = d.yhat.cv, aes(x = DATE.TIME,y = d.yhat.cv[,iname]),
                color = 'blue', size=1.5) +
 
      geom_ribbon(aes(ymin=pmax(0,(d.yhat.cv[,iname]-RMSE[[iname]])), 
                      ymax=d.yhat.cv[,iname]+RMSE[[iname]]), fill='blue', alpha=0.1) +
      geom_line(data = d.mpm, aes(x = DATE.TIME,y = d.mpm[,grep(component, names(d.mpm), value=T)]),
                color = 'black', size=0.75) +        
      ggtitle(paste0(i.name.displ,
                     ', R2 = ' , round(fit.row$R2.acoust, digits=2),
                     '/',  round(fit.row$R2.mpm, digits=2), 
                     ', MAPE = ' , round(fit.row$MAPE.acoust, digits=1),
                     '/',  round(fit.row$MAPE.mpm, digits=1), 
                     ', RMSE = ' , round(fit.row$RMSE.acoust, digits=0),
                     '/',  round(fit.row$RMSE.mpm, digits=0))) + 
      xlab('') + ylab(i.name.displ) + ylim(0, y.lim.high[iname])
    
    ggsave(p, filename=paste0('Downsampled.10.wl1000_2',i.name.displ,'_split', 
                              nsplit,'_Compare.',sfx.save,'.png'), width = 7, height = 5)
  }
  
#   ####### Extract regression coefficients
#   
#   coef.cv.mfit <- coef(cv.mfit) 
#   d.coef.cv.mfit <- data.frame(as.matrix(coef.cv.mfit[[1]]))
#   
#   for( i in 2:length(coef.cv.mfit)){
#     d.coef.cv.mfit<- data.frame(d.coef.cv.mfit , data.frame(as.matrix(coef.cv.mfit[[i]])))
#   }
#   
#   names(d.coef.cv.mfit) <- names(d.y.test[-1])
#   d.coef.cv.mfit <- rbind(as.data.frame(Rsquared),as.data.frame(MAPE),as.data.frame(RMSE),
#                           d.coef.cv.mfit)
#   rownames(d.coef.cv.mfit)[1:3] <- c('R^2', 'MAPE', 'RMSE')
#   
#   # writing the coef file
#   write.csv(d.coef.cv.mfit, paste0('Plots/regression.MAvg_split', 
#                                    nsplit,'_', toupper(meter),'_',sfx.save,'.csv'))
#   
# #  } # for n.moments
  
  
  ########################CLIPPING############
  quant.cut<-quantile(d.reg$TSEP.PD.STD.OIL.FLOW.RATE,0.97)
  d.reg.2<-d.reg[d.reg$TSEP.PD.STD.OIL.FLOW.RATE<quant.cut,]
  print(paste("Rows dropped: ", nrow(d.reg)-nrow(d.reg.2)))
  ###########################################
  
  
  
  

  ########################GLM BOOST##########################
  library(mboost)
  library(dplyr)
  library(zoo)

  glmdata<-data.frame(y.train[,3],x.train)
  colnames(glmdata)[1]<-"TSEP.PD.STD.OIL.FLOW.RATE"
  gc()
  glm.fit<-glmboost(TSEP.PD.STD.OIL.FLOW.RATE~.,
                    data = glmdata,
                    center = TRUE,
                    family = Huber(d = 400),
                    control = boost_control(mstop = 1000, nu = 0.01 , trace = TRUE))
  gc()
  # yhat<-predict
  #
  glmdata.test<-data.frame(y.test[,3],x.test)
  colnames(glmdata.test)[1]<-"TSEP.PD.STD.OIL.FLOW.RATE"

  plot(glm.fit)
  yhat<-predict(glm.fit,newdata = glmdata.test)

  plot(yhat,type= 'l')
  plot(y.test[,3],type='l', col = 'green')
  lines(yhat,col = 'blue')

  d.y.test <- data.frame(DATE.TIME=time.test, y.test[,3]) # %>% f.loess(., span)
  d.yhat.glmboost <- data.frame(DATE.TIME=time.test,yhat) # %>% f.loess(., span)
  names(d.yhat.glmboost) <- gsub('.\\d+$','',names(d.y.test))

  ma.order <- 241 #moving average over 4 hours
  y.test.ma <- rollmean(d.y.test[,-1],ma.order,align="right")
  yhat.glmboost.ma <- rollmean(d.yhat.glmboost[,-1],ma.order,align="right")

  ## skip the first ma.order-1 points
  time.ma <- time.test %>%  tail(.,-(ma.order-1))
  d.y.test.ma <- data.frame(DATE.TIME=time.ma, y.test.ma)
  d.yhat.glmboost.ma <- data.frame(DATE.TIME=time.ma, yhat.glmboost.ma)

  d.y.test <- d.y.test.ma
  d.yhat.glmboost <- d.yhat.glmboost.ma

  plot(d.y.test$y.test.ma,type= 'l')
  lines(d.yhat.glmboost$yhat.glmboost.ma,col= 'red')

  f.measure.error(d.y.test$y.test.ma,d.yhat.glmboost$yhat.glmboost.ma)
  
  
  
  ####################TRAIN RANDOM FOREST############################
  ptm <- proc.time()
  rf.fit.3<-randomForest(x.train, y.train[,3],ntree=100,do.trace = TRUE,mtry = 50,nodesize = 1000)
  proc.time() - ptm
  
  ptm <- proc.time()
  rf.fit.2<-randomForest(x.train, y.train[,2],ntree=100,do.trace = TRUE,mtry = 20,nodesize = 1000)
  proc.time() - ptm
  
  ptm <- proc.time()
  rf.fit.1<-randomForest(x.train, y.train[,1],ntree=100,do.trace = TRUE,mtry = 20,nodesize = 1000)
  proc.time() - ptm
  
  yhat.rf.3<-predict(rf.fit.3,x.test)
  yhat.rf.2<-predict(rf.fit.2,x.test)
  yhat.rf.1<-predict(rf.fit.1,x.test)
  
  
  #### smooth both test and prediction
  
  d.y.test <- data.frame(DATE.TIME=time.test, y.test) # %>% f.loess(., span)
  d.yhat.rf <- data.frame(DATE.TIME=time.test, yhat.rf.1,yhat.rf.2,yhat.rf.3) # %>% f.loess(., span)
  names(d.yhat.rf) <- gsub('.\\d+$','',names(d.y.test))
  
  ma.order <- 241 #moving average over 4 hours
  y.test.ma <- rollmean(d.y.test[,-1],ma.order,align="right")
  yhat.rf.ma <- rollmean(d.yhat.rf[,-1],ma.order,align="right")
  
  ## skip the first ma.order-1 points
  time.ma <- time.test %>%  tail(.,-(ma.order-1))
  d.y.test.ma <- data.frame(DATE.TIME=time.ma, y.test.ma)
  d.yhat.rf.ma <- data.frame(DATE.TIME=time.ma, yhat.rf.ma)
  
  d.y.test <- d.y.test.ma
  d.yhat.rf <- d.yhat.rf.ma
  
  ###########################################
  
  f.measure.error(d.y.test$TSEP.PD.STD.OIL.FLOW.RATE,d.yhat.rf$TSEP.PD.STD.OIL.FLOW.RATE)
  f.measure.error(d.y.test$TSEP.PD.STD.WATER.FLOW.RATE,d.yhat.rf$TSEP.PD.STD.WATER.FLOW.RATE )
  f.measure.error(d.y.test$TSEP.PD.STD.GAS.FLOW.RATE,d.yhat.rf$TSEP.PD.STD.GAS.FLOW.RATE)
  ######################END RANDOM FOREST########################
