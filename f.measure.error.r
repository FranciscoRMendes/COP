#### f.measure.error.r

#### calculatre R2, MAPE, and RMSE

  f.measure.error <- function(y, yhat){

  mape = round(sum(abs(y-yhat))/sum(y)*100,digits=1)
  
  rsquared=round((1-(sum((y-yhat)^2)/sum((y-mean(y))^2))),digits=2)
  
  rmse<-round(sqrt(mean((y-yhat)^2)),digits=0)
  v.fit <- c(R2=rsquared, MAPE=mape, RMSE=rmse)
  
  return (v.fit)
  }
