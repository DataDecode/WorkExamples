#Dependant on stats r package

#this function runs arima models from AR(0) to whatever AR or MA is set (preset is 4) 
#and produces a table of AIC and Log Likelihood outputs from all the models run.

#The AIC and Log Likelihood tables are able to be examined outside of the function as well
#so it does not need to be run again to see the results.

runArima<- function(data, ar = 4, ma = 4, diff = 0){
  aic_table <<- as.data.frame(matrix(0,nrow=ar+1,ncol=ma+1))  
  #lik_table <<- as.data.frame(matrix(0,nrow=ar+1,ncol=ma+1)) 
  
  for (i in 1:ar){ #writing the MA(0) column
    ar_only <- arima(data, order=c(i,diff,0))
    aic_table[i+1,1] <<- ar_only$aic
    #lik_table[i+1,1] <<- ar_only$loglik
  }
  for (j in 1:ar){ #writing the AR(0) row
    ma_only <- arima(data, order=c(0,diff,j))
    aic_table[1,j+1] <<- ma_only$aic
    #lik_table[1,j+1] <<- ma_only$loglik
  }
  
  for (i in 1:ar){ #filling in the rest of the table
    for(j in 1:ma){
      ar<- arima(data, order= c(i,diff,j), optim.control = list(maxit = 1000))
      aic_table[i+1,j+1]<<- ar$aic
      #lik_table[i+1,j+1]<<- ar$loglik
    }
  }
  print('AIC TABLE')
  print(aic_table)
  #print('LOG LIKELIHOOD TABLE')
  #print(lik_table)
  print ('For both tables, AR is in the rows and MA is in the columns starting with AR(0), MA(0), which is listed as 0.000')
}