#brt fitting and evaluation functions#
#Code written Primarily by H. Welch, adapted by MC and JJS

#for the following functions labeled 'fixed', number of trees and bag fraction follows those in 
#Hazen et al. 2021 Mov. Ecol.
#tree complexity reduced to minimize overfitting
#These were used for Cen_Cal_Seabird Models
eval_7525_BRT_Binomial <- function(dataInput, gbm.x, gbm.y, lr=lr, tc=tc){
  DataInput <- dataInput
  DataInput_bound <- floor((nrow(DataInput)/4)*3)         #define % of training and test set
  DataInput_train<- DataInput[sample(nrow(DataInput),DataInput_bound),]
  DataInput_test<- DataInput[sample(nrow(DataInput),nrow(DataInput)-DataInput_bound),]
  DataInput.kfolds <- gbm.step(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                               family="bernoulli", tree.complexity=tc,
                               learning.rate = lr, bag.fraction = 0.75, n.folds=4) #this was gbm.step
  preds <- predict.gbm(DataInput.kfolds, DataInput_test,
                       n.trees=DataInput.kfolds$gbm.call$best.trees, type="response")
  d <- cbind(obs=DataInput_test$PA, preds)
  pres <- d[d[,1]==1, 2]
  abs <- d[d[,1]==0, 2]
  e <- evaluate(p=pres, a=abs)
  e
  return(list(e, DataInput.kfolds))
}

#brt fitting and evaluation functions#
eval_7525_BRT_Binomial_Fixed <- function(dataInput, gbm.x, gbm.y, lr=lr, tc=tc){
  DataInput <- dataInput
  DataInput_bound <- floor((nrow(DataInput)/4)*3)         #define % of training and test set
  DataInput_train<- DataInput[sample(nrow(DataInput),DataInput_bound),]
  DataInput_test<- DataInput[sample(nrow(DataInput),nrow(DataInput)-DataInput_bound),]
  DataInput.kfolds <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                                family="bernoulli", tree.complexity=tc,
                                learning.rate = lr,n.trees = 2000, bag.fraction = 0.75) 
  preds <- predict.gbm(DataInput.kfolds, DataInput_test,
                       n.trees=DataInput.kfolds$gbm.call$best.trees, type="response")
  d <- cbind(obs=DataInput_test$PA, preds)
  pres <- d[d[,1]==1, 2]
  abs <- d[d[,1]==0, 2]
  e <- evaluate(p=pres, a=abs)
  e
  return(list(e, DataInput.kfolds))
}
fit.brt.n50_eval <- function(data, gbm.x, gbm.y, lr,tc, iterations){
  Species_Models <- vector("list",50)
  Species_Models_Eval <- vector("list",50)
  
  for (i in 1:iterations){
    model <- eval_7525_BRT_Binomial(data=data, gbm.x= gbm.x, gbm.y=gbm.y,lr=lr, tc=tc)
    
    Species_Models[[i]] <- model[[2]]    
    Species_Models_Eval[[i]]<-model[[1]]
  }
  return(list(Species_Models, Species_Models_Eval))
}

fit.brt.n50_eval_Fixed <- function(data, gbm.x, gbm.y, lr,tc, iterations){
  Species_Models <- vector("list",50)
  Species_Models_Eval <- vector("list",50)
  
  for (i in 1:iterations){
    model <- eval_7525_BRT_Binomial_Fixed(data=data, gbm.x= gbm.x, gbm.y=gbm.y,lr=lr, tc=tc)
    
    Species_Models[[i]] <- model[[2]]    
    Species_Models_Eval[[i]]<-model[[1]]
  }
  return(list(Species_Models, Species_Models_Eval))
}

eval_7525_BRT_Binomial_Agg <- function(dataInput, gbm.x, gbm.y, lr=lr, tc=tc){
  DataInput <- dataInput
  DataInput_bound <- floor((nrow(DataInput)/4)*3)         #define % of training and test set
  DataInput_train<- DataInput[sample(nrow(DataInput),DataInput_bound),]
  DataInput_test<- DataInput[sample(nrow(DataInput),nrow(DataInput)-DataInput_bound),]
  DataInput.kfolds <- gbm.step(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                               family="bernoulli", tree.complexity=tc,
                               learning.rate = lr, bag.fraction = 0.75, n.folds=4) #this was gbm.step
  preds <- predict.gbm(DataInput.kfolds, DataInput_test,
                       n.trees=DataInput.kfolds$gbm.call$best.trees, type="response")
  d <- cbind(obs=DataInput_test$Agg, preds)
  pres <- d[d[,1]==1, 2]
  abs <- d[d[,1]==0, 2]
  e <- evaluate(p=pres, a=abs)
  e
  return(list(e, DataInput.kfolds))
}

eval_7525_BRT_Binomial_Agg_Fixed <- function(dataInput, gbm.x, gbm.y, lr=lr, tc=tc){
  DataInput <- dataInput
  DataInput_bound <- floor((nrow(DataInput)/4)*3)         #define % of training and test set
  DataInput_train<- DataInput[sample(nrow(DataInput),DataInput_bound),]
  DataInput_test<- DataInput[sample(nrow(DataInput),nrow(DataInput)-DataInput_bound),]
  DataInput.kfolds <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                               family="bernoulli", tree.complexity=tc,
                               learning.rate = lr,n.trees = 2000, bag.fraction = 0.75) #, n.folds=4) this was gbm.step
  preds <- predict.gbm(DataInput.kfolds, DataInput_test,
                       n.trees=DataInput.kfolds$gbm.call$best.trees, type="response")
  d <- cbind(obs=DataInput_test$Agg, preds)
  pres <- d[d[,1]==1, 2]
  abs <- d[d[,1]==0, 2]
  e <- evaluate(p=pres, a=abs)
  e
  return(list(e, DataInput.kfolds))
}


fit.brt.n50_eval_Agg <- function(data, gbm.x, gbm.y, lr,tc, iterations){
  Species_Models <- vector("list",50)
  Species_Models_Eval <- vector("list",50)
  
  for (i in 1:iterations){
    model <- eval_7525_BRT_Binomial_Agg(data=data, gbm.x= gbm.x, gbm.y=gbm.y,lr=lr, tc=tc)
    
    Species_Models[[i]] <- model[[2]]    
    Species_Models_Eval[[i]]<-model[[1]]
  }
  return(list(Species_Models, Species_Models_Eval))
}



fit.brt.n50_eval_Agg_Fixed <- function(data, gbm.x, gbm.y, lr,tc, iterations){
  Species_Models <- vector("list",50)
  Species_Models_Eval <- vector("list",50)
  
  for (i in 1:iterations){
    model <- eval_7525_BRT_Binomial_Agg_Fixed(data=data, gbm.x= gbm.x, gbm.y=gbm.y,lr=lr, tc=tc)
    
    Species_Models[[i]] <- model[[2]]    
    Species_Models_Eval[[i]]<-model[[1]]
  }
  return(list(Species_Models, Species_Models_Eval))
}
library(dplyr)
library(caret)
library(mlbench)
LOO_eval_heather <- function(DataInput, gbm.x, gbm.y, lr=lr, tc,tolerance.method,tolerance,family,response){
  if(family=="bernoulli"){
    Evaluations_LOO <- as.data.frame(matrix(data=0,nrow=length(unique(DataInput$Year)),ncol=10))
    colnames(Evaluations_LOO) <- c("Year","Deviance","AUC","TSS","TPR","TNR","AveragePredAbs","AveragePredPres","N. test points","% left out")
  }
  counter=1
  for (y in min(DataInput$year):max(DataInput$year)){
    print(y)
    DataInput_train <- DataInput[DataInput$year!=y,]
    DataInput_test <- DataInput[DataInput$year==y,]
    DataInput.loo <- gbm.step(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                               family="bernoulli", tree.complexity=tc,
                               learning.rate = lr, bag.fraction = 0.75, n.folds = 4 )
    preds <- predict.gbm(DataInput.loo, DataInput_test,
                         n.trees=DataInput.loo$gbm.call$best.trees, type="response")
    position=grep(response,names(DataInput))
    # dev <- calc.deviance(obs=DataInput_test[position], pred=preds, calc.mean=TRUE)
    d <- cbind(DataInput_test[[response]], preds)
    test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
    pres <- d[d[,1]==1,2]
    abs <- d[d[,1]==0,2]
    if( length(abs)>0 & length(pres)>0){
      e <- evaluate(p=pres, a=abs)
      thresh=threshold(e)$equal_sens_spec
      ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
      Evaluations_LOO[counter,1] <- y
      Evaluations_LOO[counter,2] <- NA
      Evaluations_LOO[counter,3] <- e@auc
      Evaluations_LOO[counter,4] <- max(e@TPR + e@TNR-1)
      Evaluations_LOO[counter,5] <- e@TPR[ind]
      Evaluations_LOO[counter,6] <- e@TNR[ind]
      Evaluations_LOO[counter,7] <- test[1,2]
      Evaluations_LOO[counter,8] <- test[2,2]
      Evaluations_LOO[counter,9] <- nrow(DataInput_test)
      Evaluations_LOO[counter,10] <- nrow(DataInput_test)/nrow(DataInput)
      counter=counter+1
      rm(DataInput_test)
    }
    else{
      Evaluations_LOO[counter,1] <- y
    Evaluations_LOO[counter,2] <- NA
    Evaluations_LOO[counter,3] <- NA
    Evaluations_LOO[counter,4] <- NA
    Evaluations_LOO[counter,5] <- NA
    Evaluations_LOO[counter,6] <- NA
    Evaluations_LOO[counter,7] <- NA
    Evaluations_LOO[counter,8] <- NA
    Evaluations_LOO[counter,9] <- NA
    Evaluations_LOO[counter,10] <- NA
    counter=counter+1
    rm(DataInput_test)
    }}
  return(Evaluations_LOO)}

LOO_eval_heather_Fixed <- function(DataInput, gbm.x, gbm.y, lr=lr, tc,tolerance.method,tolerance,family,response){
  if(family=="bernoulli"){
    Evaluations_LOO <- as.data.frame(matrix(data=0,nrow=length(unique(DataInput$Year)),ncol=10))
    colnames(Evaluations_LOO) <- c("Year","Deviance","AUC","TSS","TPR","TNR","AveragePredAbs","AveragePredPres","N. test points","% left out")
  }
  counter=1
  for (y in min(DataInput$year):max(DataInput$year)){
    print(y)
    DataInput_train <- DataInput[DataInput$year!=y,]
    DataInput_test <- DataInput[DataInput$year==y,]
    DataInput.loo <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                              family="bernoulli", n.trees=2000, tree.complexity=tc,
                              learning.rate = lr, bag.fraction = 0.75 )
    preds <- predict.gbm(DataInput.loo, DataInput_test,
                         n.trees=2000, type="response")
    position=grep(response,names(DataInput))
    # dev <- calc.deviance(obs=DataInput_test[position], pred=preds, calc.mean=TRUE)
    d <- cbind(DataInput_test[[response]], preds)
    test=as.data.frame(d) %>% group_by(V1) %>% summarise(mean(preds))
    pres <- d[d[,1]==1,2]
    abs <- d[d[,1]==0,2]
    if( length(abs)>0 & length(pres)>0){
      e <- evaluate(p=pres, a=abs)
      thresh=threshold(e)$equal_sens_spec
      ind=which(abs(e@t-thresh)==min(abs(e@t-thresh)))
      Evaluations_LOO[counter,1] <- y
      Evaluations_LOO[counter,2] <- NA
      Evaluations_LOO[counter,3] <- e@auc
      Evaluations_LOO[counter,4] <- max(e@TPR + e@TNR-1)
      Evaluations_LOO[counter,5] <- e@TPR[ind]
      Evaluations_LOO[counter,6] <- e@TNR[ind]
      Evaluations_LOO[counter,7] <- test[1,2]
      Evaluations_LOO[counter,8] <- test[2,2]
      Evaluations_LOO[counter,9] <- nrow(DataInput_test)
      Evaluations_LOO[counter,10] <- nrow(DataInput_test)/nrow(DataInput)
      counter=counter+1
      rm(DataInput_test)
    }
    else{
      Evaluations_LOO[counter,1] <- y
      Evaluations_LOO[counter,2] <- NA
      Evaluations_LOO[counter,3] <- NA
      Evaluations_LOO[counter,4] <- NA
      Evaluations_LOO[counter,5] <- NA
      Evaluations_LOO[counter,6] <- NA
      Evaluations_LOO[counter,7] <- NA
      Evaluations_LOO[counter,8] <- NA
      Evaluations_LOO[counter,9] <- NA
      Evaluations_LOO[counter,10] <- NA
      counter=counter+1
      rm(DataInput_test)
    }}
  return(Evaluations_LOO)}
