rm(list=ls())
source('feature_engineering.R')
source('data_source.R')

library(xgboost)
library(gbm)
library(h2o)
library(caret)
library(dplyr)
library(glmnet)

#splitting into training and evaluation sets-for ensembling
set.seed(138538)
ind=sample(1:nrow(data),floor(nrow(data)/4))
data_eval=data[ind,]
data_tr=data[-ind,]
data_eval.lm=data.lm[ind,]
data_tr.lm=data.lm[-ind,]
feature.names=setdiff(colnames(data_tr),c('Sales','log.Sales'))
feature.names.lm=setdiff(colnames(data_tr.lm),c('Sales','log.Sales'))
    
# RANDOM FORESTS using H2O--insanely fast
local=h2o.init(nthreads=2,max_mem_size='5G')
data_r=as.h2o(data.matrix(data_tr))
rn_clf=h2o.randomForest(feature.names,
                    'log.Sales',
                    training_frame=data_r,
                    ntrees=50,
                    max_depth=20,
                    nbins_cats = 1115                    
                   )


# LINEAR REGRESSION

#lm.models=list()
#for (i in 1:1115)
#{  x=data_tr.lm[data_tr.lm$Store==i,]
#   features=lapply(feature.names.lm,function(y) { if (nrow(unique(x[y]))>1) return (y)})
#   features=paste(unlist(features),collapse="+")
#   formula=paste("log.Sales~",features)
   #print(head(x))
#   lm.models=c(lm.models,list(lm(formula,x)))
#}
#rm(list=c('x','features','formula','i'))




# XGBOOST
data_xg=xgb.DMatrix(data_tr[,feature.names],
                    label=data_tr[,'log.Sales'])
eval_ind=sample(1:nrow(data_eval),10000)
data_eval.xgb=xgb.DMatrix(data_eval[eval_ind,feature.names],
                          label=data_eval[eval_ind,'log.Sales'])

watchlist=list(val=data_eval.xgb,train=data_xg)

# error function
RMPSE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<-expm1(as.numeric(labels))
  epreds<-expm1(as.numeric(preds))
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMPSE", value = err))
}

# training model
params=list(subsample=0.8,max_depth=9,eta=0.04,colsample_bytree=0.7,nthread=3)
xg_clf=xgb.train(objective = "reg:linear", 
                 booster = "gbtree",
                 params=params,
                 data=data_xg,                
                 nrounds=2200,
                 #watchlist=watchlist,
                 early.stopping.rounds=10,
                 feval=RMPSE
                )



# ENSEMBLING
#predicting using random forest
data_eval.r=as.h2o(data_eval)
e.rn=h2o.predict(rn_clf,data_eval.r)
e.rn=as.data.frame(e.rn)
colnames(e.rn)="e.rn"

# predicting using xgboost
e.xgb=predict(xg_clf,xgb.DMatrix(data_eval[,feature.names]))

# using adaboost to blend both models
#data_eval1=cbind(data_eval,e.rn,e.xgb)
#clf_ens=gbm(Sales~.,data=data_eval1,n.trees=400,cv.folds=5)

# using xgboost to blend both models
data_eval1=cbind(data_eval,e.rn,e.xgb)
data_eval2=xgb.DMatrix(data.matrix(data_eval1[,c(feature.names,'e.rn','e.xgb')]),label=data_eval[,'log.Sales'])

watchlist=list(train=data_eval2)
params=list(subsample=0.8,max_depth=6,eta=0.04,colsample_bytree=0.7,nthread=3)
ens_clf=xgb.train(params=params,
                 data=data_eval2,                
                 nrounds=1500,
                 watchlist=watchlist,
                 early.stopping.rounds=10,
                 feval=RMPSE
                )

# using random forests to blend both models
#data_eval2=as.h2o(cbind(data_eval,e.rn,e.xgb))
#clf_ens=h2o.randomForest(feature.names,
#                    'log.Sales',
#                    training_frame=data_eval2,
#                    ntrees=300,
#                    max_depth=30,
#                    nbins_cats = 1115                    
#                   )

#PREDICTION

#predicting using random forest
p.rn=h2o.predict(rn_clf,as.h2o(test))
p.rn=as.data.frame(p.rn)
colnames(p.rn)="e.rn"
p.rn1=as.vector(p.rn)

#prediction using linear regression
#for (i in 1:1115)
#    { x=test.lm[test.lm$Store==i,]
#      p=unlist(predict(lm.models[i],x))
#      p=data.frame(Id=x$Id,p.lm=expm1(p))
#      if (i==1) p.lm=p
#      else  p.lm=rbind(p.lm,p)
#  }

# predicting using xgboost

p.xgb=predict(xg_clf,xgb.DMatrix(data.matrix(test[,feature.names])))

# bledning both prediction using adaboost--this is awesome
#test3=cbind(test3,e.rn=p.rn1,e.xgb=p.xgb)
#p=expm1(predict(clf_ens,test3,n.trees=800))

# blending both prediction using randomforest--this is awesome
#test3=cbind(test3,e.rn=p.rn1,e.xgb=p.xgb)
#p=expm1(as.vector(h2o.predict(clf_ens,as.h2o(test3))))

# blending both prediction using xgb--this is awesome
test.xg=xgb.DMatrix(data.matrix(cbind(test,e.rn=p.rn1,e.xgb=p.xgb)))
p=expm1(predict(ens_clf,test.xg))


# final prediction
p=expm1(p.rn1)*.11666/.231925+expm1(p.xgb)*.115265/.231925
predict=data.frame(Id=test[,ncol(test)],Sales=p)
colnames(predict)=c("Id","Sales")
write.csv(predict,'submission_ensemble_new.csv',row.names=F)

h2o.shutdown()






