source("mcboost.R")

classes_to_uvector<-function(res, labels) {
  res_mtx=data.frame(matrix(nrow=length(res),ncol=length(labels), 0))
  for (j in 1:length(labels)) {
    res_mtx[,j]<-(labels[j]==res)
  }
  return(res_mtx)
}

#uv_boost_learn<-function(features, classes, count, weights=rep(1/length(classes), length(classes))) {
uv_boost_learn<-function(features, classes, count) {
  weights=rep(1/length(classes), length(classes))
  class_num<-length(unique(classes))
  labels=unique(classes)
  steps<-list()
  for (index in 1:count) {
    m<-mc_boost_learn(features, classes, 15, weights)
    r<-mc_boost_predict(m,features)
    uvector<-classes_to_uvector(r, labels)
    
    cost<-rep(0,length(classes))
    for (j in 1:length(labels)) {
      cost<-cost+(labels[j]==classes)*uvector[,j]
      cost<-cost-(labels[j]!=classes)*uvector[,j]/(length(labels)-1)
    }

    error<-sum(-weights*cost*(cost<0))/sum(weights)
    factor=0.5*log((1-error)/error)
    if (factor<0) {
      break;
    }
    weights<-weights*exp(-factor*cost)/sum(weights)
    steps[[index]]<-list()
    steps[[index]][[1]]<-m
    steps[[index]][[2]]<-factor
  }
  steps[[1]][[3]]<-unique(classes)
  return(steps)
}

uv_boost_predict<-function(models, features) {
  labels<-models[[1]][[3]]
  res_mtx<-data.frame(matrix(nrow=length(features[,1]), ncol=length(labels), 0))
  for (index in 1:length(models)) {
    model<-models[[index]][[1]]
    factor<-models[[index]][[2]]
    res<-mc_boost_predict(model, features)
   
    for (j in 1:length(labels)) {
      res_mtx[res == labels[j],j]<-res_mtx[res == labels[j],j]+factor
    }  
  }
  classes<-labels[max.col(res_mtx)]
  return(classes)
}


ovo_learn<-function(features, classes, weights) {
  data<-features
  labels = unique(classes)
  models = list()
  for (index in 1:length(labels)) {
    label<-as.character(labels[index])
    bool_classes = (classes == label)
    data$classes<-bool_classes

    c1<-length(bool_classes[bool_classes %in% T])
    c0<-length(bool_classes[bool_classes %in% F])
    local_weights=weights*(bool_classes*(c0-c1) + c1)/(c1+c0)/2;

    # model<-lm(classes~.-classes, data, weights=local_weights)  
    cntrl<-rpart.control(maxdepth = 1 , minsplit=length(bool_classes), maxsurrogate = 0, usesurrogate=0, maxcompete = 1,cp = 0, xval = 0)
    model<-rpart(classes~.-classes, data, weights=local_weights, control = cntrl)
    
    models[[index]]<-list(model, local_weights)
    names(models)[index]<-label
  } 
   
  return(models)
}

ovo_predict<-function(models, features) {
  labels<-names(models)
  result<-list()
  for (index in 1:length(labels)) {
    label<-as.character(labels[index])
    model<-models[[index]][[1]]
    # result[[index]]<-predict.lm(model, features)
    result[[index]]<-(predict(model, features)>0.5)
    names(result)[index]<-label
  }
  return(result)
}


logit<-function(x){return(exp(x) / (1 + exp(x)))}

cv_test<-function(features, classes, count, learn=mc_boost_learn, predict=mc_boost_predict) {
  smpl<-sample(1:length(classes),length(classes)/2)
  f1<-features[smpl,]
  c1<-classes[smpl]
    
  f2<-features[-smpl,]
  c2<-classes[-smpl]

  m1<-learn(f1,c1,count)
  r1<-predict(m1,f2)
  m1<-NA
  m2<-learn(f2,c2,count)
  r2<-predict(m2,f1)
  m2<-NA
  return((sum(r2==c1)+sum(r1==c2))/length(classes))
}

