
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

calc_error<-function(predict_matrix,classes, weights) {
  labels = names(predict_matrix)
  error = rep(0, length(classes))
  for(index in 1:length(labels)) {
    label<-as.character(labels[index])
    error = error + 1-logit((predict_matrix[[index]]*2-1)*((classes==label)*2-1))
  }
  return(error/length(labels))
}

calc_weights<-function(errors, weights) {
  summ_error<-sum(errors)/length(errors)
  factor<-0.5 * log((1-summ_error)/summ_error)
  sum_weight=sum(weights)
  new_weights<-weights*exp(-factor*(0.5-errors)*2)/sum_weight
  return(list(new_weights,factor))
}

boost_step<-function(features,classes,weights) {
  mm<-ovo_learn(features,classes,weights)
  res<-ovo_predict(mm,features)
  errors<-calc_error(res,classes,weights)
  wf<-calc_weights(errors, weights)
  return(list(mm,res,errors,wf[[1]],wf[[2]]))
}

boost_n<-function(features,classes,weights,count) {
  steps<-list()
  steps[[1]]<-list()
  steps[[1]][[4]]<-weights
  for (i in 2:(1+count)) {
    steps[[i]] <- boost_step(features,classes,steps[[i-1]][[4]])
  }
  res<-data.frame(steps[[2]][[2]])
  for (i in 3:(1+count)) {
    res=res+data.frame(steps[[i]][[2]])
  }
  return(res)
}

boost_learn<-function(features, classes, count) {
  steps=list()
  weights<-rep(1/length(classes), length(classes))
  for(i in 1:count) {
    step=list()
    model_plus<-my_learn(features, classes, weights)
    model<-model_plus[[1]]
    step[[1]]<-model
    res<-my_predict(model, features)

    local_weights<-model_plus[[2]]
    error<-sum(local_weights*((res>0.5)!=classes))/sum(local_weights)
    print(error)
    factor=0.5*log((1-error)/error)
    step[[2]]<-factor
    if (factor<0) {
      break;
    }
    weights=weights*exp(factor*(2*((res>0.5)!=classes)-1))/sum(weights)
    steps[[i]]<-step
  }
  return(steps)
}

boost_predict<-function(steps, features) {
  res<-rep(0,length(features[,1]))
  weight<-0
  for (step in steps) {
    res<-res+step[[2]]*my_predict(step[[1]], features)
    weight<-weight+step[[2]]
  }
  return(res/weight)
}
