
learn<-function(features, classes, weights) {
  data<-features
  label=classes[[1]]  
 
    bool_classes = (classes == label)
    data$classes<-bool_classes

    c1<-length(bool_classes[bool_classes %in% T])
    c0<-length(bool_classes[bool_classes %in% F])
    local_weights=weights*(bool_classes*(c0-c1) + c1)/(c1+c0)/2;

    model<-lm(classes~.-classes, data, weights=local_weights)  
   
  return(model)
}

predict<-function(model, features) {
  result<-(predict.lm(model, features)>0.5)
  return(result)
}

boost_learn<-function(features, classes, count) {
  steps=list()
  weights<-rep(1/length(classes), length(classes))
  for(i in 1:count) {
    step=list()
    model<-learn(features, classes, weights)
    step[[1]]<-model
    res<-predict(model, features)
    error<-sum(weights*((res>0.5)!=classes))/sum(weights)
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
    res<-res+step[[2]]*predict(step[[1]], features)
    weight<-weight+step[[2]]
  }
  return(res/weight)
}
