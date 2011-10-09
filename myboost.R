require(rpart)

my_learn<-function(features, classes, weights) {
  data<-features
  label=TRUE #classes[[1]]  
 
    bool_classes = (classes == label)
    data$classes<-bool_classes

    c1<-length(bool_classes[bool_classes %in% T])
    c0<-length(bool_classes[bool_classes %in% F])
    local_weights=weights*(bool_classes*(c0-c1) + c1)/(c1+c0)/2;

    # model<-glm(classes~.-classes, data, weights=local_weights, family=binomial)
    cntrl<-rpart.control(maxdepth = 1 , minsplit=length(bool_classes), maxsurrogate = 0, usesurrogate=0, maxcompete = 1,cp = 0, xval = 0)
    model<-rpart(classes~.-classes, data, weights=local_weights, control = cntrl)

  return(list(model,local_weights))
}

my_predict<-function(model, features) {
  # result<-(predict.glm(model, features) > 0.5)
  result<-(predict(model, features) > 0.5)
  return(result)
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
    # print(error)
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
