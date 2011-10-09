require("e1071")

mc_bayes_learn<-function(features, classes, weights=rep(1/length(classes), length(classes))) {
  data<-features;
  data$classes<-classes
  labels<-unique(classes)
  models<-list() 
  for (i in 1:length(labels)) {
    bool_classes<-classes==labels[i]
    data$classes<-bool_classes
    
    models[[i]]<-naiveBayes(classes~.-classes,data, weights=weights)
    names(models)[i]<-labels[i]
  }
  return(models)
}

mc_bayes_predict<-function(models, features) {

  labels<-names(models)
  res_mtx<-data.frame(matrix(nrow=length(features[,1]),ncol<-length(labels)))
  for (i in 1:length(labels)) {
    res_mtx[,i]<-predict(models[[i]], features)
  }
  return(res_mtx)
}
