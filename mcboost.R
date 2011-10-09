source("myboost.R")

mc_boost_learn<-function(features, classes, count) {
  labels = unique(classes)
  models = list()
  for (index in 1:length(labels)) {
    label<-labels[index]
    bool_classes = (classes == label)

    model<-boost_learn(features, bool_classes, count)   
 
    models[[index]]<-model
    names(models)[index]<-label
  } 
  return(models)
}

mc_boost_predict<-function(models, features) {
  labels<-names(models)
  res_mtx<-data.frame(matrix(nrow=length(features[,1]),ncol=length(labels),data=0))
  for (index in 1:length(labels)) {
    model<-models[[index]]
    res_mtx[,index]<-boost_predict(model, features)
  }
  classes<-labels[max.col(res_mtx)]
  return(classes)
}
