source("myboost.R")

mc_boost_learn<-function(features, classes, count, weights=rep(1/length(classes), length(classes))) {
  labels = unique(classes)
  models = list()
  for (index in 1:length(labels)) {
    label<-labels[index]
    bool_classes = (classes == label)

    model<-boost_learn(features, bool_classes, count, weights)   
 
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

ava_boost_learn<-function(features, classes, count) {
  labels = unique(classes)
  models = list()
  for (i in 1:length(labels)) {
    models[[i]]<-list() 
    names(models)[i]<-labels[i]     
    for (j in 1:i) {
      if (i!=j) {
        print(i)
        print("-")
        print(j)
        local_features<-features[classes %in% labels[c(i,j)],]
        bool_classes<-classes[classes %in% labels[c(i,j)]]==labels[i]
        models[[i]][[j]]<-boost_learn(local_features, bool_classes, count)
      }
    }
  } 
  return(models)
}

ava_boost_predict<-function(models, features) {
  labels<-names(models)
  res_mtx<-data.frame(matrix(nrow=length(features[,1]),ncol=length(labels),data=0))
  for (i in 1:length(labels)) {
    if (length(models[[i]]) == 0) {
      break
    }
    for (j in 1:length(models[[i]])) {
      model<-models[[i]][[j]]
      res<-boost_predict(model, features)<0.5
      res_mtx[,i]<-res_mtx[,i]+res
      res_mtx[,j]<-res_mtx[,j]-res
    }
  }
  classes<-labels[max.col(res_mtx)]
  return(classes)
}

