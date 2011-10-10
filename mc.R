require(rpart)

mc_learn<-function(features, classes, count,weights=rep(1/length(classes),length(classes))) {
#  factors=sample(1:length(features[1,]), length(features[1,])/2)
#print(factors)
  data<-features#[factors]
  labels=unique(classes)
  size<-length(labels)
# print(labels) 
  data_model<-list()
  random_list<-list()
  for (i in 1:(100*size)) {
    random_list[[i]]<-rbinom(size,1,0.5)
  }
#print(length(random_list))
  rlist<-sample(unique(random_list), count*size)
  bool_mtx<-data.frame(matrix(nrow=length(classes), ncol=(length(rlist)+1), F))
  for (i in 1:length(rlist)) {
    includes = rlist[[i]]
    while (sum(includes==T)<=1 | sum(includes==F)<=1) {
      includes = rbinom(size,1,0.5)
    }
    in_labels<-labels[includes %in% c(T)]
    #print(includes)
    #print(in_labels)
    bool_classes<-(classes %in% in_labels)
    data$classes<-bool_classes

    c1<-sum(bool_classes)
    c0<-sum(bool_classes==F)

    local_weights<-rep(c1/(c1+c0),length(classes))
    local_weights[bool_classes]<-c0/(c1+c0)  
    local_weights<-weights

    #cntrl<-rpart.control(maxdepth = count, minsplit=10+length(bool_classes)/(2**count), maxsurrogate = 0, usesurrogate=0, maxcompete = 1,cp = 0, xval = 0)
    data_model[[i]]<-rpart(classes~.-classes, data, weights=local_weights)#, control=cntrl)
    #data_model[[i]]<-glm(classes~.-classes, data, weights=local_weights, family="binomial")
    bool_mtx[,i]<-predict(data_model[[i]],features)
    #bool_mtx[,i]<-as.double(bool_classes)
  }
  bool_model<-list()
  for (i in 1:size) {
    names(bool_mtx)[length(rlist)+1]<-"R"
    bool_classes<-(classes==labels[i])
    bool_mtx$R<-bool_classes

    c1<-sum(bool_classes)
    c0<-sum(bool_classes==F)

    local_weights<-rep(c1/(c1+c0),length(classes))
    local_weights[bool_classes]<-c0/(c1+c0)  
    local_weights<-weights

#print(i)
#print(bool_mtx)
    bool_model[[i]]<-glm(R~.-R, data=bool_mtx, weights=local_weights, family="binomial")
  }
  return(list(data_model, bool_model, labels))
}

mc_predict<-function(model, features, mode="class") {
  data_model<-model[[1]]
  bool_model<-model[[2]]
  labels<-model[[3]] 
  size<-length(labels)
  bool_mtx<-data.frame(matrix(nrow=length(features[,1]), ncol=length(data_model), F))
  for (i in 1:length(data_model)) {
    bool_mtx[,i]<-predict(data_model[[i]], features)
  }
  
  ans_mtx<-data.frame(matrix(nrow=length(features[,1]), ncol=size, F))
  for (i in 1:size) {
    m<-bool_model[[i]]
    ans_mtx[,i]<-predict(m, bool_mtx)
  }
  if(mode=="raw") {
    return(ans_mtx,labels)
  } else {
    return(labels[max.col(ans_mtx)])
  }
} 
