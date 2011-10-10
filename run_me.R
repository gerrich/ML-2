trn<-read.csv("train.csv",header=F)
tst<-read.csv("test.csv",header=F)
source("booster.R")
m<-uv_boost_learn(trn[,1:40],trn[,41],50) # last parametr - number of boosting iterations, set 1 to get fast results
r<-uv_boost_predict(m, tst[,1:40])
write.csv(r,"ans07.txt",row.names=F)

#after this remove first line from output file and submit

#to check quality:
r<-uv_boost_predict(m, trn[,1:40])
sum(r==trn[,41])

#see the matter of errors
plot(r+runif(r),trn[,41]+runif(r))

