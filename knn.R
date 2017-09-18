library(class) #Has the knn function
set.seed(4948493) #Set the seed for reproducibility
#Sample the Iris data set (70% train, 30% test)
indices<-sample(1:nrow(iris),size=nrow(iris)*.7)
traindata<-iris[indices,] #Select the 70% of rows
testdata<-iris[-indices,] #Select the 30% of rows

iris_acc<-numeric() #Holding variable
 
for(i in 1:50){
 #Apply knn with k = i
 predict<-knn(traindata[,-5],testdata[,-5],
 traindata$Species,k=i)
 iris_acc<-c(iris_acc,
 mean(predict==testdata$Species))
}
#Plot k= 1 through 50
plot(1-iris_acc,type="l",ylab="Error Rate",
 xlab="K",main="Error Rate for Iris With Varying K")

trial_sum<-numeric(20)
trial_n<-numeric(20)
set.seed(6033850)
for(i in 1:100){
 indices<-sample(1:nrow(iris),size=nrow(iris)*.7)
 traindata<-iris[indices,]
 testdata<-iris[-indices,]
 test_size<-nrow(testdata)
 for(j in 1:20){
 predict<-knn(traindata[,-5],testdata[,-5],
 traindata$Species,k=j)
 trial_sum[j]<-trial_sum[j]+sum(predict==testdata$Species)
 trial_n[j]<-trial_n[j]+test_size
 }
}
 
plot(1-trial_sum / trial_n,type="l",ylab="Error Rate",
 xlab="K",main="Error Rate for Iris With Varying K (100 Samples)")
