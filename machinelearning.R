owls<-read.csv("owls15.csv")
set.seed(4)#a number of R's random number generator
ind<-sample(2,nrow(owls), replace=TRUE, prob=c(0.67, 0.33))#sample() takes a sample with a size as big as owls
#chooose from a vestor of 2 elements, 1 or 2, and set each row to one. 
owls.training<-owls[ind==1, 1:5]#all rows set as 1 are for training, 2/3 of set
#taking in random rows for testing/training. Predictions set on last columns
owls.testing<-owls[ind==2, 1:4]#all rows with 2 are set for testing
owls.trainLabel<-owls[ind==1, 5]
owls.testLabel<-owls[ind==2,5]

meanOwls<-by(owls[, 1:4], owls.training$type, colMeans)

test<-by(owls.training, list(type=owls$type), function(x){
    y<-subset(x, select=-type)
    apply(y, 2, mean)
})
#minBarn<-apply(owls.barn, min)
ls()#maxBarn<-apply(owls.barn, 2, max)

#takes in the training set and outputs a prediction of trainLabel
model<-function(owls.training, owls.trainLabel){
  
  if(length(owls.training) == 0){
    #END 
    print("empty training set")
  }else if(duplicated(owls.trainLabel)){
    print("all classes the same")
  }
    #end
  else if (length(owls.trainLabel)==0){
    print("No labels")
    #end
  }
  
  else
    
    
}
