owls<-read.csv("owls15.csv")
set.seed(4)#a number of R's random number generator
ind<-sample(2,nrow(owls), replace=TRUE, prob=c(0.67, 0.33))#sample() takes a sample with a size as big as owls
#chooose from a vestor of 2 elements, 1 or 2, and set each row to one. 
owls.training<-owls[ind==1, 1:5]#all rows set as 1 are for training, 2/3 of set
#taking in random rows for testing/training. Predictions set on last columns
owls.testing<-owls[ind==2, 1:4]#all rows with 2 are set for testing
owls.trainLabel<-owls[ind==1, 5]
owls.testLabel<-owls[ind==2,5]

crit1<-owls.training$type=="BarnOwl"
crit2<-owls.training$type=="LongEaredOwl"
crit3<-owls.training$type=="SnowyOwl"
col<-c(1:4)
barn<-data.frame(owls.training[crit1, col])
snowy<-data.frame(owls.training[crit2, col])
longear<-data.frame(owls.training[crit3, col])
barn.max<-lapply(barn, max)
barn.min<-lapply(barn, min)
barn.mean<-lapply(barn, mean)

a<-mean(owls.training$wing.length)

totE<- -(34/86)*log2(34/86) - (28/86)*log2(28/86) - (24/86)*log2(24/86)


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
    #calulate entropy. Total 86 in training. H(X) = sum((p)log2(p))
    #body length
  for(i in owls.training[1:4]){
    
  }
    
}

entropy<-function(owls.training){
  
  count<-table(owls.training$body.length>mean(owls.training$body.length))
  count2<-table(owls.training$body.width>mean(owls.training$body.width))
  count3<-table(owls.training$wing.length>mean(owls.training$wing.length))
  count4<-table(owls.training$wing.width>mean(owls.training$wing.width))
  
  H <- c(- count[2:2]/nrow(owls.training) * log2 (count[2:2]/nrow(owls.training))) - count[1:1]/nrow(owls.training) * log2 (count[1:1]/nrow(owls.training))
  H2 <- c(- count2[2:2]/nrow(owls.training) * log2 (count2[2:2]/nrow(owls.training))) - count2[1:1]/nrow(owls.training) * log2 (count2[1:1]/nrow(owls.training))
  H3 <- c(- count3[2:2]/nrow(owls.training) * log2 (count3[2:2]/nrow(owls.training))) - count3[1:1]/nrow(owls.training) * log2 (count3[1:1]/nrow(owls.training))
  H4 <- c(- count4[2:2]/nrow(owls.training) * log2 (count4[2:2]/nrow(owls.training))) - count4[1:1]/nrow(owls.training) * log2 (count4[1:1]/nrow(owls.training))
}

gain<-function(entropy){
  
  df<-data.frame("body.length"=H,"body.width"= H2,"wing.length"= H3,"wing.width"= H4)
  IG <- H - ((H2+H3+H4)/3)
  IG2 <- H2 - ((H+H3+H4)/3)
  IG3 <- H3 - ((H2+H+H4)/3)
  IG4 <-H4 - ((H2+H3+H)/3)
  df2 <- data.frame(IG, IG2, IG3, IG4)
  
}
