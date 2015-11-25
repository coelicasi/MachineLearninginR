owls<-read.csv("owls15.csv")#read in owls data
#creates a sample of size owls.data and sets 2/3 to 1, 1/3 to 2, randomly
set.seed(10)
int<- sample(2,nrow(owls), replace=TRUE, prob=c(0.67, 0.33))
#set the training/testing
owls.training<-owls[int==1,1:5]#includes all columns
owls.testing<-owls[int==2, 1:4]#only includes first 4 columns
owls.testLabels<-owls[int==2, 5]
owls.trainLabels<-owls[int==1, 5]

owls.pop<-nrow(owls.training)#total population of the training set

b<-table(owls.training$type=="BarnOwl")
s<-table(owls.training$type=="SnowyOwl")
l<- table(owls.training$type=="LongEaredOwl")

#ent <- - (b/owls.pop * log2(b/owls.pop))

totEntropy <-  -((b[2:2]/owls.pop)*log2(b[2:2]/owls.pop)) - ((b[1:1]/owls.pop)*log2(b[1:1]/owls.pop))
                 - ((s[2:2]/owls.pop)*log2(s[2:2]/owls.pop)) - ((s[1:1]/owls.pop)*log2(s[1:1]/owls.pop))
                 - ((l[2:2]/owls.pop)*log2(l[2:2]/owls.pop)) - ((l[1:1]/owls.pop)*log2(l[1:1]/owls.pop))

#takes the averages of the attributes to use in entropy calculations
greater<-apply(owls.training[1:4], 2, function(x) length(x[mean(x)>x]))
less<-apply(owls.training[1:4], 2, function(x) length(x[mean(x)<x]))
treeDF <- list()

entropy<-function(owls.training){
  #stop cases
  if(length(owls.training==0)){
    print("Out of training data!")
  }else if(duplicated(owls.training[, 5])){
    print("All available classes are the same!")
  }else{
  for(i in owls.pop){#for all in the training data
    for (i in owls.training[1:4]){#for each attribute
      #H(s) = E p * log2 (p)
      H <- -((less/owls.pop)* log2(less/owls.pop)) - ((greater/owls.pop)*log2(greater/owls.pop))
      
      gain<- totEntropy - H
      
      splitter <- names(which.max(gain))#splits data
      split <- mean(splitter)
      
      DF <- data.frame(owls.training[1:4])
      drops <- c(splitter)
      newD <- DF[, !(names(DF) %in% drops)] #remaining data
      treeDF<-data.frame(split, entropy(newD))
      
      }
  }

  }
}
  
entropy(owls.training)
print(treeDF)
