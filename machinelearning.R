owls<-read.csv("owls15.csv")#read in owls data
#creates a sample of size owls.data and sets 2/3 to 1, 1/3 to 2, randomly
set.seed(10)
i<- sample(2,nrow(owls), replace=TRUE, prob=c(0.67, 0.33))
#set the training/testing
owls.training<-owls[i==1,1:5]#includes all columns
owls.testing<-owls[i==2, 1:4]#only includes first 4 columns
owls.testLabels<-owls[i==2, 5]

owls.pop<-nrow(owls.training)
b<-table(owls.training$type=="BarnOwl")
s<-table(owls.training$type=="SnowyOwl")
l<- table(owls.training$type=="LongEaredOwl")

ent <- - (b/owls.pop * log2(b/owls.pop))

totEntropy <-  -((b[2:2]/owls.pop)*log2(b[2:2]/owls.pop) - (b[1:1]/owls.pop)*log2(b[1:1]/owls.pop))
                 - ((s[2:2]/owls.pop)*log2(s[2:2]/owls.pop) - (s[1:1]/owls.pop)*log2(s[1:1]/owls.pop))
                 - ((l[2:2]/owls.pop)*log2(l[2:2]/owls.pop) - (l[1:1]/owls.pop)*log2(l[1:1]/owls.pop))

#takes the averages of the attributes to use in entropy calculations
greater<-apply(owls.training[1:4], 2, function(x) length(x[mean(x)>x]))
less<-apply(owls.training[1:4], 2, function(x) length(x[mean(x)<x]))


entropy<-function(owls.training){
  
  for (i in owls.training[1:4]){
    #H(s) = E p * log2 (p)
    H <- (-less/owls.pop)* log2(less/owls.pop) - ((greater/owls.pop)*log2(greater/owls.pop))
    gain<- totEntropy - H
  }
  
  #gain <- H[1] - (H[2]+H[3]+H[4])
  
}
