for all attributes that have not been used in the table, calculate their entropy and information gain values for the training samples
select the attribute that has the highest information gain
make a tree node containing that attribute
this node partitions the data: apply the algorithm recursively.

split()


BARNOWL
 body.length     wing.length      body.width      wing.width   
 Min.   :2.200   Min.   :5.000   Min.   :3.000   Min.   :1.000  
 1st Qu.:2.625   1st Qu.:5.525   1st Qu.:4.000   1st Qu.:1.225  
 Median :2.800   Median :5.950   Median :4.350   Median :1.300  
 Mean   :2.785   Mean   :5.921   Mean   :4.265   Mean   :1.335  
 3rd Qu.:3.000   3rd Qu.:6.250   3rd Qu.:4.600   3rd Qu.:1.500  
 Max.   :3.400   Max.   :6.900   Max.   :5.100   Max.   :1.700 
 
  ent.bodylength<- -38/86*log2(38/86) - 48/86*log2(48/86)
  for(i in barn){
    gain<-ent.bodylength - 5/34*log2(5/34) - 29/34*log2(29/34)
  }
    
