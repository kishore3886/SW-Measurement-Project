random_Numbers
random_Numbers_median_Local<-0
c <- fn()
print(c)
fn <- function(){
  
  sorted_Random_Numbers<-random_Numbers
  #Decreasing order 
  for (i in 1:99)
  {
    for (j in 1:99)
    {
      if (sorted_Random_Numbers[j] > sorted_Random_Numbers[j+1])
      {
        swap = sorted_Random_Numbers[j];
        sorted_Random_Numbers[j]   = sorted_Random_Numbers[j+1];
        sorted_Random_Numbers[j+1] = swap;
      }
    }
  }
  sorted_Random_Numbers
  #sorting Done  
  s<-length(sorted_Random_Numbers)
  if((s %% 2) == 0){
    a<-s/2;
    b<-a+1
    random_Numbers_median_Local<-(sorted_Random_Numbers[a]+sorted_Random_Numbers[b])/2
  }
  if(!(s %% 2) == 0){
    a<-ceiling(s/2) ;
    random_Numbers_median_Local<-sorted_Random_Numbers[a]
  }
 print(random_Numbers_median_Local) 
 return(random_Numbers_median_Local)
}


