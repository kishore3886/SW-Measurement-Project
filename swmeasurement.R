#Measurement Project  

# seed genereates random number
# this random number becomes seed for next random number

#middle square method

digits<-4
seed<-9999
random_Numbers_median_Local<-0

start=0
end=0
#rand_num_df<- data.frame(number=numeric())
rand_num_df<-numeric(100)

rand_generater<- function(){
  
  #print(seed)
  for(i in 1:100)
  {
    seed_square<-seed*seed
    seed_square_string<- toString(seed_square) 
    size<-nchar(seed_square_string)
    if(size/2!=0)
    {
      seed_square_string=paste("0",seed_square_string) 
      size=size+1
    }
    else
    {}
    start = floor(size/2)
    end_seed = start+digits-1
    end=start+1
    #for seed to the next random numer generating
    seed_char<-substr(seed_square_string, start, end_seed)
    #random number
    rand_number_char<-substr(seed_square_string, start, end)
    print(seed_char)
    seed<-as.numeric(seed_char)
    rand_Local<- as.numeric(rand_number_char)
    
    rand_num_df [i]<-rand_Local    
  }
   return(rand_num_df)
  
}

cal_Minimum<-function(){
  min_Number_Local= random_Numbers[3]
  for(i in 1:100)
  {
    #print(i)
    #rint(random_Numbers[i])
    if(random_Numbers[i]<min_Number_Local)
    {
      min_Number_Local= random_Numbers[i]
    }
    
  }
  return(min_Number_Local)
  
}
cal_Maximum<- function(){
  max_Number_Local=0
  for(i in 1:100)
  {
    
    if(random_Numbers[i]>max_Number_Local)
    {
      max_Number_Local<- random_Numbers[i]
    }
    else{}
    
  }
  return(max_Number_Local)
}

cal_Median_Random<- function(){
  
  random_Numbers_median_Local=0
  #s<- length(random_Numbers)
  #to find median first sort the vector 
  #copied into other data frame , as we will be performing manipulations on global variable
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
#sorting Done  
  if((s %% 2) == 0){
    a<-s/2;
    b<-a+1
    random_Numbers_median_Local=(sorted_Random_Numbers[a]+to_sort[b])/2
  }
  if(!(s %% 2) == 0){
    a<-ceiling(s/2) ;
    random_Numbers_median_Local=sorted_Random_Numbers[a]
  }
  
  return(random_Numbers_median_Local)
}
cal_Mean<- function(){
  sum_Random_Numbers=0
  random_Numbers_Mean_local=0
  for(i in 1:100)
  {
    sum_Random_Numbers<-sum_Random_Numbers+random_Numbers[i]
    
  }
  
  random_Numbers_Mean_local <- sum_Random_Numbers/100
  
  return(random_Numbers_Mean_local)
}
cal_StandardDeviation<-function(){
  
  standardDeviation_random_numbers=0
  variance_random_numbers=0
  for(i in random_Numbers)
    variance_random_numbers =variance_random_numbers+ ((i - random_Numbers_Mean)^ 2);
  
  standardDeviation_random_numbers= sqrt(variance_random_numbers/100)
  
}
cal_Mode<- function(){
  maxCount <- 0;
  modeValue <- 0;
  
  sorted_Random_Numbers<-random_Numbers
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
  tally<-numeric(100)
  for (i in 1:99) 
  {
    for(z in 1:99)
    {
      
      if(sorted_Random_Numbers[i]==sorted_Random_Numbers[z])
      {
        tally[i]=tally[i]+1
      }
    }
  }
  
    for (i in 1:100) 
  {
    if (tally[i] > maxCount) 
    {
      maxCount <- tally[i];
      modeValue <- sorted_Random_Numbers[i];
    }
  }
  print(maxCount)
  return(modeValue)
}
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



#starts here----------------------------------------------------------------------

random_Numbers<- rand_generater()
min_Number<-cal_Minimum()
max_Number<-cal_Maximum()
random_Numbers_Mean<-cal_Mean()
random_Numbers_StandardDeviation<-cal_StandardDeviation()
random_Numbers_Mode<-cal_Mode()
random_Numbers_Median<-fn()

random_Numbers
sorted_Random_Numbers
    min_Number
    max_Number
    random_Numbers_Mean
    random_Numbers_StandardDeviation
    random_Numbers_Mode
    random_Numbers_Median
  #Testing the results------------------------------------------------  
    
    boxplot(random_Numbers)
  #to show all numbers are generated randomly   
    plot(random_Numbers)
    abline(a=0,b=1)
  # testing for mean Hypothesis testing 
    test_cal <-t.test(random_Numbers, alternative = "two.sided",mu=60,conf.int=0.95)
    test_cal
  #df- Degrees of freedom 
    (49.91-60)/(sd(random_Numbers)/sqrt(length(random_Numbers)))
    