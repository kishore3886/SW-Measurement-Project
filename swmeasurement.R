#Measurement Project  

# seed genereates random number
# this random number becomes seed for next random number

#middle square method

#size<-4
seed<-1234
seed_square=0
start=0
end=0
sum_Random_Numbers=0
#rand_num_df<- data.frame(number=numeric())
rand_num_df<-numeric(100)

rand_generater<- function(){
  
  print(seed)
  for(i in 1:100)
  {
    seed_square=seed*seed
    seed_square_string<- toString(seed_square) 
    size=nchar(seed_square_string)
    if(size/2!=0)
    {
      seed_square_string=paste("0",seed_square_string) 
      size=size+1
    }
    else
    {}
    start = floor(size/2)
    end = start+digits-1
    seed_char<-substr(seed_square_string, start, end)
    print(seed_char)
    seed<-as.numeric(seed_char)
    rand_num_df [i]<-seed    
  }
   return(rand_num_df)
  
}

cal_Minimum<-function(){
  min_Number_Local= random_Numbers[3]
  for(i in 1:100)
  {
    print(i)
    print(random_Numbers[i])
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
cal_Median<- function(){
  
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
  
}

#starts here----------------------------------------------------------------------

random_Numbers<- rand_generater()
min_Number<-cal_Minimum()
max_Number<-cal_Maximum()
random_Numbers_median<-cal_Median()
random_Numbers_Mean<-cal_Mean()
random_Numbers_StandardDeviation<-cal_Standard_Deviation()
    typeof(random_Numbers_median)

    
    
    
#Finding median

    
    to_sort
    if((s %% 2) == 0){
      a<-s/2;
      b<-a+1
      random_Numbers_median=(to_sort[a]+to_sort[b])/2
    }
    if(!(s %% 2) == 0){
      a<-ceiling(s/2) ;
      random_Numbers_median=to_sort[a]
    }

    
    
    
#FINDING MEAN 
    for(i in 1:100)
    {
      sum_Random_Numbers<-sum_Random_Numbers+random_Numbers[i]
      
    }
    
    random_Numbers_Mean <- sum_Random_Numbers/100
 

    
#standard deviation
    standardDeviation_random_numbers=0
    variance_random_numbers=0
      for(i in random_Numbers)
        variance_random_numbers =variance_random_numbers+ ((i - random_Numbers_Mean)^ 2);
      
      standardDeviation_random_numbers= sqrt(variance_random_numbers/100)
      syscal<-sd(random_Numbers) 
      syscal
    min_Number
    max_Number
    random_Numbers_Mean
    random_Numbers_median
    standardDeviation_random_numbers
    