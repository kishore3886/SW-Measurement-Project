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

    #print(rand_generater())
 random_Numbers<- rand_generater()
 random_Numbers[3]
 typeof(random_Numbers)
 print(length(random_Numbers))
    #print(seed_square)
    min_Number=2565
    max_Number=0
    random_Numbers_median=0
    typeof(random_Numbers_median)
    #finding minimum

    for(i in 1:100)
    {
      print(i)
      print(random_Numbers[i])
      if(random_Numbers[i]<min_Number)
       {
       min_Number= random_Numbers[i]
     }
   
    }
    
#finding maximum
    
    for(i in 1:100)
    {
      if(random_Numbers[i]>max_Number)
      {
        max_Number= random_Numbers[i]
      }
      
    }
#FINDING MEAN 
    for(i in 1:100)
    {
      sum_Random_Numbers<-sum_Random_Numbers+random_Numbers[i]
      
    }
    
    random_Numbers_Median <- sum_Random_Numbers/100
    min_Number
    max_Number
    random_Numbers_Median