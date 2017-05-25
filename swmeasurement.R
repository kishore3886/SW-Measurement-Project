#Measurement Project  

# seed genereates random number
# this random number becomes seed for next random number

#middle square method

#size<-4
seed<-1234
seed_square=0
start=0
end=0
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


    print(rand_generater())
    #print(seed_square)
    
