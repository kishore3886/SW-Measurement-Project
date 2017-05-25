#Measurement Project  

# seed genereates random number
# this random number becomes seed for next random number

#middle square method

size<-4
seed<-1234
seed_square=0
start=0
end=0
for (i in 1:1){
  
    rand_num  <-rand_generater(seed)
    print(seed_square)
    seed_square_char<- toString(rand_num) 
    start = size/2
    end = start+size-1
    seed_char<-substr(seed_square_char, start, end)
    seed<-as.numeric(seed_char)
    print(start)
    print(end)
    print(seed_square_char)
    print(rand_num)
    print(seed_char)
    print(seed)
}

rand_generater<- function(seed){
  
  seed_square=seed*seed
  
  return(seed_square)
}
