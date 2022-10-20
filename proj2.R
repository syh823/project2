##   Group members
# Yuheng Song, s2447118
# Yanming Gu, s2304572
# Yiqian Dai, s2329960


## Address of github repo:
# https://github.com/syh823/projrct2.git


## Each team member's contribution:
# Yuheng Song undertook about 55% of the work.
# He wrote the Pone and Pall functions, which corresponds to q1-q3. He also helped a lot with others' work. 
# What's more, he put up some ideas about the result and dug into the reasons behind it. 

# Yanming Gu undertook about 40% of the work.
# She wrote the dloop function, which corresponds to q4-6. She also wrote main comments, and discuss the result with Yuheng.

# Yiqian Dai undertook about 5% of the work.
# She contributes to some comments of our codes. 




## Here's function pone
Pone <- function(n, k, strategy, nreps) {
  # m is the total number of cards
  m=2*n
  
  # For strategy 1
  if(strategy==1) {
    # Denote sum1 as the total number of successful trails that someone finds his prisoner number using strategy 1
    sum1=0
    temp=array()
    # Do a (nreps) times simulation
    for(i in (1:nreps)){
      # Generate card numbers
      card<-sample(c(1:m),m,replace = FALSE)
      # Define the card number of the kth box when the prisoner(with number k) first opens the box
      temp[1]=card[k]
      # The prisoner has n chances to open boxes
      for(j in (1:n)){
        # If the current card number is equal to the prisoner number(k), then he makes it
        if(temp[j]==k)
        {
          # If the prisoner makes it within n times, then successful trials increase by 1
          sum1=sum1+1
          break
        }
        else
        {
          # Otherwise, the prisoner continues to open the box of the card number in the previous box
          temp[j+1]=card[temp[j]]
        }
      }
      
    }
    # The probability of strategy 1 is the ratio of successful trials to nrep trails
    prob1=sum1/nreps
    print(prob1)
  }
  
  
  # For strategy 2
  if(strategy==2) {
    # Denote sum2 as the total number of successful trails that find their prisoner number using strategy 2 
    sum2=0
    # Do a (nreps) times simulation
    for (i in (1:nreps)){
      temp2=array()
      # Generate card numbers
      card<-sample(c(1:m),m,replace = FALSE)
      # The prisoner randomly opens the first box
      start=sample(c(1:m),1,replace = FALSE)
      # Refer to card number in the first box 
      temp2[1]=card[start]
      # The prisoner has n chances to open boxes
      for(j in (1:n)){
        # If the current card number is equal to the prisoner number(k), then he makes it
        if(temp2[j]==k){
          # If the prisoner makes it within n times, then successful trials increase by 1
          sum2=sum2+1
          break
        }
        else{
          # Otherwise, the prisoner continues to open the box of the card number in the previous box
          temp2[j+1]=card[temp2[j]]
        }
      } 
    }
    #The probability of strategy2 is the ratio of successful trials to nrep trails
    prob2=sum2/nreps
    print(prob2)
  }
  
  # For strategy 3
  if(strategy==3) {
    # Denote sum3 as the total number of successful trails that find their prisoner number using strategy 3
    sum3=0 
    # Do a (nreps) times simulation
    for(i in (1:nreps)){
      # Generate only a half(n) card numbers. This is to simulate the prisoner opens n boxes from 2n boxes
      temp3=sample(c(1:m),n,replace = FALSE)
      # If one of these n numbers has k(the prisoner's number), then he makes it 
      if(length(which(temp3==k))!=0){
        # The successful trials increase by 1
        sum3=sum3+1
      }
    }
    #The probability of strategy3 is the ratio of successful trials to nrep trails
    prob3=sum3/nreps
    prob3
  }
  
}





## Here's function Pall
Pall <- function(n, strategy, nreps) {
  # m is the total number of cards  
  m=2*n

  # Let sum be the successful trails that all prisoners find their number. Now assume they make it every times.
  sum=nreps 
  
  # For strategy 1
  if(strategy==1){
    # Do a (nreps) times simulation
    for(i in (1:nreps)){
      # Generate card numbers
      card<-sample(c(1:m),m,replace = FALSE)
      # Go through each prisoner
      for(j in (1:m)){
        # account is used to calculate the number of times that boxes were opened
        account=1
        # The prisoner first opens the box of his prisoner number j
        card_number=j
        # The prisoner can keep opening boxes n times
        while(account<=n){
          # The prisoner continues to open the box of precious card number
          card_number=card[card_number]
          # The prisoner finds the card with their number on it 
          if(card_number==j){
            break
          }
          # If not, the prisoner opens one more box
          account=account+1
        }
        # If the prisoner opens n boxes without finding their number, the trail fails
        if(account==n+1){
          sum=sum-1
          break
        }
      }
    }
    
    #The probability of strategy 1 is the ratio of successful trials to nrep trails
    print(sum/nreps)
  }
  
  # For strategy 2
  if(strategy==2){
    # Do a (nreps) times simulation
    for(i in (1:nreps)){
      # Generate card numbers
      card<-sample(c(1:m),m,replace = FALSE)
      # Go through each prisoner
      for(j in (1:m)){
        # account is used to calculate the number of times that boxes were opened
        account=1
        # The prisoner randomly opens the first box
        card_number=sample(c(1:m),1,replace=FALSE)
        # The prisoner can keep opening boxes n times
        while(account<=n){
          # The prisoner continues to open the box of precious card number
          card_number=card[card_number]
          # The prisoner finds the card with their number on it 
          if(card_number==j){
            break
          }
          # If not, the prisoner opens one more box
          account=account+1
        }
        # If the prisoner opens n boxes without finding their number, the trail fails
        if(account==n+1){
          sum=sum-1
          break
        }
      }
    }
    #The probability of strategy2 is the ratio of successful trials to nrep trails
    print(sum/nreps)
  }
  
  # For strategy 3
  if(strategy==3){
    # Do a (nreps) times simulation
    for(i in (1:nreps)){
      # Generate card numbers
      card<-sample(c(1:m),m,replace = FALSE)
      # Go through each prisoner
      for(j in (1:m)){
        # card1 is the remaining cards that haven't been selected
        card1=card
        # account is used to calculate the number of times that boxes were opened
        account=1
        # The prisoner can keep opening boxes n times
        while(account<=n){
          # Every times, the prisoner select a box randomly.
          card_number=sample(card1,1,replace = FALSE)
          # After each trail, the card has been selected will be removed from the card pool
          card1=card1[-which(card1==card_number)]
          # The prisoner finds the card with their number on it
          if(card_number==j){
            break
          }
          # If not, the prisoner opens one more box
          account=account+1
        }
        # If the prisoner opens n boxes without finding their number, the trail fails
        if(account==n+1){
          sum=sum-1
          break
        }
      }
    }
    # The probability of strategy 3 is the ratio of successful trials to nrep trails
    print(sum/nreps)
  }
}



## Individual success probabilities under strategy 1,2,3 for n = 5
Pone(5, 8, 1, 10000)
Pone(5, 8, 2, 10000)
Pone(5, 8, 3, 10000)

## Individual success probabilities under strategy 1,2,3 for n = 50
Pone(50, 8, 1, 10000)
Pone(50, 8, 2, 10000)
Pone(50, 8, 3, 10000)

## Joint success probabilities under strategy 1,2,3 for n = 5
Pall(5, 1, 10000)
Pall(5, 2, 10000)
Pall(5, 3, 10000)

## Joint success probabilities under strategy 1,2,3 for n = 50
Pall(50, 1, 10000)
Pall(50, 2, 10000)
Pall(50, 3, 10000)

## About the test result:
# For individual success probabilities, under strategy 1 and 3 are both around 0.5, however, the probability under strategy 2 
# is obviously lower than 0.5. The reason for the decrease in strategy 2 probability is that there exists such situation: 
# a prisoner with number c falls in a loop of number a, b, d, f, e... then, after a few trials opening the boxes, he'll find 
# that he would never open a box with his number b in it. Strategy 1 avoids such bad loops cleverly by opening the first box 
# using his own number. He puts himself in a loop belongs to him, rather than falls into other people's loops.
# And for strategy 3, the probability is just n/2n = 0.5.
# Also, the boxes prisoners can open(n) don't have much effect on the results. 

# For joint success probabilities, it is clear that n influences the results. When n is relatively small, the strategy 1 is the 
# best solution, with about 1/3 success rate. Even though strategy is slightly different to strategy 1, the outcome changes a lot.
# It's even worse than strategy 3, which is the simplest and the most straightforward way to open n boxes. 
# As we see from the results when n = 50, the strategy 2 still stands out, with over 0.3 success rate, whereas both strategy 2 
# and 3 fails. If to calculate the exactly probability of strategy 3, it should be (1/2)^50, which is almost 0. 
# So, when n becomes bigger, the advantage of using strategy becomes much bigger. 




## dloop function overview: 
# We create a 2 dimension array (len). Its rows are 1 to 2*n successively. Its columns are 1 to nreps successively.
# The number of the ith row and jth column of array len represents that if a loop of length i occurs in the jth simulation.
# We don't care about exactly how many loops of length x occurs in one simulation. We just set it to 1 if it occurs at least once. 

dloop<-function(n,nreps){
  # m is the total number of cards
  m=2*n
  # Create an array len to store information about occurrences of loops
  len=array(0,dim=c(m,nreps))
  # Do a nreps times simulation
  for(i in (1:nreps)){
    # Generate card numbers
    card=sample(c(1:m),m,replace=FALSE)
    # Let j be the prisoner number. He must be in the middle of one loop, we need to find that loop's length.
    for(j in (1:m)){
      # Set the first card number, and the default length of loops to 1
      temp=card[j]
      length=1
      # If we find a card number is equal to the current prisoner number, then set the corresponding position of array len to 1
      while(1){
        if(temp==j){
          len[length,i]=1
          break
        }
        # Otherwise, update the card number and continue comparing it to the prisoner's number. The length of the loops increase by 1
        else{
          length=length+1
          temp=card[temp]
        }
      }
    }
  }
  ## print(len)
  
  
  ## The sum of the kth row of array len is the total number of occurrences of a loop of length k during nreps times simulation.
  prob <- c(1:m)
  for(k in (1:m)){
    prob[k] <- sum(len[k, ])/nreps
  }
  print(prob)
  
  ## The probability that there is no loop longer than 50 is equal to 
  # 1 minus the sum of probabilities that there would be a loop of length longer than 50
  print('The probability that there is no loop longer than 50 is :')
  print(1-sum(prob[(n+1):m]))
  
  ## draw a scatter plot, in which x is the length of loops, and y is the probability of that loop occurs at least once
  x<-c(1:m)
  y<-prob
  plot(x,y,xlab='loop length', ylab='probablities')
}
 ## Testing
dloop(50, 10000)

 

