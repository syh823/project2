dloop = function(n, nreps){
  m = 2*n
  count_loops <- array()
  for(i in (1:nreps)){
    u <- sample(c(1:m), m, replace = FALSE)
    #index <- c(1:m)
    for(j in (1:m)){
      l = 1
      flag=TRUE
      while (u[j] != j){
        j <- u[j]
        l <- l + 1
        if (u[j] == j){
          flag=FALSE
          if(flag==TRUE) next
        }
      }
      count_loops[l, i] <- count_loops[l, i] + 1
      print(count_loops)
    }
  }
  
  #处理count_loops矩阵，把大于1的次数都改为1，方便后面计算
  for(i in (1:m)){
    for(j in (1:nreps)){
      if (count_loops[i, j] >= 1){
        count_loops[i, j] = 1
      }
    }
  }
  prob <- c(1:m)
  for(k in (1:m)){
    prob(k) <- sum(count_loops[k, ])/nreps
  }
  print(prob)
}
dloop(100,1000)
