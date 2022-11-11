pi_anator <- function(){
  #set seed to randomize
  set.seed(1)
  
  #creates an array of 10000 x-variables from -1 to 1
  x<-runif(100000, min=-1, max=1)
  #creates an array of 10000 y-variables from -1 to 1
  y<-runif(100000, min=-1, max = 1)
  #find the distance from (0,0) formed by combining the x 
  #and y
  r <- sqrt(x^2 + y^2)
  #produces and print the result of pi
  print(4*length(which(r<=1))/length(r))
}

[]