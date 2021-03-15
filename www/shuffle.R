## Shuffle Function

## Take in data from with two columns: values ($v) and group ($num)
mix <- function(dat){
  
  if(is.numeric(dat[,1])){
    order <- sample(dat[,1])
    grp <- dat[,2]
    dat <- data.frame(group = grp, y = order)
    dat
  }
  
  else{
    order <- sample(dat[,2])
    grp <- dat[,1]
    dat <- data.frame(group = grp, y = order)
    dat
    
  }
}


# Make vector of all differences


diffMix <- function(dat) {
  diff.data <- vector()
  
  for (i in 1:500)
  {
    mixed <- mix(dat)
    as.data.frame(means <- mixed %>%
      group_by(group) %>%
      summarise(mean = mean(y), .groups = 'drop'))
    diff.temp<- as.numeric(means[1,2]- means[2,2])
    
    diff.data <-c(diff.data, diff.temp)
    
  }
  data.frame(differences = diff.data)
}
