## Shuffle Function

## Take in data from with two columns: values ($v) and group ($num)
mix <- function(dat){
  order <- sample(dat$v)
  grp <- dat$num
  dat <- data.frame(v = order, num = grp)
  dat
}
