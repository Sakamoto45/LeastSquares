confirmed_global <- read.csv("time_series_covid19_confirmed_global.csv")

f<-function(x) {
  if (abs(x)<0.5) return(0)
  if (x>0.5) return(1)
  if (x<-0.5) return(-1)
  return(0)
}

main <- function(list, delta = 5, step = 10) {
  
  for (country in list) {
    i = 0
    res <- data.frame()
    while (i*step+5+2*delta <= ncol(confirmed_global)) {
      res <- rbind(res, data.frame(value = c(confirmed_global[country, i*step+5]+confirmed_global[country, i*step+5+2*delta]-2*confirmed_global[country, i*step+5+delta]), time = c(i), country = country))
      i <- i+1
    }
    res$value <- res$value / max(abs(res$value))
    model <- rbind(model, res)
  }
  
  return(model)
}

model <- data.frame()

list <- c(51, 54, 188, 220, 226)#список интересующих стран

model <- main(list, 7, 7)


for (i in 1:nrow(model)) {
  model$value[i] <- f(model$value[i])
}

statistic <- data.frame()

for (country in list) {
  
  res<-data.frame(name = paste(confirmed_global[country, 1], confirmed_global[country, 2]),
                  p = nrow(model[unlist(model$country)==country & unlist(model$value)==1,]), 
                  z = nrow(model[unlist(model$country)==country & unlist(model$value)==0,]),
                  n = nrow(model[unlist(model$country)==country & unlist(model$value)==-1,]),
                  ptoz = 0, pton = 0, ztop = 0, zton = 0, ntop = 0, ntoz = 0)
  
  temp <- model[unlist(model$country)==country,]$value

  for (i in 1:(length(temp)-1)) {
    if (temp[i] == 1 & temp[i+1] == 0) res$ptoz <- res$ptoz+1
    if (temp[i] == 1 & temp[i+1] == -1) res$pton <- res$pton+1
    if (temp[i] == 0 & temp[i+1] == 1) res$ztop <- res$ztop+1
    if (temp[i] == 0 & temp[i+1] == -1) res$zton <- res$zton+1
    if (temp[i] == -1 & temp[i+1] == 1) res$ntop <- res$ntop+1
    if (temp[i] == -1 & temp[i+1] == 0) res$ntoz <- res$ntoz+1
  }
  print(res)
  statistic <- rbind(statistic, res)
}

statistic

write.csv(statistic,"statistic.csv", row.names = TRUE)
