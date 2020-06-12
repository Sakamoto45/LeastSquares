library(ggplot2)
library(ggridges)
library(dplyr)


main <- function(n) {

  confirmed_global <- read.csv("time_series_covid19_confirmed_global.csv")
  
  Square <- function(a, b, c, x) a*x^2+b*x+c

  Exp <- function(a, b, c, x) c*exp(a*x+b)
  
  My <- function(a, b, c, x) exp(c-exp(-a*x+b))
  
  

    
  LQ <- function(a, b, c){
    

    h=0.05
    #eps = 10    
    
    ha <- h*0.2
    hb <- h*10
    hc <- h*10
    repeat  {
      prev <- f(confirmed, a, b, c)
      eps <- 10
      print(prev)
      repeat {
        aprev <- f(confirmed, a, b, c)
        repeat {
          a <- a+ha
          if (f(confirmed, a, b, c)>=f(confirmed, a-ha, b, c)) break
        }
        ha <- -ha/3
        acurr <- f(confirmed, a, b, c)
        if (abs(aprev-acurr)<eps) break
      }
      ha <- 10*ha
      
      repeat {
        bprev <- f(confirmed, a, b, c)
        repeat {
          b <- b+hb
          if (f(confirmed, a, b, c)>=f(confirmed, a, b-hb, c)) break
        }
        hb <- -hb/3
        bcurr <- f(confirmed, a, b, c)
        if (abs(bprev-bcurr)<eps) break
      }
      hb <- 10*hb
      
      repeat {
        cprev <- f(confirmed, a, b, c)
        repeat {
          c <- c+hc
          if (f(confirmed, a, b, c)>=f(confirmed, a, b, c-hc)) break
        }
        hc <- -hc/3
        ccurr <- f(confirmed, a, b, c)
        if (abs(cprev-ccurr)<eps) break
      }
      hc <- 10*hc
      
      curr <- f(confirmed, a, b, c)
      if (abs(prev-curr)<eps) break
    }
  
    return(c(a, b, c))
  }
 
  confirmed <- as.numeric(confirmed_global[n, -c (1:4)])
  confirmed <- confirmed[confirmed > 0]
  confirmed <- data.frame(conf = confirmed, time = 1:length(confirmed))
  
  graph <- cbind(confirmed, data.frame(type = rep("real", nrow(confirmed))))
  
  
  f <- function(Data, ...) sqrt(sum((Square(..., Data["time"])-Data["conf"])^2))
  parametrs <- LQ(0, 10, 500)
  graph <- rbind(graph, data.frame(conf = Square(parametrs[1], parametrs[2], parametrs[3], 1:nrow(confirmed)), time = 1:nrow(confirmed), type = rep("quadric", nrow(confirmed))))
  
  f <- function(Data, ...) sqrt(sum((My(..., Data["time"])-Data["conf"])^2))
  parametrs <- LQ(0, 0, 0)
  graph <- rbind(graph, data.frame(conf = My(parametrs[1], parametrs[2], parametrs[3], 1:nrow(confirmed)), time = 1:nrow(confirmed), type = rep("my", nrow(confirmed))))
  
  f <- function(Data, ...) sqrt(sum((Exp(..., Data["time"])-Data["conf"])^2))
  parametrs <- LQ(1, 1, 1)
  graph <- rbind(graph, data.frame(conf = Exp(parametrs[1], parametrs[2], parametrs[3], 1:nrow(confirmed)), time = 1:nrow(confirmed), type = rep("exp", nrow(confirmed))))
  
  
  ggplot(data = graph, aes(x = time, y = conf, group = type, color = type)) + 
    geom_line() +
    scale_color_manual(values = c("orange", "red", "green", "blue"),                    
      labels = c("c*exp(a*x+b)", "exp(c-exp(-ax+b))", "ax^2+bx+c", "confirmed"),
      name = "Graph") +
    labs(title = "COVID-19 confirmed cases time series", 
      x = "Time, day", 
      y = paste("number of cases in", paste(confirmed_global[n, 1], confirmed_global[n, 2]))) + 
    theme_bw()
}

main(188)#ввести номер строки с интересующей страной из файла "time_series_covid19_confirmed_global.csv"

