library(ggplot2)

fnormal <- function(mu,sig,x1,x2) {
  x <- seq(from = mu - 6 * sig, to = mu + 6 * sig,length.out = 1000)
  
  df <- data.frame(x = x,
                   y = dnorm(x,mu,sig))
  
  df2 <- data.frame(x = c(x1,seq(x1,x2,length.out = 1000),x2),
                    y = c(0,dnorm(seq(x1,x2,length.out = 1000),mu,sig),0))
  
  arrow1 <- rbind(df2[2,],c(df2[2,1],df2[2,2]+0.1))
  
  arrow2 <- rbind(df2[nrow(df2)-1,], c(df2[nrow(df2)-1,1],
                                       df2[nrow(df2)-1,2]+0.1))
  
  z1 <- (x1 - mu) / sig
  
  z2 <- (x2 - mu) / sig
  
  qplot(df$x,df$y, geom = "line", xlab = "x", ylab = "f(x)") + 
    geom_polygon(data = df2, aes(x,y), fill = "blue") + 
    geom_line(data = arrow1, aes(x = x, y = y),
              arrow = arrow(length = unit(0.3,"cm"),
                            ends = "first",type = "closed")) + 
    annotate("text",x = arrow1$x[2], y = arrow1$y[2]+0.01, label = z1) +
    geom_line(data = arrow2,aes(x,y),
              arrow = arrow(length = unit(0.3,"cm"),
                            ends = "first",type = "closed",angle = 45)) + 
    annotate("text",x = arrow2$x[2], y = arrow2$y[2]+0.01, label = z2)
}

mu <- 0
sig <- 1
