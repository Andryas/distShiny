library(ggplot2)

normal <- function(mu,sig,x1,x2) {
  x <- seq(from = mu - 6 * sig, to = mu + 6 * sig,length.out = 1000)
  
  df <- data.frame(x = x,
                   y = dnorm(x,mu,sig))
  
  df2 <- data.frame(x = c(x1,seq(x1,x2,length.out = 1000),x2),
                    y = c(0,dnorm(seq(x1,x2,length.out = 1000),mu,sig),0))
  
  sumy <- df$y[which.max(df$y)] / 4
  
  arrow1 <- rbind(df2[2,],c(df2[2,1],df2[2,2] + sumy))
  
  arrow2 <- rbind(df2[nrow(df2)-1,], c(df2[nrow(df2)-1,1],
                                       df2[nrow(df2)-1,2] + sumy))
  
  z1 <- (x1 - mu) / sig
  
  z2 <- (x2 - mu) / sig
  
  g <- qplot(df$x,df$y, geom = "line", xlab = "x", ylab = "f(x)") + 
    geom_polygon(data = df2, aes(x,y), fill = "blue") + 
    geom_line(data = arrow1, aes(x = x, y = y),
              arrow = arrow(length = unit(0.3,"cm"),
                            ends = "first",type = "closed")) + 
    annotate("text",x = arrow1$x[2], y = arrow1$y[2] + sumy * 0.1, label = z1) +
    geom_line(data = arrow2,aes(x,y),
              arrow = arrow(length = unit(0.3,"cm"),
                            ends = "first",type = "closed",angle = 45)) + 
    annotate("text",x = arrow2$x[2], y = arrow2$y[2] + sumy * 0.1, label = z2)
  
  list(g,z1,z2,r = pnorm(z2) - pnorm(z1))
}

exponencial <- function(lambda,x1,x2){
 to <- if(x2 <= (10/lambda)){10 / lambda} else{x2}
 
 x <-  seq(from = 0, to = to, length.out = 1000)
 
 df <- data.frame(x = x,
                  y = dexp(x,lambda))
 
 df2 <- data.frame(x = c(x1,seq(from = x1, to = x2,length.out = 1000),x2),
                   y = c(0,dexp(seq(x1,x2,length.out = 1000),rate = lambda),0))
 
 sumy <- mean(df$y) * 1.2
 
 arrow1 <- rbind(df2[2,],c(df2[2,1],df2[2,2] + sumy))
 
 arrow2 <- rbind(df2[nrow(df2)-1,], c(df2[nrow(df2)-1,1],
                                      df2[nrow(df2)-1,2] + sumy))
 
 
 g <- qplot(x = x,y = y,data = df,geom = "line") + 
   geom_polygon(data = df2, aes(x,y), fill = "blue") + 
   labs(x = "x", y = "f(x)") + 
   geom_line(data = arrow1,aes(x = x,y = y),arrow = arrow(length = unit(0.3,"cm"),
                                           ends = "first",type = "closed")) + 
   geom_line(data = arrow2,aes(x = x,y = y),arrow = arrow(length = unit(0.3,"cm"),
                                           ends = "first",type = "closed")) + 
   annotate("text",x = arrow1$x[2], y = arrow1$y[2] + sumy * 0.2, label = x1) +
   annotate("text",x = arrow2$x[2], y = arrow2$y[2] + sumy * 0.2, label = x2)
   
 list(g,r = pexp(x2,lambda) - pexp(x1,lambda))
}

uniforme_cont <- function(a,b,x1,x2){
  x <- seq(from = a, to = b, length.out = 1000)
  
  df <- data.frame(x = x,
                   y = dunif(x,min = a,max = b))
  
  
  df2 <- data.frame(x = c(x1,seq(from = x1, to = x2, length.out = 1000),x2),
                    y = c(0,dunif(seq(x1,x2,length.out = 1000),a,b),0))
  
  sumy <- (1/(b-a)) * 0.25
  
  arrow1 <- rbind(df2[2,],c(df2[2,1],df2[2,2] + sumy))
  
  arrow2 <- rbind(df2[nrow(df2)-1,], c(df2[nrow(df2)-1,1],
                                       df2[nrow(df2)-1,2] + sumy))
  
  
  g <- qplot(x = x, y = y, data = df, geom = "line",xlab = "x", ylab = "f(x)") + 
    geom_segment(aes(x = a, xend = a, y = 0, yend = 1 / (b - a)),
                 linetype = "dashed") + 
    geom_segment(aes(x = b, xend = b, y = 0, yend = 1 / (b - a)),
                 linetype = "dashed") + 
    geom_polygon(data = df2, aes(x,y), fill = "blue") + 
    geom_line(data = arrow1,aes(x = x,y = y),
        arrow = arrow(length = unit(0.3,"cm"),ends = "first",type = "closed")) + 
    geom_line(data = arrow2,aes(x = x,y = y),
        arrow = arrow(length = unit(0.3,"cm"),ends = "first",type = "closed")) + 
    annotate("text",x = arrow1$x[2], y = arrow1$y[2] + sumy * 0.2, label = x1) +
    annotate("text",x = arrow2$x[2], y = arrow2$y[2] + sumy * 0.2, label = x2)
  
  list(g,r = punif(x2,a,b) - punif(x1,a,b))
}

