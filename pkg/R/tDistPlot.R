tDistPlot <- function(tvalue, df, alpha = 0.05, ...) {

  CI <- 1 - alpha

  distWidth <- ceiling(tvalue)

  if(distWidth-tvalue < 0.6){
    distWidth <- ceiling(tvalue) + 1
  }

  plot(x = seq(-distWidth, distWidth,length.out = 1000),
       y = dt(seq(-distWidth, distWidth,length.out = 1000), df = df),
       main = "t Distribution",
       ylab = "Probability Density",
       xlab = "",
       type = "l",
       lwd = 2,
       ...)

  mtext(text = bquote(nu[1] ~ paste("=") ~ .(df)))

  abline(v = qt(alpha/2, df = df), lty = 2)
  abline(v = qt(CI + alpha/2, df = df), lty = 2)
  points(tvalue, dt(tvalue, df = df), pch = 19, cex = 1.5, col = "red")

  if(tvalue > abs(qt(alpha/2, df = df))){
    text(tvalue, dt(tvalue, df = df), label = tvalue, pos = 3, col = "red")
  }else{
  text(tvalue, dt(tvalue, df = df), label = tvalue, pos = 4, col = "red")
  }
  text(0, 0.18, label = paste0(CI*100, "%"))

}



