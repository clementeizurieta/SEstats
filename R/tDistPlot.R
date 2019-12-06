tDistPlot <- function(df, alpha = 0.05, tvalue, distWidth = 3) {

  CI <- 1 - alpha

  plot(x = seq(-distWidth, distWidth,length.out = 1000),
       y = dt(seq(-distWidth, distWidth,length.out = 1000), df = df),
       main = "t Distribution",
       ylab = "Probability Density",
       xlab = "",
       type = "l",
       lwd = 2)
  abline(v = qt(alpha/2, df = df), lty = 2)
  abline(v = qt(CI + alpha/2, df = df), lty = 2)
  points(tvalue, dt(tvalue, df = df), pch = 19, cex = 1.5, col = "red")
  text(tvalue, dt(tvalue, df = df), label = tvalue, pos = 4, col = "red")
  text(0, 0.18, label = paste0(CI, "%"))

}



