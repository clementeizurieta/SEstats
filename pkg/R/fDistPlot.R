fDistPlot <- function(fvalue, df1, df2, alpha = 0.05, distWidth = 5){

  CI <- 1-alpha

  plot(x = seq(0, distWidth,length.out = 1000),
       y = df(seq(0, distWidth,length.out = 1000), df1 = df1, df2 = df2),
       main = "F Distribution",
       ylab = "Probability Density",
       xlab = "",
       type = "l",
       lwd = 2)

  mtext(text = bquote(nu[1] ~ paste("=") ~ .(df1) ~ paste(", ") ~ nu[2] ~ paste("=") ~ .(df2)), side = 3, line = 0)

  abline (v = qf(CI, df1 = df1, df2 = df2), lty = 2)
  text(x = qf(CI, df1 = df1, df2 = df2), y = 0.35, label = paste("F(0.95) =", round(qf(CI, df1 = df1, df2 = df2), 2)), pos = 4)

  points(fvalue, df(fvalue, df1 = df1, df2 = df2), pch = 19, col = "red", cex = 1.5)
  text(fvalue, df(fvalue, df1 = df1, df2 = df2), label = paste(fvalue), col = "red", pos = 3)
}
