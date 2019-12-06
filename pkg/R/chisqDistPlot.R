chisqDistPlot <- function(chisqValue, df, alpha = 0.05, distWidth){

  CI <- 1-alpha

  plot(x = seq(0, distWidth,length.out = 1000),
     y = dchisq(seq(0, distWidth,length.out = 1000), df= df),
     main = "Chi-squared Distribution",
     ylab = "Probability Density",
     xlab = "",
     type = "l",
     lwd = 2)

  mtext(text = bquote(nu ~ paste("=") ~ .(df)), side = 3, line = 0)

  abline(v = qchisq(CI, df = df), lty=2)
  text(x = qchisq(CI, df = df), y = 0.4, label = paste0("Chi-sq(", CI,") = ", round(qchisq(CI, df = df),2)), pos = 4)
  points(chisqValue, dchisq(chisqValue, df = df), col = "red", pch = 19, cex = 1.5)
  text(chisqValue, dchisq(chisqValue, df = df), label = chisqValue, pos = 3, col = "red")

}
