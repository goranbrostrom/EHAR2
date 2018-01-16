plgeo <- function(h = 0.05, maxval = 100){
    x <- 0:(maxval - 1)
    y <- h * (1 - h)^x
    oldpar <- par(mfrow = c(2, 2))
    on.exit(par(oldpar))
    H <- (1:maxval) * h
    ## 1. hazard function:
    plot(c(1, 1), c(0, h), type = "l", xlim = c(0.5, maxval + 1),
         main = "Hazard function", lwd = 2, ylim = c(0, h * 1.2),
         ylab = "", xlab = "Duration", axes = FALSE)
         axis(1, at = c(1, 3, 5, 7, 9, 11))
         axis(2, at = c(0, 0.25), las = 1)
         box()
    for (i in 2:(maxval + 1)){
        lines(c(i, i), c(0, h), lwd = 2)
    }
    abline(h = 0)
    ## 2. Cumulative hazard function:
    plot(c(0, 1), c(0, 0), type = "l", ylim = c(0, H[maxval]),
         xlim = c(0, maxval + 1), main = "Cumulative Hazard function",
         ylab = "", xlab = "Duration")
    for (i in 1:maxval){
        lines(c(i, i+1), c(H[i], H[i]))
    }
    ## 3. Density function:
    f <- h * (1 - h)^(1:maxval)
    plot(c(1, 1), c(0, h), type = "l", xlim = c(0, maxval + 1),
         main = "Probability mass function", lwd = 2, ylim = c(0, h * 1.2),
         ylab = "", xlab = "Duration", axes = FALSE)
         axis(1, at = c(1, 3, 5, 7, 9, 11))
         axis(2, at = c(0, 0.10, 0.25))
         box()
    for (i in 2:(maxval + 1)){
        lines(c(i, i), c(0, f[i-1]), lwd = 2)
    }
    abline(h = 0)
    
    ## 4. Survivor function:
    s <- (1 - h)^(1:maxval)
    plot(c(0, 1), c(1, 1), type = "l", xlim = c(0, maxval + 1),
         main = "Survivor function", ylim = c(0, 1),
         ylab = "", xlab = "Duration")
    for (i in 1:maxval){
        lines(c(i, i + 1), c(s[i], s[i]))
    }
    abline(h = 0)
}
