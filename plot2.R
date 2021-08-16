plot2 <- function(){
    set.seed(1211) # fix random sequence.
    x <- 3 + rnorm(100)
    y <- 2 + x + rnorm(100, sd = 0.5)
    oldpar <- par(mfrow = c(1, 2))
    on.exit(par(oldpar))
    par(las = 1)
    plot(x, y, main = "Scatterplot of x and y")
    hist(y, main = "Histogram of y", probability = TRUE)
}
