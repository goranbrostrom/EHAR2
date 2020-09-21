plot2 <- function(){
    x <- rnorm(100)
    y <- x + rnorm(100, sd = 0.1)
    oldpar <- par(mfrow = c(1, 2))
    on.exit(par(oldpar))
    plot(x, y, main = "Scatterplot of x and y")
    hist(y, main = "Histogram of y", probability = TRUE)
}
