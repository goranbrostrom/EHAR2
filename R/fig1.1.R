fig1.1 <- function(){
    ##plot(1,5, xlim = c(0, 8), ylim = c(0, 4), axes = FALSE)
    ##pdf(file = "fig1.1.pdf", width = 15, height = 6)
    oldpar <- par(xaxt="n", yaxt="n")
    symbols(c(3, 12), c(3, 3), circles = c(0.5, 0.5),
            xlim = c(0, 18), ylim = c(0, 6),
            xlab = "", ylab = "")
    text(3, 3, "Alive", cex = 2.5)
    text(12, 3, "Dead", cex = 2.5)
    arrows(5, 3, x1 = 10, cex = 2)
    par(oldpar)
    ##dev.off()
}
