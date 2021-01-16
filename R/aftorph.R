aftorph <- function(){
    ## Comparing AFT and PH fits of old age mortality.
    library(eha)
    ## 1. Two-sample Gompertz:
    
    fit1w <- phreg(Surv(enter - 60, exit - 60, event) ~ 1, 
                    data = oldmort[oldmort$sex == "female", ], dist = "gompertz") 
    fit1m <- phreg(Surv(enter - 60, exit - 60, event) ~ 1, 
                    data = oldmort[oldmort$sex == "male", ], dist = "gompertz") 
    fm <- as.formula("Surv(enter - 60, exit - 60, event) ~ sex + region")
    ## 2. PH
    
    fit.ph <- phreg(fm, data = oldmort, dist = "gompertz")
    
    ## 3. AFT
    
    fit.aft <- aftreg(fm, data = oldmort, dist = "gompertz")
    
    op <- par(mfrow = c(1, 2))
    w1 <- hazards(fit1w)
    m1 <- hazards(fit1m)
    
    ## The AFT plot:
    
    plot(m1$x, m1$y, type = "l", main = "AFT", log = "xy")
    lines(w1$x, w1$y, lty = 2)
    haft <- hazards(fit.aft)
    lines(haft$x, haft$y, col = "red")
    lines(haft$x / exp(fit.aft$coefficients[1]), haft$y, col = "red", lty = 2)
    
    ## The PH plot:
    plot(m1$x, m1$y, type = "l", main = "PH", log = "xy")
    lines(w1$x, w1$y, lty = 2)
    hph <- hazards(fit.ph)
    lines(hph$x, hph$y, col = "red")
    lines(hph$x, exp(fit.ph$coefficients[1]) * haft$y, col = "red", lty = 2)
    
    
    list(aft = fit.aft, ph = fit.ph)
    par(op)
}