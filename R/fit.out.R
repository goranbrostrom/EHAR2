fit.out <- function(fit, caption = "", label){
    ## Output of regression results
    
    if (knitr::is_latex_output()){ # PDF
        if (!missing(label)){
            label <- paste0("tab:", label)
        }
        dr <- drop1(fit, test = "Chisq")
        if (inherits(fit, "tpchreg")) fit <- summary(fit) ## Hideous hack!!
        ltx(fit, dr = dr, caption = caption, label = label)
    }else{ # HTML
        sf <- summary(fit)
        xx <- regtable(sf, digits = 4, short = TRUE)
        nn <- ncol(xx)
        rr <- c("Max Log", "Likelihood", "", round(fit$loglik[2], 1), rep("", nn - 4))
        xx <- rbind(xx, rr)
        if (!is.null(rme <- sf$rmean)){
            rr <- rep("", nn)
            rr[1:2] <- c("Restricted", "mean")
            m <- length(rme)
            rr[3:(2 + m)] <- round(rme, 3)
            xx <- rbind(xx, rr)
        }
        kbl(xx, booktabs = TRUE, caption = caption) %>%
            kable_styling(##font_size = 12, 
                full_width = FALSE)
    }
}