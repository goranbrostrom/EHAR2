fit.out <- function(fit, caption, label){
    ## Output of regression results
    if (knitr::is_latex_output()){ # PDF
        if (!missing(label)){
            label <- paste0("tab:", label)
        }
        dr <- drop1(fit, test = "Chisq")
        ltx(fit, dr = dr, caption = caption, label = label)
    }else{ # HTML
        xx <- regtable(summary(fit), digits = 4)
        nn <- ncol(xx)
        rr <- c("Max Log", "Likelihood", "", round(fit$loglik[2], 1), rep("", nn - 4))
        xx <- rbind(xx, rr)
        kbl(xx, booktabs = TRUE, caption = caption) %>%
            kable_styling(##font_size = 12, 
                full_width = FALSE)
    }
}