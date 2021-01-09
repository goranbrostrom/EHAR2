fit.out <- function(fit, caption, label){
    ## Output of regression results
    if (knitr::is_latex_output()){ # PDF
        dr <- drop1(fit, test = "Chisq")
        ltx(fit, dr = dr, caption = caption, label = "tab:coxold8")
    }else{ # HTML
        xx <- summary(fit)$coefficients[, 1:4]
        kbl(round(xx, 4), booktabs = TRUE, caption = caption) %>%
            kable_styling(font_size = 12, full_width = FALSE)
    }
}