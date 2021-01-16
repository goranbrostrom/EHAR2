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
        kbl(xx, booktabs = TRUE, caption = caption) %>%
            kable_styling(##font_size = 12, 
                full_width = FALSE)
    }
}