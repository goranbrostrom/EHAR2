tbl <- function(tt, caption = ""){
    library(kableExtra)
    kbl(tt, caption = caption, booktabs = TRUE) %>%
        kable_styling(full_width = FALSE, 
                    position = "center")
}
