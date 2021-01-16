tbl <- function(tt, caption = ""){
    library(kableExtra)
    kbl(tt, caption = caption, booktabs = TRUE, row.names = FALSE) %>%
        kable_styling(full_width = FALSE, 
                    position = "center")
}
