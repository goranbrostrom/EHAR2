tbl <- function(tt, caption = "", fs = 11, rownames = FALSE){
    library(kableExtra)
    kbl(tt, caption = caption, booktabs = TRUE, row.names = rownames) %>%
        kable_styling(full_width = FALSE, font_size = fs,
                    position = "center")
}
