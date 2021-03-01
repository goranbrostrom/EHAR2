tbl <- function(tt, caption = "", fs = 11){
    library(kableExtra)
    kbl(tt, caption = caption, booktabs = TRUE, row.names = FALSE) %>%
        kable_styling(full_width = FALSE, font_size = fs,
                    position = "center")
}
