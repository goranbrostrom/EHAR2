ger <- function(){
    require(eha)
    load("olm.rda")
    out <- check.surv(olm$enter, olm$exit, olm$event, olm$id)
    ko <- olm[olm$id %in% out, ]
    ## All records belonging to "bad individuals"
    ko <- ko[order(ko$id, ko$enter, -ko$event), ]
    for (i in ko$id){
        slup <- ko[ko$id == i, , drop = FALSE]
        n <- NROW(slup)
        if (n > 1){
            for (j in (2:n)){
                slup$enter[j] <- slup$exit[j-1]
            }
            ko[ko$id == i, ] <- slup
        }
    }
    om <- olm
    om[om$id %in% out, ] <- ko
    om <- om[om$enter < om$exit, ]
    om
}
