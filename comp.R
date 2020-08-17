comp <- function(enter, exit, event, start.age = 0){
    ## Competing risks:
    ## event == 0: censoring
    ## event != 0: cause of "exit"
    ##
    ## First, assume that causes are numbered 1, 2, ..., n.
    ##
    require(eha)
    n <- max(event)

    ## First, S(), "total" survival:
    rs.tot <- risksets(Surv(enter, exit, event > 0.5))
    haz.tot <- rs.tot$n.events / rs.tot$size
    n.times <- length(haz.tot) + 1
    S <- numeric(n.times)
    S[1] <- 1  # I.e. S(0) = 1
    for (i in 2:n.times) S[i] <- S[i-1] * (1 - haz.tot[i-1])
    ##
    haz <- matrix(0, nrow = n, ncol = length(haz.tot))
    P <- matrix(0, nrow = n, ncol = length(haz.tot) + 1)
    ## Go thru all causes, cause by cause (row by row in 'haz':
    for (row in 1:n){
        rs <- risksets(Surv(enter, exit, event == row))
        haz.row <- rs$n.events / rs$size
        tmp <- 0
        cols <- which(rs.tot$risktimes %in% rs$risktimes)
        haz[row, cols] <- haz.row
        ## Given hazards, now get P[row, ]:
        P[row, 2:NCOL(P)] <- cumsum(S[1:(n.times - 1)] * haz[row, ])
    }
    plot(c(start.age, rs.tot$risktimes), S, ylim = c(0, 1),
         xlim = c(start.age, max(rs.tot$risktimes)), xlab = "Age",
         type = "s", ylab = "Probability")
    for (i in 1:n)
        lines(c(start.age, rs.tot$risktimes), P[i, ], lty = i+1, ##col = i+1,
              type = "s")
    abline(h = 0)
    legend(35, 0.95, lty = 1:(n+1), ##col = 1:(n+1),
           legend = c("Survival", "Dead", "Other parish",
           "Scandinavia", "Outside Scandinavia"))
    invisible(list(P = P, S = S))
}

