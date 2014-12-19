ct.rr <- function(cttable, level = 0.95)
{
    p <- 1 - (1 - level) / 2
    if (attr(cttable, "Groups") == 1)
    {
        DE <- cttable[1, 1]
        dE <- cttable[1, 2]
        De <- cttable[2, 1]
        de <- cttable[2, 2]

        # (4.1) pg 31
        rr <- (DE / (DE + dE)) / (De / (De + de))

        # interval on log scale sec 7.2 pg 82
        se <- sqrt((dE / (DE * (DE + dE))) + (de / (De * (De + de))))
        int <- log(rr) + c(-1, 1) * qnorm(p) * se

        return(list(RR = rr, CI = exp(int)))

        # see page 82 for small sample size

    }
}
