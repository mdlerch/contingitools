ct.rr <- function(cttable, level = 0.95)
{
    p <- 1 - (1 - level) / 2
    if (attr(cttable, "factors") == 1)
    {
        DE <- cttable[1, 1, 1]
        dE <- cttable[1, 2, 1]
        De <- cttable[2, 1, 1]
        de <- cttable[2, 2, 1]

        rr <- (DE / (DE + dE)) / (De / (De + de))

        # interval on log scale

        se <- sqrt((dE / (DE * (DE + dE))) + (de / (De + de)))
        int <- log(rr) + c(-1, 1) * qnorm(p) * se
        return(list(RR = rr, interval = exp(int)))

        # see page 82 for small sample size

    }
}
