ct.rr <- function(cttable, level = 0.95)
{
    if (class(cttable) != "cttable")
    {
        cttable <- cttable(cttable)
    }

    if (attr(cttable, "Groups") == 1)
    {
        return(ct.rr.one(cttable, level))
    }
    else
    {
        out <- list()
        for (i in 1:attr(cttable, "Groups"))
        {
            g <- paste0("Group", i)
            out[[g]] <- ct.rr.one(cttable[ , , i], level)
        }

        return(out)
    }
}

ct.rr.one <- function(cttable, level)
{

    p <- 1 - (1 - level) / 2

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
