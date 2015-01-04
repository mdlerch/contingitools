ct.er <- function(cttable, level = 0.95)
{
    if (class(cttable) != "cttable")
    {
        cttable <- cttable(cttable)
    }

    if (attr(cttable, "Groups") == 1)
    {
        return(ct.er.one(cttable, level))
    }
    else
    {
        out <- list()
        for (i in 1:attr(cttable, "Groups"))
        {
            g <- paste0("Group", i)
            out[[g]] <- ct.er.one(cttable[ , , i], level)
        }

        return(out)
    }
}

ct.er.one <- function(cttable, level = 0.95, correction = FALSE)
{
    p <- 1 - (1 - level) / 2

    DE <- cttable[1, 1]
    dE <- cttable[1, 2]
    De <- cttable[2, 1]
    de <- cttable[2, 2]

    # Section 7.3 page 83
    er <- DE / (DE + dE) - De / (De + de)

    # interval on original scale (difference of proportions)
    se <- sqrt(DE * dE / (DE + dE) ^ 3 + De * de / (De + de) ^ 3)
    int <- er + c(-1, 1) * qnorm(p) * se

    return(list(ER = er, CI = int))
}
