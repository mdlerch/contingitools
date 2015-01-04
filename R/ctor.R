ct.or <- function(cttable, level = 0.95, correction = FALSE)
{
    if (class(cttable) != "cttable")
    {
        cttable <- cttable(cttable)
    }

    if (attr(cttable, "Groups") == 1)
    {
        return(ct.or.one(cttable, level, correction))
    }
    else
    {
        out <- list()
        for (i in 1:attr(cttable, "Groups"))
        {
            g <- paste0("Group", i)
            out[[g]] <- ct.or.one(cttable[ , , i], level, correction)
        }

        return(out)
    }
}

ct.or.one <- function(cttable, level = 0.95, correction = FALSE)
{
    p <- 1 - (1 - level) / 2

    DE <- cttable[1, 1]
    dE <- cttable[1, 2]
    De <- cttable[2, 1]
    de <- cttable[2, 2]

    # section 7.1
    if (correction)
    {
        or <- (DE * de) / ((De + 1) * (dE + 1))
    } else {
        or <- (DE / dE) / (De / de)
    }

    # interval on log scale sec. 7.1.2 pg 77
    if (correction)
    {
        orc <- ((DE + .5) * (de + .5)) / ((De + .5) * (dE + .5))
        se <- sqrt( 1 / (DE + .5) + 1 / (dE + .5) + 1 / (De + .5) + 1 / (de + .5))
        int <- log(orc) + c(-1, 1) * qnorm(p) * se
    }
    else
    {
        se <- sqrt( 1 / DE + 1 / dE + 1 / De + 1 / de)
        int <- log(or) + c(-1, 1) * qnorm(p) * se
    }

    return(list(OR = or, CI = exp(int)))
}
