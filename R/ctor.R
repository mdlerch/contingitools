ct.or <- function(cttable, level = 0.95)
{
    if (class(cttable) != "cttable")
    {
        cttable <- cttable(cttable)
    }

    p <- 1 - (1 - level) / 2

    if (attr(cttable, "factors") == 1)
    {
        DE <- cttable[1, 1, 1]
        dE <- cttable[1, 2, 1]
        De <- cttable[2, 1, 1]
        de <- cttable[2, 2, 1]

        # (4.2) pg 32
        or <- (DE / dE) / (De / de)

        # interval on log scale sec. 7.1.2 pg 77
        se <- sqrt( 1 / DE + 1 / dE + 1 / De + 1 / de)
        int <- log(or) + c(-1, 1) * qnorm(p) * se
        return(list(OR = or, CI = exp(int)))
    }

    # small sample size on page 80

}
