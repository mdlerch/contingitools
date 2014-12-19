# page 60 table 6.1 a b c d is DE dE De de
cttable <- function(input)
{
    if (class(input) == "cttable")
    {
        return(cttable)
    }
    input <- c(input)
    if (length(input) %% 4 == 0)
    {
        if (length(input) == 4)
        {
            out <- matrix(input, nrow = 2, byrow = TRUE,
                          dimnames = list(case = c("D", "!D"),
                                          exposure = c("E", "!E")))
            class(out) <- "cttable"
            attr(out, "factors") <- 1
        } else {
            out <- array(input, dim = c(2, 2, nf <- length(input) / 4),
                         dimnames = list(case=c("D", "!D"), exposure = c("E", "!E"),
                                         factors = paste("f", 1:nf, sep = "")))
            out <- aperm(out, c(2, 1, 3))
            class(out) <- "cttable"
            attr(out, "factors") <- nf
        }
    } else {
        stop("incorrect size")
    }
    return(out)
}

print.cttable <- function(cttable)
{
    if (class(cttable) != "cttable")
    {
        stop("This is not a cttable")
    }
    nf <- attr(cttable, "factors")

    if (nf == 1)
    {
        print(as.table(cttable[ ,  ]))
    } else {
        for (i in 1:nf)
        {
            msg <- paste("Factor ", i, "\n", sep = "")
            cat(msg)
            print(as.table(cttable[ , , i]))
            cat("\n")
        }
    }
    invisible(cttable)
}

