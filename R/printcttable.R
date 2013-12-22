#'
#' \code{print.cttable}  quick description
#'
#' @param cttable description

#'
#' @return NULL
#' @export
#'
print.cttable <- function(cttable)
{
	if (class(cttable) != "cttable")
	{
		stop("This is not a cttable")
	}
	nf <- attr(cttable, "factors")

	if (nf == 1)
	{
		print(as.table(cttable[ ,  , 1]))
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

