#'
#' \code{ct.rr}  quick description
#'
#' @param cttable description
#' @param level description
#' @param 0 description
#' @param 95 description

#'
#' @return returned
#' @export
#'
ct.rr <- function(cttable, level = 0.95)
{
	p <- 1 - (1 - level) / 2
	if (attr(cttable, "factors") == 1)
	{
		DE   <- cttable[1, 1, 1]
		nDE  <- cttable[1, 2, 1]
		DnE  <- cttable[2, 1, 1]
		nDnE <- cttable[2, 2, 1]

		rr <- DE / (DE + nDE) / (DnE / (DnE + nDnE))

		# interval on log scale

		se <- sqrt((nDE / (DE * (DE + nDE))) + (nDnE / (DnE + nDnE)))
		int <- log(rr) + c(-1, 1) * qnorm(p) * se
		return(list(estimate = rr, interval = exp(int)))

		# see page 82 for small sample size

	}
}
