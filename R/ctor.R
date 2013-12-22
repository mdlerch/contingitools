#'
#' \code{ct.or}  quick description
#'
#' @param cttable description
#' @param level description
#' @param 0 description
#' @param 95 description

#'
#' @return returned
#' @export
#'
ct.or <- function(cttable, level = 0.95)
{
	p <- 1 - (1 - level) / 2
	if (attr(cttable, "factors") == 1)
	{
		DE   <- cttable[1, 1, 1]
		nDE  <- cttable[1, 2, 1]
		DnE  <- cttable[2, 1, 1]
		nDnE <- cttable[2, 2, 1]

		or <- DE / nDE / (DnE / nDnE)

		# interval on log scale
		se <- sqrt( 1 / DE + 1 / nDE + 1 / DnE + 1 / nDnE)
		int <- log(or) + c(-1, 1) * qnorm(p) * se
		return(list(estimate = or, interval = exp(int)))
	}

	# small sample size on page 80

}
