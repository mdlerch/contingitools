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
	if (attr(cttable, "factors") == 1)
	{
		DE   <- cttable[1, 1, 1]
		nDE  <- cttable[1, 2, 1]
		DnE  <- cttable[2, 1, 1]
		nDnE <- cttable[2, 2, 1]

		or <- A * B / (C * D)
	}

	return(or)
}
