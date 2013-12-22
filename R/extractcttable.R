#'
#' \code{extractcttable}  quick description
#'
#' @param cttable description
#' @param f description

#'
#' @return returned
#' @export
#'
extractcttable <- function(cttable, f = 1)
{
	out <- cttable(t(cttable[ , , f]))

	return(out)
}
