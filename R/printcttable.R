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

}
