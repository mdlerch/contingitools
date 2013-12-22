#'
#' \code{cttable} Build a contingency table or set of contingency tables out
#' of an input vector or matrix
#'
#' @param input numeric numeric vector of length 4n where n is an integer

#'
#' @return numeric matrix or array of class \code{cttable} depending on size of n
#' @export
#'

cttable <- function(input)
{
	input <- c(input)
	if (length(input) %% 4 == 0)
	{
		out <- array(input, dim = c(2, 2, nf <- length(input) / 4),
		             dimnames = list(case=c("D", "!D"), exposure = c("E", "!E"),
		                             factors = paste("f", 1:nf, sep = "")))
		out <- aperm(out, c(2, 1, 3))
		class(out) <- "cttable"
		attr(out, "factors") <- nf
	} else {
		stop("incorrect size")
	}
	return(out)
}

print.cttable()
