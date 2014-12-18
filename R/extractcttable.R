extractcttable <- function(cttable, f = 1)
{
	out <- cttable(t(cttable[ , , f]))

	return(out)
}
