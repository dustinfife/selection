#' Corrects correlations using Case III
#' 
#' Using Thorndike's Case III correction, \code{caseIII} corrects the xy
#' correlation for direct restriction on z (and by implication, indirect
#' restriction on x)
#' 
#' The Case III correction is defined as follows insert later The result is an
#' unbiased estimate of the unattenuated correlation between X and Y
#' 
#' @param rxy the restricted correlation between x (the indirectly selected
#' variable) and y (the outcome variable).
#' @param rzy the restricted correlation between z (the selection variable) and
#' y (the outcome variable).
#' @param rxz the restricted correlation between x (the indirectly selected
#' variable) and z (the selection variable).
#' @param uz the ratio of restricted to unrestricted variance (i.e.,
#' sigmaz'/sigmaz).
#' @return a scalar that is the unbiased estimate of the correlation between X
#' and Y.
#' @author Dustin Fife
#' @export
#' @seealso \code{\link{caseIV}}, \code{\link{caseIIIR}}, \code{\link{em}},
#' \code{\link{rel.correction}}
#' @references Thorndike, R. L. (1949). Personnel selection: Test and
#' measurement techniques. Oxford, England: Wiley.
#' 
#' Pearson, K. (1903). Mathematical contributions to the theory of evolution.
#' XI. On the influence of natural selection on the variability and correlation
#' of organs. Philosophical Transactions of the Royal Society of London. Series
#' A, Containing Papers of a Mathematical or Physical Character, 200, 1-66.
#' @examples
#' 
#' 	# load example data
#' data(selection.example.data)
#' 	# give me only those rows that have full information
#' new.dat = selection.example.data[!is.na(selection.example.data$Performance),]
#' cor.mat = cor(new.dat[,c("R", "Biodata", "Performance")])
#' 	# correct assuming direct selection on R, indirect on biodata, and a dv of performance
#' corrected = caseIII(rxy=cor.mat[1,3], rzy=cor.mat[2,3], rxz=cor.mat[1,2], uz = sd(new.dat$R)/sd(selection.example.data$R))	
#' corrected
#' 
caseIII <-
function(rxy, rzy, rxz, uz){
	uz = 1/uz
	num = rxy-rxz*rzy + rxz*rzy*uz^2
	denom = sqrt((1-rxz^2 + rxz^2*uz^2)*(1-rzy^2 + rzy^2*uz^2))
	return(num/denom)
}
