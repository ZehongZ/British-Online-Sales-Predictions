
# T-tests on coefficients
arimaPrint<- function(m1, list=NULL){
# m1: is an arima() model object
# list: is the index list of non zero coefficients (i.e "na" values in fixed)
# 
#
print('T-test table for model coefficients')
if (length(list)==0) 

matrix(prettyNum(c(m1$coef,sqrt(diag(m1$var.coef)), 2 * pnorm(-abs(m1$coef / sqrt(diag(m1$var.coef)))))), 
		nrow=length(m1$coef), ncol=3, byrow=F,
		dimnames = list(names(m1$coef), c("estimates", "std error", "p-values")))

else {
coef = m1$coef[list]
matrix(prettyNum(c(coef,sqrt(diag(m1$var.coef)), 2 * pnorm(-abs(coef / sqrt(diag(m1$var.coef)))))), 
		nrow=length(coef), ncol=3, byrow=F,
		dimnames = list(names(coef), c("estimates", "std error", "p-values")))
}
}
