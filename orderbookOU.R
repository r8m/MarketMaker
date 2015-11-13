library(sde)
ou.lik <- function(x,dt) {
    function(theta1,theta2,theta3) {
        n <- length(x)
        dt <- dt
        -sum(dcOU(x=x[2:n], Dt=dt, x0=x[1:(n-1)],
                  theta=c(theta1,theta2,theta3), log=TRUE))
    }
}

ou.fit <- function(dataF, dt){
    fitF<-mle(ou.lik(dataF, dt),
        start=list(theta1=0,theta2=0.5,theta3=1),
        method="L-BFGS-B",lower=c(0,1e-5,1e-3), upper=c(3,3,3))
    coef(fitF)
}