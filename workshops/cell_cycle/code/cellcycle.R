params <- list (k_act=0.1 , k_inact=0.1 , C=100 )

posflux <- function(A, params) {
  return (params$k_act * (params$C-A) )
}

negflux <- function(A, params) {
  return (params$k_inact * A )
}


A = seq (0 , 100 , 1 )
plot(A,posflux(A,params), type="l", col="red", main="Flux balance", xlim=c(0, 100), ylim=c(0, 10))

lines(A,negflux(A,params), col="blue")

library (deSolve)


rhs <- function(t, A, params) {
  return (with (params, list (k_act * (C-A) - k_inact*A)))
}

A0 <- 0.0

t <- seq (0 , 100 , 1 )

out <- ode (A0, t, rhs, params)

plot(out, lwd=2, main="Simple Circuit", xlim=c(0, 100), ylim=c(0, 100))


A0=10
out <- ode (A0, t, rhs, params)
lines (out, lwd=2 )

A = seq (0 , 100 , 1 )
plot(A,negflux (A,params), type="l" , col="red" , main="Flux balance plot" , 		xlim=c (0 , 100 ), ylim=c (0 , 10 ))

params$C=0
lines(A,posflux(A,params), col="blue")

params$C=10
lines(A,posflux(A,params), col="blue")

A = seq (0 , 100 , 1 )
plot (A,negflux (A,params), type="l" , col="red" , main="Flux balance plot" , 			xlim=c (0 , 100 ), ylim=c (0 , 10 ))

for (C in seq (0 ,200 ,10 )){
  params$C=C
  lines (A,posflux (A,params), col="blue" )
}

plot(x=NULL,y=NULL,xlim=c(0,100),ylim=c(0,100),xlab="Cyclin level",ylab="Steady State Cdk1")
params$C=0
A0 = 0
out <- ode (A0, t, rhs, params)
points(params$C,out[101,2])

params$C=10
A0 = out[101,2]
out <- ode (A0, t, rhs, params)
points(params$C,out[101,2])


hill <- function(X,K,n) {
  return(X^n/(K^n + X^n))
}

x = seq(0,4,0.1)
plot(x,hill(x,1,4),xlim=c(0,4),ylim=c(0,1),xlab="X",ylab="Hill(X)")


plot(x=NULL,y=NULL,xlim=c(0,4),ylim=c(0,1),xlab="X",ylab="Hill(X)")
for (n in seq(1,10,1)) {
  lines(x,hill(x,2,n))
}


feedback_params <- list(a_act = 0.16, f_act = 0.8, K_act = 34, n_act = 11, a_inact = 0.08, f_inact = 0.4, K_inact = 33, n_inact = 3.5, C = 60)


library(phaseR)


RHS_2D <- function(t, y, parameters) {
  with(as.list(parameters), {
    C = y[1]
    A = y[2]
    dc=k_syn-(a_deg + f_deg*A^n_deg/(K_deg^n_deg + A^n_deg))*C
    pda = (a_act+f_act*A^n_act/(K_act^n_act+A^n_act))*(C-A)
    nda = (a_inact+f_inact*A^n_inact/(K_inact^n_inact+A^n_inact))*A
    return(list(c(dc,pda-nda)))
  })
}



full_pars = list(k_syn=1.0,a_deg=0.01,f_deg=0.04,K_deg=32,n_deg=17,a_act=0.16,f_act=0.8,K_act=35,n_act=11,a_inact=0.08,f_inact=0.4,K_inact=30,n_inact=3.5)



flowField(RHS_2D, x.lim = c(0, 100), 
          y.lim = c(0, 100),
          xlab="C", ylab="A",
          parameters = full_pars, 
          points = 15, add = FALSE)



nullclines(RHS_2D, x.lim = c(0, 100), 
           y.lim = c(0, 100),
           parameters = full_pars, 
           points = 500)




trajectory(RHS_2D, y0 = c(10,10), t.end = 500, 
           parameters = full_pars)
