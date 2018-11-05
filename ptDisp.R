# clean workspace
rm(list = ls(all=TRUE))

library(raster)
library(RColorBrewer)

# input data
Q <- 10000  # emissions  (g/s)
U <- 5      # wind speed (M/s)

# to compute sigma Y,Z ==> cte to fit later (1 val per met class of stability)
## ex for class 3
# sigmaY
a <- -2.054 
b <- 1.0231
c <- -0.0096
# sigmaZ
d <- -2.341
e <- 0.9477
f <- -0.002

# grid def
x  <- seq.int(10, 1000, by=10)
y  <- seq.int(-300, 300, by=10)
xy <- expand.grid(x=x, y=y, KEEP.OUT.ATTRS=FALSE)

# src pt disp
calc.sigma <- function(x, c1, c2, c3) { return( exp(c1 + c2*log(x) + c3*(log(x)**2)) ) }
calc.f     <- function(y, sigmaY) { return( exp(-((y**2) / (2*(sigmaY**2)))) ) }
calc.conc  <- function(f, Q, U, sigmaY, sigmaZ) { return( Q/U * f/(sigmaY * sigmaZ * pi) ) }

# orrrrrr
calc.disp <- function(x, y, Q, U, a, b, c, d, e, f) {
  return( Q/U * exp(-((y**2) / (2*(exp(a + b*log(x) + c*(log(x)**2)))**2))) / (exp(a + b*log(x) + c*(log(x)**2)) * exp(d + e*log(x) + f*(log(x)**2)) * pi) ) 
}


# compute conc
sigY <- calc.sigma(x=xy$x, c1=a, c2=b, c3=c)
sigZ <- calc.sigma(x=xy$x, c1=d, c2=e, c3=f)
ffff <- calc.f(y=xy$y, sigmaY=sigY)
conc <- calc.conc(f=ffff, sigmaY=sigY, sigmaZ=sigZ, Q=Q, U=U)
# agg 2 compare 2 excel file of Fred
res <- cbind(xy, sigY, ffff, sigZ, conc) 
# plot
# plot(rasterFromXYZ(xyz=res[,c("x","y","conc")]), breaks=c(1e-4, 1e-3, 1e-2, 1e-1, 1, 1e1, 5e2), col=rainbow(6) )
#colr <- brewer.pal(n=9,"BuGn")
brks <- exp(seq(0, log(max(conc)), length.out=50))
colr <- colorRampPalette(brewer.pal(12, "Set3"))(length(brks) -1)
#plot(rasterFromXYZ(xyz=res[,c("x","y","conc")]), breaks=brks, col=colr)
