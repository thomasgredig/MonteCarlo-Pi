library(ggplot2)
library(raster)
# array size
N = 50
J = 1
beta = 5

# initialize array
spin = matrix(data=sign(runif(N*N)-0.5), nrow=N)
sum(spin)



plot(raster(spin))

for(i in 1:5000) {
  # choose random spin
  x=round(runif(1,min=1,max=N))
  y=round(runif(1,min=1,max=N))
  # compute energy change to flip:
  nb = spin[(x %% N)+1,y] + spin[((x-2) %% N)+1,y] + spin[x,(y %% N)+1] + spin[x,((y-2) %% N)+1]
  dE = 2*J*spin[x,y]*nb
  if (dE<0) { 
    spin[x,y] = -spin[x,y] 
  } else {
    # flip coin
    if (runif(1)< exp(-dE*beta)) {
      spin[x,y] = -spin[x,y]
    }
  }
}

plot(raster(spin))

