#########################################
# Compute the temperature dependence of the
# Magnetization for the Ising 2D model and
# test the speed in R
#
# see https://arxiv.org/pdf/0803.0217.pdf
#
# (c) 2019 Thomas Gredig
#########################################

library(ggplot2)
library(raster)
# array size
N = 10
J = 1
beta = 3
path.FIGS = 'images'


# cumputation
computeIsing <- function(num.iter, J, beta) {
  for(i in 1:num.iter) {
    # choose random spin
    x=round(runif(1,min=1,max=N))
    y=round(runif(1,min=1,max=N))
    # compute energy change to flip:
    nb = spin[(x %% N)+1,y] + spin[((x-2) %% N)+1,y] + 
      spin[x,(y %% N)+1] + spin[x,((y-2) %% N)+1]
    dE = 2*J*spin[x,y]*nb
    if (dE<0) { 
      spin[x,y] <<- -spin[x,y] 
    } else {
      # flip coin
      if (runif(1)< exp(-dE*beta)) {
        spin[x,y] <<-  -spin[x,y]
      }
    }
  }
}

# initialize array
spin = matrix(data=sign(runif(N*N)-0.5), nrow=N)
sum(spin)

plot(raster(spin))
computeIsing(100*N*N, J, beta)
plot(raster(spin))

Mavg = c()
# bSeq = seq(0.1,1, by=0.01)
TSeq = seq(0.5,5, by=0.1)
bSeq = 1/TSeq
for(b in bSeq) {
  spin = matrix(data=sign(runif(N*N)-0.5), nrow=N)
  computeIsing(400*N*N, J, b)
  Mavg = c(Mavg, sum(spin)/(N*N))
}


d = data.frame(
  beta = bSeq,
  M = Mavg
)
ggplot(d, aes(1/beta/J, abs(M))) +
  geom_point(col='red', size=2) + 
  ggtitle(paste('N=',N)) + 
  xlab('T/J') +
  ylab('|M|') +
  theme_bw()
ggsave(file.path(path.FIGS,paste0('Ising2D-',N,'.png')), width=4, height=3, dpi=220)
