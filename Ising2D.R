#########################################
# Compute the temperature dependence of the
# Magnetization for the Ising 2D model and
# test the speed in R
#
# see https://arxiv.org/pdf/0803.0217.pdf
# see http://micro.stanford.edu/~caiwei/me334/Chap12_Ising_Model_v04.pdf 
# see https://github.com/basilwong/monte-carlo-2D-ising
#
# (c) 2019 Thomas Gredig
#########################################


# Ising2D Model Parameters
##########################
library(ggplot2)
library(raster)

N = 10  # array size
J = 1   # interaction strength
beta = 3  # inverse temperature
conv = 400   # convergence factor
path.FIGS = 'images'
path.DATA = 'data'
file.runTime = file.path(path.DATA,'runTimes.csv')

# Computation
#############
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
      if (runif(1) < exp(-dE*beta)) {
        spin[x,y] <<-  -spin[x,y]
      }
    }
  }
}

# Array Initialization
######################
spin = matrix(data=sign(runif(N*N)-0.5), nrow=N)

# Sample Output
###############
plot(raster(spin))
computeIsing(100*N*N, J, beta)
plot(raster(spin))


# Computation Intesive Run: M vs T
##################################
d.runTimeAll = read.csv(file.runTime, stringsAsFactors = FALSE)
d.runTime = data.frame(N,conv,date=Sys.Date(), start.time=Sys.time(),end.time=0,diff.s=0)
Mavg = c()
TSeq = seq(0.5,5, by=0.1)
bSeq = 1/TSeq
for(b in bSeq) {
  spin = matrix(data=sign(runif(N*N)-0.5), nrow=N)
  computeIsing(conv*N*N, J, b)
  Mavg = c(Mavg, sum(spin)/(N*N))
}
d.runTime$end.time = Sys.time()
d.runTime$diff.s = as.numeric(d.runTime$end.time-d.runTime$start.time)
d.runTimeAll = rbind(d.runTimeAll,d.runTime)
write.csv(d.runTimeAll,file=file.runTime,row.names = FALSE)

# Graphing of Data
##################
d = data.frame(
  beta = bSeq,
  M = Mavg
)
ggplot(d, aes(1/beta/J, abs(M))) +
  geom_point(col='red', size=2) + 
  ggtitle(paste('N=',N,'x',N,' conv=',conv)) + 
  xlab('T/J') +
  ylab('|M|') +
  theme_bw()
ggsave(file.path(path.FIGS,paste0('Ising2D-',N,'x',N,'-c',conv,'.png')), width=4, height=3, dpi=220)
write.csv(d,file.path(path.DATA,paste0('Ising2D-',N,'x',N,'-c',conv,'.csv')), row.names=FALSE)
