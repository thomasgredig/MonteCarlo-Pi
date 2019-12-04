######################################
# Monte Carlo method for computing Pi
# Adaption from earlier routine in C
#
# Using multiple cores
#
# (c) 2019 Thomas Gredig
#
######################################
library(parallel)
library(foreach)
library(doParallel)

num.cores = detectCores()
registerDoParallel(num.cores) 
print(paste("Using",num.cores,"cores."))


# number of iterations and figure path
NUM = 100e6

path.FIGS = 'images'
library(ggplot2)

# computes pi with num.iterations
computePi <- function(num.iterations) {
  q.x = runif(num.iterations)
  q.y = runif(num.iterations)
  l = q.x*q.x+q.y*q.y
  n = length(which(l<=1))
  n/num.iterations*4
}

# computes pi with num.iterations
computePiParallel <- function(num.iterations) {
  c <- function(no) {
    foreach (i=1:num.cores,.combine=rbind) %dopar% {
      q.x = runif(no)
      q.y = runif(no)
      l = q.x*q.x+q.y*q.y
      n = length(which(l<=1))
    }
  }
  sum(c(num.iterations/num.cores))/num.iterations*4
}

# determine time
system.time({
  result.pi = computePi(NUM)
  print(result.pi)
})

system.time({
  result.pi = computePiParallel(NUM)
  print(result.pi)
})



r=data.frame()
for(i in seq(from = 1E4, to=100E6, length.out=30)) {
  result.pi = computePiParallel(i)
  accuracy = (result.pi / pi - 1) * 100
  r = rbind(r, data.frame(i, pi = result.pi, accuracy))
}
write.csv(r[,1:3], file='pi-simulation-data.csv', row.names = FALSE)

# add a fit using an exponential model
r$accuracyABS = abs(r$accuracy)
nls(data = r,
    accuracyABS ~ A*i^(w),
    start = list(A=1e2,w=-0.59)) -> fit
summary(fit)
q1 = predict(fit, list(i=r$i))
r1 = data.frame(i = r$i,
                pi = pi,
                accuracy = q1,
                accuracyABS = q1)
r1$type = 'model'
str(r1)
r$type = 'Monte Carlo'
str(r)
r2 = rbind(r,r1)
r2$type = factor(r2$type)

ggplot(r, aes(i/1e6, accuracyABS, color=type)) + 
  geom_point( size=2.5, col='black') + geom_point( size=2, alpha=0.5) +
  geom_line(data=r1, size=2, alpha=0.7) + 
  scale_y_log10() + 
  scale_x_log10() + 
  theme_bw() + 
  xlab('iterations in millions') + 
  ylab('accuracy (%)') + 
  theme(legend.position = c(0.05,0.05),
        legend.justification = c(0,0))
ggsave(file.path(path.FIGS,'pi-accuracy.png'), width=4,height=3, dpi=300)


# pi plot
r1 = subset(r, i>0.5e6)
plot(r1$i, r1$pi)
abline(h=pi, col='red', lwd=2)


# check random numbers
dr = data.frame(
  x = runif(10000),
  y = runif(10000))
ggplot(dr, aes(x,y)) + geom_point(col='red', size=0.2) + theme_bw()
ggsave(file.path(path.FIGS,'random-num-gen.png'), width=4, height=4, dpi=220)
