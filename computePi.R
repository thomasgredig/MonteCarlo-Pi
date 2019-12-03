######################################
# Monte Carlo method for computing Pi
# Adaption from earlier routine in C
#
# (c) 2019 Thomas Gredig
#
######################################

# number of iterations
NUM = 2e6

# computes pi with num.iterations
computePi <- function(num.iterations) {
  q.x = runif(num.iterations)
  q.y = runif(num.iterations)
  l = q.x*q.x+q.y*q.y
  n = length(which(l<=1))
  n/num.iterations*4
}

# determine time
system.time({
  result.pi = computePi(NUM)
})

r=data.frame()
for(i in seq(from = 1E5, to=1E6, length.out=30)) {
  result.pi = computePi(i)
  accuracy = (result.pi / pi - 1) * 100
  r = rbind(r, data.frame(i, pi = result.pi, accuracy))
}

library(ggplot2)
ggplot(r, aes(i/1e6, abs(accuracy))) + 
  geom_point(col='red', size=2) +
  scale_y_log10() + 
  scale_x_log10() + 
  theme_bw() + 
  xlab('iterations in millions') + 
  ylab('accuracy (%)')
ggsave('pi-accuracy.png', width=4,height=3, dpi=300)


r1 = subset(r, i>0.5e6)
plot(r1$i, r1$pi)
abline(h=pi, col='red', lwd=2)


# check random numbers
dr = data.frame(
  x = runif(10000),
  y = runif(10000))
ggplot(dr, aes(x,y)) + geom_point(col='red', size=0.2) + theme_bw()
ggsave('random-num-gen.png', width=4, height=4, dpi=300)
