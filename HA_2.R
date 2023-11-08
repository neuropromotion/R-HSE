# fuctions for two hemispheres:
pos.hemisphere = function(x){
  return(sqrt(1-x**2)) 
}
neg.hemisphere = function(x){
  return(-sqrt(1-x**2)) 
}

# generate 1000 uniform distributed dots (x and y cords):
x = runif(1000, -1, 1)
y = runif(1000, -1, 1)

#plot them 
plot(x,y,type='n', xlim=c(-1,1), ylim=c(-1,1))
points(x,y, col='black', pch=19, cex=0.4)
# O dot:
points(0,0, col='red', pch=19)

# plot circle with radius 1:
x.circle = seq(-1,1,length.out=100)
y.circle.pos = pos.hemisphere(x.circle)
y.circle.neg = neg.hemisphere(x.circle)
# plot two hemispheres:
lines(x.circle, y.circle.pos, lwd=2, col='pink')
lines(x.circle, y.circle.neg, lwd=2, col='pink')


answers.mat = x**2 + y**2 <= 1 # boolean matrix: which dots is in circle
probability = sum(answers.mat)/1000 # prob. occur within circle 
pi_exp = 4 * probability # multiple to square of square


throws = vector()
pi_s = vector()
samples = 10
for (i in 1:1000){
  x = runif(samples, -1, 1)
  y = runif(samples, -1, 1)
  
  answers.mat = x**2 + y**2 <= 1
  probability = sum(answers.mat)/samples
  pi_exp = 4 * probability
  
  throws = c(throws, samples)
  pi_s = c(pi_s, pi_exp)
  samples = samples + 50
}

plot(throws, pi_s, type='l', col='red', lwd=1, main='Dependency between \nthrows and pi value',
     xlab='Thrown', ylab='pi value', ylim=c(3.1,3.2))
lines(c(-100,5000000), c(pi,pi), lwd=2, col='blue')

















