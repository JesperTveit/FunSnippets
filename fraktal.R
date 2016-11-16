# Author: Jesper Tveit 
# date: 16.11.2016
#
# The following code plots an m by m visualization of the recursion relation 
# Z[n+1] = P + Z[n]^4, for P in the interval [-w/2,w/2]x[-i*w/2,i*w/2]
#
# O(m^2 x n) computations - give it a few seconds to finish ;)
#

w <- 2.8  # interval size
m <- 512  # image resolution 
n <- 250  # recursive steps

# series data
Za <- matrix(0,m,m)
Zb <- matrix(0,m,m)

# positions in the complex plane
Pa <- matrix((1/2 - c(1:m)/m)*w,m,m,TRUE)
Pb <- matrix((1/2 - c(1:m)/m)*w,m,m,FALSE)

# divergence 
divrg <- matrix(FALSE,m,m)

for(i in 1:150)
{
  Za1 <- Za*Za - Zb*Zb
  Zb1 <- 2*Za*Zb
  Za2 <- Pa + Za1*Za1 - Zb1*Zb1
  Zb2 <- Pb + 2*Za1*Zb1
  divrg <- Za2*Za2 + Zb2*Zb2 > 4
  Za <- ifelse(divrg, Za, Za2)
  Zb <- ifelse(divrg, Zb, Zb2)
}

image(exp(-Za*Za-Zb*Zb))
