# MonteCarlo-Pi
 simple pi generator to test speed in R
 
## Timing

Computing 2 million steps takes about 103 ms. A typical result is `3.142`, so result is about 0.01% accurate. 


## Model and Accuracy

The accuracy gets better with iterations. We can fit an power law that predicts the accuracy of the generated number. If accuracy **A** needs to be found for **n** iterations, then: **A = 97 n<sub>-0.586</sub>**


![Accuracy of Pi generated by Monte Carlo increases with number of iterations](images/pi-accuracy.png)


## Random Generator

The random generator is quite important, so here is a rendering of an image that shows the randomness of the generator. This image has 10000 data points:

![Randomness of the Generator](images/random-num-gen.png)