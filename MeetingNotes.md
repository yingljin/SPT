# SPT

# 24/2/15
1. Do permutation test with t-statistics
2. Reduce grid to 32 by 32

# 24/2/22
1. Get rid of random effects. Repeat the procedure for only the two moving average errors.
- we'd like to figure out why the type I error is so far away from nominal values

# 24/2/29
Summary:
1. Permutation does not work any better than linear regression when conditioning on subject and time, because it is not rebust against spatial correlation (despite its robustness against non-normal distribution). Permuting the outcome breaks the spatial correlation.

Next steps:
1. Go along with the bootstrap track. But calculate empirical quantiles and compare to the observed value for rejection criterion
2. In bootstrap, sample blocks with different sizes (like 3, 5, 7,...)
3. We can also simulation under the alternative hypothesis and see the coverage rate/power

# 24/3/15
1. Remove random effect, generate only spatial correlated error, explore the effect of 1) filter size and 2) weights within the filter
- Perhaps plot variogram? 
2. Block bootstrap:
- explore different block size (maybe, decide block size based on variogram?)
- keep the expectation of the image size fixed. generation propobablity propotional to block size
3. Send cipiran the progress report
4. Send Andrew the robust spatial inference paper

# 24/3/21
1. For LM, increase sample size (maybe 1000) and figure out why type I error is lower than nominal value
2. Add the block bootstrap across alpha. 
- Use a weight function with a high decay rate (exp(-alpha*d^2) ? )
- Report distribution of estimated beta
3. Simulate under alternative hypothesis (beta=1 ? ), report coverage probability

# 24/3/26
1. In the block bootstrap, investigate the correlation between residuals, after substracting the fixed effects.
- Maybe semivariograms for residuals? 

https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9308704/

PS: how to simulate data that looks like lesion
- changing the step size of moving average? 

# 24/4/2
1. In data generation, look into different grid size
2. Try to generate data with spatial variogram that looks like the lesion ones
- Maybe two filters?
3. Correction factor? 

# 24/4/9
Some notes after talking with Antonio: 

1. Semivariograms should really be increasing. If it is not, it is probably a result for noise or heterogeneity (variance change across location).
- But alternatively, if we only interested in local correlation, we wouldn't need semivariograms! We only need the estimates filter weights for that. 
2. If the goal is only to simulate data that looks like the real data, it might be better to use standard image simulation model and learn the weight matrix
2. Block bootstrap: Hierarchical clustering


Meeting with Andrew:

1. Block bootstrap with the diverse weight matrix
2. Generate correlated images (Generative adversarial network, diffusion model, etc). 
3. The weird semivariogram may have to do with edge behaviors. Statistically, the edges pixels seem to be correlated with center pixels. 

