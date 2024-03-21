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
3. Simulate under alternative hypothesis (beta=1 ? ), report coverage probablity