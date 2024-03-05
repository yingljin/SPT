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