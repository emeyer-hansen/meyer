meyer - Novel & Classical Statistical Tools
==========

*Emil Niclas Meyer-Hansen*

This *R* package contains both novel and classical functions useful for anyone doing data analysis and statistical inference.

The package contributes with functions based on novel concepts developed by the author. These include the procedures of ``meyerization`` and an equation for determining the *degrees of freedom* for a prior *Student t*-distribution using the number of existing studies and their sample size. Another novel contribution is a function for determining the 'level' of a *Type M error* (Gelman & Carlin 2014) that mimicks the rules-of-thumb for similar metrics.

The remaining functions are drawn from classical concepts for statistical inference though some novelty exists in some of their applications in this package. These functions include determining the level of 'Practical' and 'Statistical Significance', a catch-all function for detecting all kinds of 'missing' and 'infinite' values. Other functions allow computing the *pooled standard error* (Rubin 1987), the *critical value* of a test statistic, the *p*-value, and *confidence intervals*. Contrary to existing implementations of these classical functions, this package provides improved error handling, easing their use.

## Installation

To install this R package, follow these steps:

(1) Ensure you have the latest version of *R* and RStudio installed.
(2) Load the ``devtools`` package (or install it if you haven't already).
(3) Install the ``meyer`` *R* package. With the package currently *not* on CRAN, this must be done through the author's GitHub.

Steps 2 and 3 can be done by copy-pasting the following code into R and executing it:

```
# install.packages("devtools")
library(devtools)
devtools::install_github("emeyer-hansen/meyer")
```

## Rescale data with `meyerization`

*meyerization* refers to the procedure of rescaling data to make it analogous to a correlation coefficient (e.g., Pearson's *r*). For any normally distributed variable, this ensures that it has an approximate mean of 0 and asymptotic range [-1; 1].

For linear regression, using `meyerization` on both a continuous predictor *x* and outcome *y*, allows the coefficient to be interpreted as a correlation coefficient. It also allows the coefficient of the effect of *x* on *y* to be interpreted as the following: An increase (decrease) in the predictor from its mean to its highest (lowest) value is associated with an increase (decrease) in the outcome from its mean to its highest (lowest) value by the size of the coefficient.

```
library(meyer)

# Random data
set.seed(1791)
n <- 1000
x = rnorm(n)
data <- data.frame(
  x,
  y = runif(1, 0.1, 10)*x+rnorm(n)
)

# Meyerizing
data$x <- meyerize(data$x)
data$y <- meyerize(data$y)

# Mean approx 0
format(mean(data$x), scientific = F)
format(mean(data$y), scientific = F)

# Range approx [-1; 1]
range(data$x)
range(data$y)

# Pearson's r
round(cor(data$y, data$x), =3)

# Meyerized coefficient
round(summary(lm(y~x, data))$coefficients[2,1], 3)
```

## Determine df for *t*-distribution with `meyersDF`

This function uses a novel equation to assist researchers working within the framework of Bayesian statistics in specifying their priors as a *t*-distribution. The *Student t*-distribution (Gosset 1908) is highly useful due to its properties of *maximum entropy* and *conjugacy* with the *Cauchy* and (Gaussian) *Normal* distribution, where the former is typically used as a relatively skeptical prior, while the latter is less skeptical in comparison. The *t*-distribution is specified using a mean and standard deviation (or variance) parameter, as well as *degrees of freedom* (df), which control the width of its tails, where lower values result in 'fat' tails, and higher values 'slimmer' tails. When df is 1, the *t*-distribution and the *Cauchy*-distribution are equivalent, and when df is infinite, the *t*-distribution and the *normal* distribution are equivalent. This means that df can be used to shape the degree of skepticism in the distribution: The lower the df, the greater the skepticism.

The `meyersDF` function provides an equation that determines df as a function of the number of previously conducted studies and their sample size. This mimicks a qualitive assessment but comes with the advantage of quantifying the assessment and make it reproducible. With few studies and/or small sample size, the df is rather low to indicate skepticism towards the results from the given field of research. With many studies and/or a large sample size, df is higher and converges to infinity, thus making the distribution equivalent to a prior specified with the Normal distribution. It enables the researcher to specify an arbitrary bias that serves to reflect skepticism towards dubious fields with tradition for using small sample sizes and that systematically fail to replicate, e.g., psychology (Open Science Collaboration ?).

```
library(meyer)

# Random average sample size
set.seed(1791)
n <- rnorm(1, 1000, 200)
round(n)

# Random number of studies
s <- round(runif(1, 1, 3))
s

# Resulting df
df <- meyersDF(n, s, bias = 40)
df
```

## 'Exaggeration Level' with `typeMlevel`

A *Type M error* reflects the magnitude by which a quantity of interest has been over-/underestimated (Gelman & Carlin 2014). With values closer to 1 indicating greater precision, rules-of-thumb of the 'level' of exaggeration can be established to serve as a heuristic for researchers on how to evaluate a Type M error.

Mimicking conventional rules-of-thumb for similar metrics, I propose the following levels to help researchers assess whether a Type M error is acceptable: 5%, 1%, and 0.1%, with lower levels indicative of greater precision. Note that the appropriateness of these levels vary across contexts, and that their use does *not* absolve the researcher of using qualitative insight to better assess the impact of a Type M error. Other levels are likely more appropriate for different fields of research and for different quantities of interest.

```
library(meyer)

# Random Type M errors
typeM <- c(1, 1.001, 0.999, 1.01, 0.99, 1.05, 0.95, 0.949, 1.051)

# Type M levels
typeMlevel(typeM)
```

## Power Assessment

*Statistical power* refers to the ability to correctly detect an effect and is related to the *false positive* (Type I) error rate (Cohen 1988). When doing a *power analysis*, researchers often aim for a power of 80% and Type I error of 5%. If the sample size is known, it is possible to calculate the smallest expectedly detectable coefficient. However, sometimes this expectedly smallest detectable coefficient doesn't test 'statistically significant', while values lower than this do test 'statistically significant'. It can be in the interest of the researcher to identify these instances and take precaution when taking such results at face value. 

```
library(meyer)

# Random data
r <- c(0.04, 0.04, 0.051, 0.051)
min_detectable_r <- 0.05
p_value <- c(0.05, 0.06, 0.05, 0.06)
alpha <- 0.05

# Power Assessment
assessPower(r, min_detectable_r, p_value, alpha)
```

## 'Practical Significance' Level with `pracSigLevel`

Determines the level of 'practical significance' for a given type of effect size (e.g., Pearson's *r*, Cohen's *d*). Allows the user draw on a series of rules-of-thumb developed by various researchers, (e.g., Lovakov & Agadullina 2021).

```
library(meyer)

# Typical cutoffs
effect_sizes <- c(0.1, 0.2, 0.5, 0.8, 1.2, 2, 2.1)

# Practical Significance Level
pracSigLevel(effect_sizes)
```

## 'Statistical Significance' Level with `statSigLevel`

Simple function for determining the level of 'statistical significance' of the provided *p*-values (Fisher 1925; Neyman & Pearson 1933). The function allows using 'conventional' levels (i.e., 5%, 1%, 0.1%), as well as 'pseudo' levels (i.e., 10%).

```
library(meyer)

# p-Values at typical cutoffs
p_values <- c(0.11, 0.1, 0.05, 0.01, 0.001)

# Using 'conventional' rules
statSigLevel(p_values)

# Using 'pseudo' rules
statSigLevel(p_values, 'pseudo')
```

## Detect 'Missingness' with `is.missing`

By grouping the numerous built-in functions necessary for detecting the plethora of 'missing' values, this function eases the workflow of data processing.

`is.missing` simultaneously checks the input data for NA, NaN, and NULL values, while the function `is.missing.or.infinite` extends to detecting infinite values as well.

```
library(meyer)

# Random data
set.seed(1791)
x <- c(rnorm(3), NA, NaN, NULL, Inf, -Inf)

# is.missing
is.missing(x)

# is.missing.or.infinite
is.missing.or.infinite(x)
```

## Pooled Standard Error with `pooledSE`

This function computes the *pooled standard error* (Rubin 1987), which is typically used assessing uncertainty in the estimated quantities of interest when imputing missing values. This is due to the equation accounting for both within and between variance.

```
library(meyer)

# Random data
set.seed(1791)
m <- round(rnorm(1, 100, 25))
se <- rexp(m, 1)
q <- rnorm(m)

# Pooled SE
round(pooledSE(se, q, m), 3)
```

## Critical Value with `criticalValue`

The *critical value* is the demarcation point between a 'statistically significance' and 'insignificant' result. While there is nothing new about its application here, its inclusion helps assessing the critical value with different multiple hypothesis testing correction methods.

```
library(meyer)

# Typical alphas, df
alpha <- c(0.05, 0.01)
df <- Inf

# Critical value
criticalValue(alpha, df)
```

## Compute *p*-values with `pValue`

Function for computing the (in)famous *p*-value using test statistics. This is no new invention (Pearson 1900; Fisher 1925) but the version provided here allows accounting for underflow, whereby small values are rounded to zero (Demmel 1984). This can be fixed by upward adjusting *p*-values and is necessary if using adjustments for *multiple hypothesis testing* (e.g., *Bonferroni* correction; Miller 1966).

```
library(meyer)

# t-statistics
t_stat <- c(1.95, 1.96, 2)

# p-values
round(pValue(t_stat, correction = T), 3)
```

# Confidence Intervals with `ci`

Another classical function based on the work of Jerzy Neyman (1937) that simply computes the *confidence interval* with the specified level of certainty.

```
library(meyer)

# Random data
set.seed(1791)
n <- 5

# Quantity of interest
x <- rnorm(n)
round(x, 3)

# Standard error
se <- rexp(n)
round(se, 3)

# 95\% CI
ci(x, se)
```

## Special Thanks

I developed this R package in my spare time while doing an MSc in Political Science at Aarhus University, Denmark, and draws heavily from lessons learned from the courses 'Advanced Quantitative Methodology: Causal Inference', 'Political Data Science: Digital Skills & Applied Statistical Analysis', and 'Bayesian Statistics'. I want to thank these course instructors for their inspirational teaching:
- Rasmus Skytte
- Julian Schuessler
- Martin Bisgaard
- Jonathan Doucette

For daring to question the established scientific institutions and for their astounding work towards making science more credible, I extend an especial 'thank you' to:
- Andrew Gelman
- Brian Nosek
- Uri Simonsohn
- Joe Simmons
- Leif D. Nelson

## Literature

- Cohen, J. (1988): *Statistical Power Analysis for the Behavioral Sciences*, 2nd edition. Routledge.
- Demmel, J. (1984): 'Underflow & the Reliability of Numerical Software', *SIAM Journal on Scientific & Statistical Computing*, 5(4): 887-919.
- Fisher, R. A. (1925): *Statistical Methods for Research Workers*. Oliver & Boyd.
- Gelman, A. & J. Carlin (2014): 'Beyond Power Calculations: Assessing Type S (Sign) & Type M (Magnitude) Errors', *Perspectives on Psychological Science*, 9(6): 641-651.
- Gosset, W. S. (1908): 'The Probable Error of a Mean', *Biometrika*, 6(1): 1–25.
- Lovakov, A. & E. R. Agadullina (2021): 'Empirically Derived Guidelines for Effect Size Interpretation in Social Psychology', *European Journal of Social Psychology*, 51(3): 485-504.
- Miller, R. G. (1966): *Simultaneous Statistical Inference*. Springer.
- Neyman, J. (1937): 'Outline of a Theory of Statistical Estimation Based on the Classical Theory of Probability', *Philosophical Transactions of the Royal Society A*, 236(767): 333-380.
- Neyman, J. & E. S. Pearson (1933): 'The Testing of Statistical Hypotheses in Relation to Probabilities a priori', *Mathematical Proceedings of the Cambridge Philosophical Society*, 29(4): 492-510.
- Pearson, K. (1900): 'On the criterion that a given system of deviations from the probable in the case of a correlated system of variables is such that it can be reasonably supposed to have arisen from random sampling', *Philosophical Magazine*, 50(302): 157-175.
- Rubin, D. B. (1987): *Multiple Imputation for Nonresponse in Surveys*. Wiley.
