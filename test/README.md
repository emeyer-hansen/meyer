[_metadata_:author]:- "Emil Niclas Meyer-Hansen"
[_metadata_:date]:- "2/12/2023"
[_metadata_:tags]:- "markdown metadata"
meyer - Novel & Classical Statistical Tools
==========

*Emil Niclas Meyer-Hansen*

This *R package* contains both novel and classical functions useful for anyone doing data analysis and statistical inference.

The package contributes with functions based on novel concepts developed by the author. These include the concept of *meyerization* and an equation to determine the *degrees of freedom* for a prior *Student t*-distribution that reflects the strength of existing evidence. Another novel contribution is a function for determining the 'level' of a *Type M error* (Gelman & Carlin 2014), mimicking the rules-of-thumb used for similar metrics.

The remaining functions are drawn from classical concepts for statistical inference. These include functions for determining the levels of 'Practical' and 'Statistical Significance', and a catch-all function for detecting 'missing' and 'infinite' values. Other functions allow computing the *pooled standard error* (Rubin 1987), the *critical value* of a test statistic, the *p*-value, and *confidence intervals*. Contrary to existing implementations of these classical functions, this package provides improved error handling, easing their use.

## Table of Contents
- [Installation](#installation)
- [Meyerization](#meyerize)
- [Meyer's DF](#meyersDF)
- [Type M Level](#typeMlevel)
- [Power Assessment](#power)
- ['Practical Significance' Level](#pracSigLevel)
- ['Statistical Significance' Level](#statSigLevel)
- [Missingness](#missingness)
- [Pooled Standard Error](#pooledSE)
- [Critical VAlue](#criticalValue)
- [p-Value](#pValue)
- [Confidence Interval](#ci)
- [Changelog](#changelog)
- [Feedback](#feedback)
- [Citation](#citation)
- [Acknowledgements](#acknowledgements)
- [Literature](#literature)

<a id="installation"></a>
## Installation

To install this R package, please follow these steps:

(1) Ensure you have the latest version of *R* and RStudio installed.
(2) Load the ``devtools`` package (or install it if you haven't already).
(3) Install the ``meyer`` *R package*. With the package currently *not* on CRAN, this must be done through the author's GitHub.

Steps 2 and 3 can be done by copy-pasting the following code into R and executing it:

```r
# install.packages("devtools")
library(devtools)
devtools::install_github("emeyer-hansen/meyer")
```

<a id="meyerize"></a>
## Rescale data with `meyerize`

The novel procedure of *meyerization* involves rescaling a numeric vector *x* similar to that of a *correlation coefficient*, characterized by a range of [-1; 1], which readers are likely most familiar with in the form of Pearson's *r*. By rescaling data in this manner, coefficients from linear regressions become analogous to - and interpretable as - a correlation coefficient. 

In a manner similar to Charles P. Winsor's *winsorization*, I term this rescaling procedure *meyerization*, and for those wishing for a less self-indulgent name, I suggest the alternative name of *r score transformation*.

Derived from other rescaling procedures (e.g., *normalisation*), the formula used to *meyerize* vector $x$ is:

$$x_{meyerized} = 2\dfrac{x_i - x_{min}}{x_{max} - x_{min}} - \dfrac{1}{n}\sum_{i=1}^{n} 2\dfrac{x_i - x_{min}}{x_{max} - x_{min}}, n \in \mathbb{N}, x \in \mathbb{R}, x_1,\dots, x_n$$
Unlike similar approaches, this formula ensures that the rescaled vector $x$ has an approximate mean of $0$, and assuming $x$ is normally distributed, an asymptotic range $[-1; 1]$. This contrasts its *min-maxing* counterpart, which instead ensures the range $[-1; 1]$, and assuming normality, an asymptotic mean of $0$.

When applied to the context of linear regression, *meyerizing* both a continuous predictor *x* and outcome *y* enables the coefficient to be interpreted and compared to a correlation coefficient and can be meaningfully interpreted as the following: An increase (decrease) in the predictor from its mean to its highest (lowest) value is associated with an increase (decrease) in the outcome from its mean to its highest (lowest) value by the size of the coefficient.

Here is a demonstration:

```r
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
round(cor(data$y, data$x), 3)

# Meyerized coefficient
round(summary(lm(y~x, data))$coefficients[2,1], 3)
```

<a id="meyersDF"></a>
## Determine df for *t*-distribution with `meyersDF`

This function uses a novel equation to assist researchers working within the framework of *Bayesian statistics* in specifying their priors with a *t*-distribution. The *Student t*-distribution (Gosset 1908) is highly useful due to its properties of *maximum entropy* (McElreath 2015: 179) and relation to the *Cauchy* and (Gaussian) *Normal* distribution. Where the former is typically used as a relatively weakly informative prior, the latter is less skeptical in comparison.

The *t*-distribution is specified using a mean and standard deviation (or variance) parameters, as well as a *degrees of freedom* (df), which controls the width of its 'tails'. Lower values result in 'fatter' tails and higher values in 'slimmer' tails. When *df* is 1, the *t*-distribution and the *Cauchy*-distribution are equivalent, and when *df* is infinite, the *t*-distribution and the *normal* distribution are equivalent. This means that *df* can be used to shape the degree of skepticism in the distribution: The lower the *df*, the greater the skepticism.

The `meyersDF` function provides an equation that determines *df* as a function of the number of previously conducted studies and their sample size. This mimics a qualitive assessment of the strength of previous evidence, all while quantifying and making the assessment reproducible. With few studies and/or a small sample size, the *df* is rather low (with a limit of 1) to indicate skepticism towards the results from the given field of research. With many studies and/or a large sample size, *df* is higher, converging to infinity, gradually making the prior distribution equivalent to a Normal distribution.

The following equation is used by `meyersDF`:

$$df = \bar n + ln(2 + s^{0.5})^{\sqrt{\bar n}} - bias, \bar n \in \mathbb{R^+}, s \in \mathbb{N}, bias \in \mathbb{R},$$

where $ln$ is the natural logarithm, $s$ is the number of existing relevant studies in the research field, $\bar n$ is their average sample size, and $bias$ is an qualitatively determined value that serves to reflect skepticism towards dubious fields with tradition for using small sample sizes and that systematically fail to replicate, e.g., psychological science (Open Science Collaboration 2015).

Here is an example:

```r
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

<a id="typeMlevel"></a>
## 'Exaggeration Level' with `typeMlevel`

A *Type M error* reflects the magnitude by which a quantity of interest has been over-/underestimated (Gelman & Carlin 2014). With values closer to 1 indicating greater precision, rules-of-thumb of the 'level' of exaggeration can be established to serve as a heuristic for researchers on how to evaluate a Type M error.

Mimicking conventional rules-of-thumb for similar metrics, I propose the following levels of acceptable Type M errors: 5%, 1%, and 0.1%. Note that these levels are rather arbitrary, not equally appropriate across contexts, and that their use does *not* absolve the researcher of critical thinking, with domain-specific knowledge likely being a better source of assessing what is an 'acceptable' Type M error.

Example:

```r
library(meyer)

# Random Type M errors
typeM <- c(1, 1.001, 0.999, 1.01, 0.99, 1.05, 0.95, 0.949, 1.051)

# Type M levels
typeMlevel(typeM)
```

<a id="power"></a>
## Power Assessment

*Statistical power* refers to the ability to *correctly* detect an effect and is related to the *false positive* (Type I) error rate (Cohen 1988). When doing a *power analysis*, researchers often aim for a power of 80% and Type I error of 5%. If the sample size is known, it is possible to calculate the smallest expectedly detectable coefficient. However, sometimes this expectedly smallest detectable coefficient fail to test 'statistically significant', while values lower than this unexpectedly test 'statistically significant'. It can be in the interest of the researcher to identify these instances and take precaution when interpreting and theorizing from such results.

Example:

```r
library(meyer)

# Random data
r <- c(0.04, 0.04, 0.051, 0.051)
min_detectable_r <- 0.05
p_value <- c(0.05, 0.06, 0.05, 0.06)
alpha <- 0.05

# Power Assessment
assessPower(r, min_detectable_r, p_value, alpha)
```

<a id="pracSigLevel"></a>
## 'Practical Significance' Level with `pracSigLevel`

Determines the level of 'practical significance' for a given type of effect size (e.g., Pearson's *r*, Cohen's *d*). Allows the user to apply a series of different rules-of-thumb developed by various researchers, (e.g., Lovakov & Agadullina 2021).

Example:

```r
library(meyer)

# Typical cutoffs
effect_sizes <- c(0.1, 0.2, 0.5, 0.8, 1.2, 2, 2.1)

# Practical Significance Level
pracSigLevel(effect_sizes)
```

<a id="statSigLevel"></a>
## 'Statistical Significance' Level with `statSigLevel`

Simple function for determining the level of 'statistical significance' using the provided *p*-values (Fisher 1925; Neyman & Pearson 1933). The function allows using 'conventional' levels (i.e., 5%, 1%, 0.1%), as well as 'pseudo' levels (i.e., 10%).

Example:

```r
library(meyer)

# p-Values at typical cutoffs
p_values <- c(0.11, 0.1, 0.05, 0.01, 0.001)

# Using 'conventional' rules
statSigLevel(p_values)

# Using 'pseudo' rules
statSigLevel(p_values, 'pseudo')
```

<a id="missingness"></a>
## Detect 'Missingness' with `is.missing`

By grouping the numerous built-in functions necessary for detecting the plethora of 'missing' values together, this function eases the workflow of data processing and analysis.

`is.missing` simultaneously checks the input data for NA, NaN, and NULL values, while the function `is.missing.or.infinite` extends to detecting infinite values as well.

Example:

```r
library(meyer)

# Random data
set.seed(1791)
x <- c(rnorm(3), NA, NaN, NULL, Inf, -Inf)

# is.missing
is.missing(x)

# is.missing.or.infinite
is.missing.or.infinite(x)
```

<a id="pooledSE"></a>
## Pooled Standard Error with `pooledSE`

This function computes the *pooled standard error* (Rubin 1987), which is typically used for assessing uncertainty when imputing missing values in some *quantity of interest* (QOI). This metric corresponds to the *total variance* ($V_T$), which accounts for *within variance* ($V_W$), *between variance* ($V_B$), and the number of imputations ($\frac{V_B}{m}$). `pooledSE` uses the following equations (Heymans & Eekhout 2019):

$$V_W = \dfrac{1}{m}\sum_{i=1}^{m}SE_i^{2}, m \in \mathbb{N}, SE \in \mathbb{R^{+}}, SE_1,\dots, SE_m,$$
with $m$ being the number of imputed datasets, $SE$ is a vector of (estimated) *standard errors* of the QOI.

$$V_B = \dfrac{1}{m-1}\sum_{i=1}^{m}(\theta_i-\bar{\theta})^{2}, m \in \mathbb{N}, \theta \in \mathbb{R}, \theta_1, \dots, \theta_m,$$
where $m$ is the number of imputed datasets, $\theta$ is a vector of the QOI and $\bar{\theta}$ its arithmetic mean. $n-1$ is *Bessel's correction* (Radziwill 2017).

$$V_T = V_W+V_B+\dfrac{V_B}{m}, \{V_W, V_B\} \in \mathbb{R^{+}}, m \in \mathbb{N}$$
Again, $m$ is the number of imputations, $V_W$ is the within variance, and $V_B$ is the between variance.

This finally leads to the *pooled standard error*, which is the square-root of the *total variance* $V_T$:

$$SE_{Pooled} = \sqrt{V_T}, V_T \in \mathbb{R^{+}}$$
Example:

```r
library(meyer)

# Random data
set.seed(1791)
m <- round(rnorm(1, 100, 25))
se <- rexp(m, 1)
q <- rnorm(m)

# Pooled SE
round(pooledSE(se, q, m), 3)
```

<a id="criticalValue"></a>
## Critical Value with `criticalValue`

The *critical value* is the demarcation point between a 'statistically significance' and 'insignificant' result. While there is not much new to its application here, it includes improved error handling.

Example:

```r
library(meyer)

# Typical alphas, df
alpha <- c(0.05, 0.01)
df <- Inf

# Critical value
criticalValue(alpha, df)
```

<a id="pValue"></a>
## Compute *p*-values with `pValue`

Function for computing the (in)famous *p*-value using test statistics. This is no new invention (Pearson 1900; Fisher 1925) but the version provided here allows accounting for underflow, whereby small values are rounded to zero (Demmel 1984). This can be fixed by upward adjusting *p*-values and is necessary to correctly adjust for *multiple hypothesis testing* (e.g., *Bonferroni* correction; Miller 1966).

Example:

```r
library(meyer)

# t-statistics
t_stat <- c(1.95, 1.96, 2)

# p-values
round(pValue(t_stat, correction = T), 3)
```

<a id="ci"></a>
## Confidence Intervals with `ci`

Another classical function based on the work of Jerzy Neyman (1937). `ci` simply computes the *confidence interval* of the specified level of certainty using the equation:

$$CI = \bar{\theta} \pm SE\times z, \bar{\theta} \in \mathbb{R}, z \in \mathbb{R^+},$$
where $\bar{\theta}$ is the arithmetic mean of some *quantity of interest*, $z$ is the critical value, $SE$ is the (estimated) standard error of $\bar{\theta}$.

Example:

```r
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

<a id="feedback"></a>
## Feedback

As a scientist, I highly value feedback and constructive criticism. I urge any and all to provide suggestions for improvement, including bugfixes, optimized code, and additions to the repertoire of functions in this *R* package.

<a id="changelog"></a>
## Changelog

- [update] 19 Dec 2023: Fix to *pracSigLevel* function, enabling the significance levels of Funder & Ozer (2019) to be used. Improved descriptions, added feedback and changelog sections.
- [small change] 5 Dec 2023: Updated descriptions, acknowledgements.
- [release] 2 Dec 2023: Initial version.

<a id="citation"></a>
## Citation

```
@software{MeyerHansen_meyer_2023,
    author = {Meyer-Hansen, Emil N.},
    month = {12},
    title = {{meyer: Novel and Classical Statistical Tools}},
    url = {https://github.com/emeyer-hansen/meyer},
    version = {0.1},
    year = {2023}
}
```

<a id="acknowledgements"></a>
## Acknowledgements

I developed this R package in my spare time while doing an MSc in Political Science at Aarhus University, Denmark. It draws heavily from lessons learned from the courses 'Advanced Quantitative Methodology: Causal Inference', 'Political Data Science: Digital Skills & Applied Statistical Analysis', and 'Bayesian Statistics', and I want to thank the instructors of these excellent courses for their inspirational teaching:
- [Rasmus Skytte](https://pure.au.dk/portal/en/persons/raks%40ps.au.dk)
- [Julian Schuessler](https://pure.au.dk/portal/en/persons/julians%40ps.au.dk)
- [Martin Bisgaard](https://pure.au.dk/portal/en/persons/mbisgaard%40ps.au.dk)
- [Matt Loftis](https://dk.linkedin.com/in/mattwloftis)
- [Jonathan Doucette](https://vbn.aau.dk/da/persons/156937)

While grateful to everyone that have contributed to the development of statistics and data science, I specifically thank those that dare to challenge faulty scientific practices and work towards making science more credible, especially the following:
- [Andrew Gelman](http://www.stat.columbia.edu/~gelman/)
- [Brian Nosek](https://www.cos.io/team/brian-nosek)
- [Uri Simonsohn](https://urisohn.com/)
- [Joseph Simmons](https://oid.wharton.upenn.edu/profile/jsimmo/)
- [Leif D. Nelson](https://haas.berkeley.edu/faculty/nelson-leif/)

<a id="literature"></a>
## Literature

- Cohen, J. (1988): *Statistical Power Analysis for the Behavioral Sciences*, 2nd edition. Routledge.
- Demmel, J. (1984): 'Underflow & the Reliability of Numerical Software', *SIAM Journal on Scientific & Statistical Computing*, 5(4): 887-919.
- Fisher, R. A. (1925): *Statistical Methods for Research Workers*. Oliver & Boyd.
- Funder, D. C. & D. J. Ozer (2019): 'Evaluating Effect Size in Psychological Research: Sense & Nonsense', *Advances in Methods & Practices in Psychological Science*, 2(2): 155-168.
- Heymans, M. W. & I. Eekhout (2019): "Rubin's Rules", in *Applied Missing Data Analysis With SPSS & (R)Studio*, chapter 9.
- Gelman, A. & J. Carlin (2014): 'Beyond Power Calculations: Assessing Type S (Sign) & Type M (Magnitude) Errors', *Perspectives on Psychological Science*, 9(6): 641-651.
- Gosset, W. S. (1908): 'The Probable Error of a Mean', *Biometrika*, 6(1): 1–25.
- Lovakov, A. & E. R. Agadullina (2021): 'Empirically Derived Guidelines for Effect Size Interpretation in Social Psychology', *European Journal of Social Psychology*, 51(3): 485-504.
- McElreath, R. (2015): *Statistical Rethinking: A Bayesian Courses with Examples in R*, 2nd edition. Chapman & Hall/CRC.
- Miller, R. G. (1966): *Simultaneous Statistical Inference*. Springer.
- Neyman, J. (1937): 'Outline of a Theory of Statistical Estimation Based on the Classical Theory of Probability', *Philosophical Transactions of the Royal Society A*, 236(767): 333-380.
- Neyman, J. & E. S. Pearson (1933): 'The Testing of Statistical Hypotheses in Relation to Probabilities a priori', *Mathematical Proceedings of the Cambridge Philosophical Society*, 29(4): 492-510.
- Open Science Collaboration (2015): 'Estimating the Reproducibility of Psychological Science', *Science*, 349(6251): 943.
- Pearson, K. (1900): 'On the criterion that a given system of deviations from the probable in the case of a correlated system of variables is such that it can be reasonably supposed to have arisen from random sampling', *Philosophical Magazine*, 50(302): 157-175.
- Radziwill, N. M. (2017): *Statistics (the easier way) with R*. Lapis Lucera.
- Rubin, D. B. (1987): *Multiple Imputation for Nonresponse in Surveys*. Wiley.

---
Revised 20.12.2023 - [Emil Niclas Meyer-Hansen](mailto:emil098meyerhansen@gmail.com)
