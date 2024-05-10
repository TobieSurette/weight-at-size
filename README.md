# Tools and theory for modeling weight-at-size

Tools, analyses and background theory for modeling fish and crustacean weight-at-size from southern Gulf of Saint Lawrence data.

# Background

Weight-at-size equations are used to model the expected weights over some group of animals, such as a population in a region at a certain time of year.

Many factors influence an animal's weight at a given size. Major factors include sex, health, maturity, and the stage of its reproductive cycle. Some of these factors varyh though time, both seasonally and interannually, and region. 
This means that sampling location and season can have an influence on the resulting observations. In addition, there are factors which incluence the *observed* weight and makes it deviate from the *true* weight. The following diagram provides an overall view of the causal factors influencing weight meaasurement data.

![image](https://github.com/TobieSurette/weight-at-size/assets/14942142/bd76d48c-7b92-4ed6-ba37-8818494bf190)

Weights obtained though application a weight-at-size equation to size-frequency data are generally more convenient than measuring each animal individually. These equations can be used to convert size-frequencies of a sampled population to equivalent weights. They can also be used to characterize different species or populations, or as controls to evaluate animal condition, since the weight of an animal also reflects some aspects of its health.

The form of the equation is the allometric relation $w = \alpha x^\beta$, where $w$ is the animal's weight, $\alpha$ is the scaling coefficient, $x$ is a measure of animal size and $\beta$ is the dimensional scaling coefficient.

# September multispecies survey data

The first issue is that the precision of weight measurements has varied over the survey. Precision of weight measurements depends on the balance scales being used at sea. For years prior to 2000, the precisions were generally about 5 grams. Formally this means that weight measurements were rounded to the nearest multiple of 5. From 2000 to 2010, the precision of weight values were generally about 2 grams, while weight values were about 1 gram from 2011 onward. 

There are additional factors adding noise to weight measurements:
- Rough sea conditions
- data transcription errors
- species misclassifications
- damaged or mangled animals

The second issue is the occurence of outliers in the data. *Ad hoc* deletion of problem data works to a certain extent, especially when we have a large, precisely measured dataset, but we wish to have a general method to deal with outliers.

We treat the outliers as arising from an extra error source, i.e. extra variation in the weight from some perturbation such as misreading the balance scale or a transcription error. 

# Analytical approach



Formally we include in our model a second error distribution about the regression mean. The first error distribution models 



