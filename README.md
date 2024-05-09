# Tools and theory for modeling weight-at-size

A set of tools and background theory for modeling fish and crustacean weight-at-size from southern Gulf of Saint Lawrence data.

# Background

Weight-at-size equations are very common in animal observational studies. As their name implies, they serve to predict an animal's weight, which is generally harder to measure, to some convenient size measurement. These equations can be used to convert size-frequencies of a sampled population to equivalent weights. They can also be used to characterize different species or populations, or as controls to evaluate animal condition, since the weight of an animal also reflects some aspects of its health.

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

Formally we include in our model a second error distribution about the regression mean. The first error distribution models 
