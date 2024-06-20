# Tools and theory for modeling weight-at-size

Tools, analyses and background theory for modeling fish and crustacean weight-at-size from southern Gulf of Saint Lawrence data.

# Background

Weight-at-size equations are used to model the expected weights over some group of animals, such as a population in a region at a certain time of year.

Many factors influence an animal's weight at a given size. Major factors include sex, health, maturity, and the stage of its reproductive cycle. Some of these factors vary though time, both seasonally, interannually and regionally. 
This means that sampling location and season can have an influence on the resulting observations. In addition, there are factors which influence the *observed* weight and makes it deviate from the *true* weight. The following diagram provides an overall view of the causal factors influencing weight measurement data.

The green boxes indicate true size and true weight, which are the predictor and response variables of interest. The yellow boxes indicate the corresponding observed quantities as measured by rulers and scales. The blue boxes are factors known to influence the measurement of size and weight. The white boxes are factors that influence an individual's true weight and that may be known.

![image](https://github.com/TobieSurette/weight-at-size/assets/14942142/bd76d48c-7b92-4ed6-ba37-8818494bf190)

Weights obtained though application a weight-at-size equation to size-frequency data are generally more convenient than measuring each animal individually. These equations can be used to convert size-frequencies of a sampled population to equivalent weights. They can also be used to characterize different species or populations, or as controls to evaluate animal condition, since the weight of an animal also reflects some aspects of its health.

The form of the equation is the allometric relation $w = \alpha x^\beta$, where $w$ is the animal's weight, $\alpha$ is the scaling coefficient, $x$ is a measure of animal size and $\beta$ is the dimensional scaling coefficient.

# Snow crab 

The case for predicting indivdual weight for snow crab is interesting. Although the animal has a hard-shell which contrains a lot of the variability of factors observed in fish, there are important factors to consider. 

## Maturity 

The first factor to consider is that of **maturity**. Crab under a terminal moult at which crab undergo changes in shape. Among males, crab claws get larger and legs become longer. In other words, the volume of crab increases and we can expect the weight to increase accordingly.

## Time-since moult

The second factor to consider is the time that has elapsed since moulting. After moulting, the crab shell if softer and less thick. Within the shell, the muscle mass has yet to expand within the new shell, and it is filled with water. Thus, we expect that new-shelled crab would weight less than hard-shelled crab.

## Missing legs

The third factor, also ver important, is that snow crab comonly loose their limbs through predation and interaction or competition with other snow crab. Snow crab legs are very long (hence their other common name being _spider crab_) and thus each represent a notable proportion of the total body weight.

## Approach 

The snow crab weight-at-size equation is based on indirectly estimating the controbutions of each of the above effects on the average weights of snow crab in a sample. We make the following notes:
- The weights contributions of each leg at each of the five different positions needs to be esitmated. Some legs are much smaller than others, e.g. the fourth and fifth walking legs, and the frequency of their absence may be low as well. We would expect that the uncertainty associated with the controbuitions would be high.
- The inferrence opf the weight contribution of the central disc (body) wholly depends on being able to estimate the contributions of each of five pairs of walking legs.

**Dataset sample summary**: 

| Dataset  | year      | maturity | hardness | cheliped | second leg | third leg | fourth leg | fifth leg | sample size | 
| :------: | :------:  | :------: | :------: | :------: | :------:   | :------:  | :------:   | :------:  | :------:    |        
| PEI      | 1986-1987 | immature | soft     |        1 |          2 |         4 |          4 |         1 |          60 |
| PEI      | 1986-1987 | immature | hard     |       13 |          2 |         3 |          6 |         4 |          24 |
| PEI      | 1986-1987 | mature   | soft     |        3 |          5 |         2 |          3 |         3 |          76 |
| PEI      | 1986-1987 | mature   | hard     |       67 |         30 |        22 |         15 |        10 |         142 |
| ENS      | 1997-1998 | immature | soft     |       23 |         46 |        29 |         58 |        31 |         166 |
| ENS      | 1997-1998 | immature | hard     |       37 |         66 |        50 |         31 |        66 |         261 |
| ENS      | 1997-1998 | mature   | soft     |       23 |         42 |        28 |         24 |        26 |         329 |
| ENS      | 1997-1998 | mature   | hard     |       42 |         81 |        79 |         54 |        62 |         387 |
| RVS      | 2012-2017  | immature |          |    1889 |      3018 |       2796 |       2810 |      2941 |       17688 |
| RVS      | 2012-2017  | mature   |          |    1133 |      1693 |       1500 |       1385 |      1502 |        6339 |
| SCS      | 2013-2014 | immature | soft     |       10 |         44 |        42 |         39 |        31 |         816 |
| SCS      | 2013-2014 | immature | hard     |       15 |         24 |        25 |         22 |        19 |         322 |
| SCS      | 2013-2014 | mature   | soft     |      100 |        169 |       153 |        137 |       158 |        1664 |
| SCS      | 2013-2014 | mature   | hard     |      126 |        188 |       139 |        156 |       195 |        1440 |

- Note that for the SCS, the sample sizes for immature crab are much smaller, especially those with missing chelipeds and fifth walking legs.

# September multispecies survey data

The first issue is that the precision of weight measurements has varied over the survey. Precision of weight measurements depends on the balance scales being used at sea. For years prior to 2000, the precisions were generally about 5 grams. Formally this means that weight measurements were rounded to the nearest multiple of 5. From 2000 to 2010, the precision of weight values were generally about 2 grams, while weight values were about 1 gram from 2011 onward. 

There are additional factors adding noise to weight measurements:
- Rough sea conditions
- data transcription errors
- species misclassifications
- damaged or mangled animals
- presence of prey in stomach and regurgitation of stomach contents

The second issue is the occurence of outliers in the data. *Ad hoc* deletion of problem data works to a certain extent, especially when we have a large, precisely measured dataset, but we wish to have a general method that can deal with outliers without removing them.

We treat the outliers as arising from an extra error source, i.e. extra variation in the weight from some perturbation such as misreading the balance scale or a transcription error.

# Analytical approach

Formally we include in our model a second error distribution about the regression mean. The first error distribution models 

