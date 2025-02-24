# Reproducible research: version control and R


Questions 1,2, and 3:
https://github.com/GeorgeOxford/logistic_growth

Question 4: 
What do you observe?

Two plots are formed showing a grid with coordinates on the axis. Random paths are formed with the number of steps (distance) shown by the colour of the path. The direction of the path changes randomly with each step and continues to 500 steps. There are no limits to the coordinates but the path is limited by a distance of 0.25 per step.

What is a random seed?

A random seed is the start number for a random sequence of numbers to be generated using a random number generator. Its a way of making random sequences reproducible.

Brownian path code changes:
![Alt text](Code_changes_brownian.png) 

Question 5:

The dataset has 13 columns and 33 rows.

The data is highly skewed and so a log tranformation to both the genome length and the virion volume is appropriate. 

The exponent (alpha) is 1.5152, p-value = 6.44e-10, and the scaling factor (beta) is 1181.807, p-value = 2.28e-10. These are both statistically significant at an apha of 0.001. The values obtained are the same as those obtained in the 2014, Cui, Schlub and Holmes article.

The code for the figure can be found in question-5-data/Plot_code.R.

The estimated volume of a 300kb virus is 6697006nm^3. 

Bonus: the difference between reproducability and replicability

Reproducability refers to the ability to produce consistent results when applying the same analysis to the same data, whilst replicability is when similar results can be obtained when performing the same analysis but with different data collected under similar conditions. 

Git is a version control system that allows developers to collaborate on a project by sharing files/code whilst also tracking changes that are made during development. Github is an online platform that provides a cloud to store git repositories and can be accessed by anyone given permission. 
Git and Github are useful to enhance the reproducibility and replicability of data because they make it easier to share and save analysis code and data between researchers, and for future use. This means that when researchers are researching similar subjects or collaborating there will be less human error or inconsistency in their results. 








## Instructions

The homework for this Computer skills practical is divided into 5 questions for a total of 100 points (plus an optional bonus question worth 10 extra points). First, fork this repo and make sure your fork is made **Public** for marking. Answers should be added to the # INSERT ANSWERS HERE # section above in the **README.md** file of your forked repository.

Questions 1, 2 and 3 should be answered in the **README.md** file of the `logistic_growth` repo that you forked during the practical. To answer those questions here, simply include a link to your logistic_growth repo.

**Submission**: Please submit a single **PDF** file with your candidate number (and no other identifying information), and a link to your fork of the `reproducible-research_homework` repo with the completed answers. All answers should be on the `main` branch.

## Assignment questions 

1) (**10 points**) Annotate the **README.md** file in your `logistic_growth` repo with more detailed information about the analysis. Add a section on the results and include the estimates for $N_0$, $r$ and $K$ (mention which *.csv file you used).
   
2) (**10 points**) Use your estimates of $N_0$ and $r$ to calculate the population size at $t$ = 4980 min, assuming that the population grows exponentially. How does it compare to the population size predicted under logistic growth? 

3) (**20 points**) Add an R script to your repository that makes a graph comparing the exponential and logistic growth curves (using the same parameter estimates you found). Upload this graph to your repo and include it in the **README.md** file so it can be viewed in the repo homepage.
   
4) (**30 points**) Sometimes we are interested in modelling a process that involves randomness. A good example is Brownian motion. We will explore how to simulate a random process in a way that it is reproducible:

   - A script for simulating a random_walk is provided in the `question-4-code` folder of this repo. Execute the code to produce the paths of two random walks. What do you observe? (10 points)
   - Investigate the term **random seeds**. What is a random seed and how does it work? (5 points)
   - Edit the script to make a reproducible simulation of Brownian motion. Commit the file and push it to your forked `reproducible-research_homework` repo. (10 points)
   - Go to your commit history and click on the latest commit. Show the edit you made to the code in the comparison view (add this image to the **README.md** of the fork). (5 points)

5) (**30 points**) In 2014, Cui, Schlub and Holmes published an article in the *Journal of Virology* (doi: https://doi.org/10.1128/jvi.00362-14) showing that the size of viral particles, more specifically their volume, could be predicted from their genome size (length). They found that this relationship can be modelled using an allometric equation of the form **$`V = \beta L^{\alpha}`$**, where $`V`$ is the virion volume in nm<sup>3</sup> and $`L`$ is the genome length in nucleotides.

   - Import the data for double-stranded DNA (dsDNA) viruses taken from the Supplementary Materials of the original paper into Posit Cloud (the csv file is in the `question-5-data` folder). How many rows and columns does the table have? (3 points)
   - What transformation can you use to fit a linear model to the data? Apply the transformation. (3 points)
   - Find the exponent ($\alpha$) and scaling factor ($\beta$) of the allometric law for dsDNA viruses and write the p-values from the model you obtained, are they statistically significant? Compare the values you found to those shown in **Table 2** of the paper, did you find the same values? (10 points)
   - Write the code to reproduce the figure shown below. (10 points)

  <p align="center">
     <img src="https://github.com/josegabrielnb/reproducible-research_homework/blob/main/question-5-data/allometric_scaling.png" width="600" height="500">
  </p>

  - What is the estimated volume of a 300 kb dsDNA virus? (4 points)

**Bonus** (**10 points**) Explain the difference between reproducibility and replicability in scientific research. How can git and GitHub be used to enhance the reproducibility and replicability of your work? what limitations do they have? (e.g. check the platform [protocols.io](https://www.protocols.io/)).
