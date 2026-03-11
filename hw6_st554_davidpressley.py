"""HW6_ST554_DavidPressley_Part2.py

Original file for Part I is located at
    https://colab.research.google.com/drive/1VTo5HAbc6HCZZShtl3lnGXV0eoe2T2zx

HW6 - Part II
David Pressley
Course: ST554
Due: March 11, 2026

**Submission:** Save to GitHub repo, submit repo link on Moodle

The purpose of this homework is to get a little more practice with SQL and gain some practice with creating classes.

**Instructions:**
- Include markdown text describing what you are doing, even when not explicitly asked for
- Add comments to .py file explaining work
- Save updates to GitHub as you work through it
- No edits after the due date
- check sharing settings
- use hw5 repo. Develop on main.
"""

"""## Part II - Messing with Classes

### Question 5
Create a class called `SLR_slope_simulator` that encapsulates the simulation of the sampling distribution of the slope estimator.

Recall we assume the following model for SLR:

Y_i = \beta_0 + \beta_1 x_i + E_i

where the E_i are assumed to be independent and identically distributed from a Normal distribution with mean 0 and variance sigma^2.

#### Class Definition

Define the `SLR_slope_simulator` class with:
- `__init__`: arguments `self`, `beta_0`, `beta_1`, `x`, `sigma`, and `seed`. Create initial attributes of `beta_0`, `beta_1`, `sigma`, `x`, `n`, `rng`, and `slopes` (an empty list).
- `generate_data`: generates one dataset (returning x and y)
- `fit_slope`: takes in x and y, fits SLR model, returns estimated slope
- `run_simulations`: takes number of simulations, uses `generate_data()` and `fit_slope()` in a loop, modifies `slopes` attribute
- `plot_sampling_distribution`: checks if slopes has length > 0, plots histogram if so
- `find_prob`: takes a `value` and `sided` argument ("above", "below", or "two-sided") and approximates the corresponding probability

# Python File for Part II
"""

#import  modules needed
import matplotlib.pyplot as plt
import numpy as np
from numpy.random import default_rng
from sklearn import linear_model

class SLR_slope_simulator:
    """
    Simulates the sampling distribution of the SLR slope estimator
    Parameters:
        - beta_0: true y-intercept of the SLR model
        - beta_1: true slope of the SLR model
        - x: numpy array of predictor values
        - sigma: standard deviation of the error terms (E_i)
        - seed: seed for the random number generator (for reproducibility)
    """

    # Initializer
    def __init__(self, beta_0, beta_1, x, sigma, seed):
        # create model parameters as attributes
        self.beta_0 = beta_0
        self.beta_1 = beta_1
        self.sigma = sigma
        self.x = x
        self.n = len(x)
        # create random number generator from seed
        self.rng = default_rng(seed)
        # initialize slope as empty list for slope estimator
        self.slopes = []

    #generate one dataset, return (x, y)
    def generate_data(self):
        """ Generate one dataset from the SLR model
            Return x and y arrays based on
        """
        y = self.beta_0 + self.beta_1*self.x + self.sigma*self.rng.standard_normal(self.n)
        return self.x, y

    def fit_slope(self, x, y):
        """ Fit an SLR model to x and y. Return fitted slope """
        # takes (x, y), fits SLR model, returns estimated slope
        reg = linear_model.LinearRegression()
        fit = reg.fit(x.reshape(-1,1), y)
        return fit.coef_[0]

    # takes number of simulations (n), calls generate_data() and fit_slope()
    # modifies slopes
    def run_simulations(self, num_sims):
        """ run num_sims simulations
            store estimated slopes

            uses generate_data() and fit_slopes() in a loop
            Replace slopes with array of slope estimates.
        """
        slope_list = []

        for i in range(num_sims):
            x, y = self.generate_data()
            slope = self.fit_slope(x,y)
            slope_list.append(slope)

        #convert to numpy array for later
        self.slopes = np.array(slope_list)


    # plot_sampling_distribution: checks if slopes has length > 0, plots histogram if so
    def plot_sampling_distribution(self):
        """ Plot Histogram of the simulated sample slopes """
        if len(self.slopes) == 0:
            print("run_simulations() must be called first")
            return

        plt.hist(self.slopes)
        plt.xlabel("Sample Slope")
        plt.ylabel("Frequency")
        plt.title("Sampling Distribution of Slope Estimator")

        plt.axvline(x=self.beta_1, color='red', linestyle='--', label=f"Slope (beta_1 = {self.beta_1})")
        plt.legend()
        plt.show()

    # takes a value and sided argument ("above", "below", or "two-sided")
    # and approximates the corresponding probability
    # if two-sided, check if value is above or below median of slopes to
    # determine which tail to use, and use 2x the one-sided probability to
    # get two-sided probability
    def find_prob(self, value, sided):
        """Approximate probability based on simulated slopes.

        sided options:
          "above" = P(slope > value)
          "below" = P(slope < value)
          "two-sided" = P(|slope| > |value|)

        Implementation note:
        1. For two-sided, we check if the value is above or below the median of the
        slopes to determine which tail to use. We then calculate the one-sided
        probability and multiply by 2 to get the two-sided probability.

        2. We use the mean of the boolean array (i.e. [T,F,T,T,F...] for (self.slopes >/< value)
        to get the proportion of slopes that satisfy the condition, the mean of which gives us
        the probability: (how many slopes are above/below the value) / (total number of slopes) = probability
        """
        if len(self.slopes) == 0:
            print("run_simulations() must be called first")
            return
        if sided == "above":
            return (self.slopes > value).mean()
        elif sided == "below":
            return (self.slopes < value).mean()
        elif sided == "two-sided":
            print(f"Median of slopes: {np.median(self.slopes)} ")
            if value >= np.median(self.slopes):
                return 2*(self.slopes > value).mean()
            else:
                return 2*(self.slopes < value).mean()
""" Testing the Class
Create an instance of the object with `beta_0 = 12`, `beta_1 = 2`,
`x = np.array(list(np.linspace(start = 0, stop = 10, num = 11))*3)`,
`sigma = 1`, and `seed = 10`.
"""
# Create the x values as a (33 row,0 column) numpy array. (three copies of 0-10 sequence, same as HW5)
x = np.array(list(np.linspace(start = 0, stop = 10, num = 11))*3)

# Instantiate the simulator
sim = SLR_slope_simulator(beta_0 = 12, beta_1 = 2, x = x, sigma = 1, seed = 10)
print(f"Simulation created with beta_0={sim.beta_0}, beta_1={sim.beta_1}, n={sim.n}")
type(sim)

""" Call `run_simulations()` before running simulations (should return error message)"""
sim.plot_sampling_distribution()

""" Run 10,000 simulations to approximate the sampling distribution of the slope"""
sim.run_simulations(10000)

""" Plot the sampling distribution"""
sim.plot_sampling_distribution()
print("Printing sampling distribution plot...")

""" Approximate the two-sided probability of being larger than 2.1"""
prob = sim.find_prob(2.1,"two-sided")
print(f"Two-sided probability of being larger than 2.1: {prob}")


""" Print out the simulated slopes"""
# print array of slopes from slopes attribute.
print(f"Array of slopes from slopes attribute: {sim.slopes}")
print(f"Number of slopes: {len(sim.slopes)}")