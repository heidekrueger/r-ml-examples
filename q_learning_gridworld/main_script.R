library(tidyverse)

# the following line will automatically set the correct working_directory
# NOTE: this only works in RStudio, not in other R clients!
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load the grid world environment
source('./environment.R')
# load helper functions for plotting
source('./plotting.R')
# load the Q_learning implementations
source('./Q_learning.R')


# change environment settings if desired
default_reward = -0.1
s_init = c(row=1, col=1)


# change hyperparameters if desired
epsilon = 0.2
step_size = 0.5 # good for vizualisation, may be too high in practice
discount_rate = 1


# set all Q values to 0 and plot
initialize_Q_values()
plot_Q()

# interative training for a few steps
training_loop(n_episodes = 2, max_steps = 20, interactive = T)


 # now let's train for 1000 episodes
training_loop(n_episodes = 1000, print=1)
plot_Q(step=total_steps_count)
