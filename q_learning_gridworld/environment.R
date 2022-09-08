library(tidyverse)

env_loaded = T # set a flag when this file has been sourced

all_state_rows = c(1,2,3)
all_state_cols = c(1,2,3,4)
all_actions = c("up", "down", "left", "right")


default_reward = -0.03
s_init = c(row=1, col=1)


oob <- function(state){
  # Helper function: checks whether a state is out of bounds
  row = state["row"]
  col = state["col"]
  
  result = row < 1 | col < 1 | row > 3 | col > 4 | (row==2 & col==2)
  unname(result)
}

terminal_value <- function(state){
  # returns the terminal value of state or 0 if state is not terminal
  row = state["row"]
  col = state["col"]
  case_when(
    row == 2 & col ==4 ~ -1,
    row == 3 & col ==4 ~ 1,
    TRUE ~ 0)
}

move_up <- function(state){
  c(state["row"]+1, state["col"])
}
move_down <- function(state){
  c(state["row"]-1, state["col"])
}
move_left <- function(state){
  c(state["row"], state["col"]-1)
}
move_right <- function(state){
  c(state["row"], state["col"]+1)
}

step <- function(state, action, episode_step = 0, print=F){
  
  candidates = case_when(
    action == "up" ~ list(straight=move_up(state),  turn_l=move_left(state),  turn_r=move_right(state)),
    action == "left" ~ list(straight=move_left(state),  turn_l=move_down(state),  turn_r=move_up(state)),
    action == "right" ~ list(straight=move_right(state),  turn_l=move_up(state),  turn_r=move_down(state)),
    action == "down" ~ list(straight=move_down(state),  turn_l=move_right(state),  turn_r=move_left(state)),
    TRUE ~ list(straight=NULL, turn_l = NULL, turn_r=NULL)
  )
  # bug in case_when may mess up names depending on your version of R
  names(candidates) = c("straight", "turn_l", "turn_r")
  # get transition probabilities
  probs = c(straight=0.8, turn_l=0.1, turn_r=0.1)
  # if a move would be out of bounds, set it's probability weight to 0
  for (i in 1:3){
    if (oob(candidates[[i]])){
      probs[i] = 0.0
    }
  }
  # draw a direction according to the candidate probabilities
  direction = sample(names(probs), 1, prob=probs)
  new_state = candidates[[direction]]
  
  tv = terminal_value(new_state)
  
  if (tv){
    reward = tv
    terminal = T
  } else {
    reward = default_reward
    terminal = F
  }
  
  
  if (print){
    print("==============================")
    print("Performing a state transition.")
    print(paste0("Episode step: ", episode_step))
    print(paste0("Old state: (", state["row"], ",", state["col"], ")"))
    print(paste0("Chosen Action: ", action))
    print(paste0("Random direction (straight/turn left/turn right): ", direction))
    print(paste0("New state: (", new_state["row"],",",new_state["col"], ")"))
    print(paste0("Reward: ", reward))
  }
  
  list(new_state=new_state, reward=reward, terminal=terminal)
}