library(tidyverse)

# hyperparameters
epsilon = 0.3
step_size = 0.5
discount_rate = 1


## set up initial Q-values (all 0)
initialize_Q_values <- function(){
  qa = c(up=0,down=0,left=0,right=0) # one action
  qr =  list(col_1 = qa, col_2 = qa, col_3 = qa, col_4 = qa) # one row of states
  # qvals will be a global variable
  qvals <<- list(row_1 = qr, row_2 = qr, row_3 = qr) # all states
  
  # reset counters
  total_episode_count <<- 0
  total_steps_count <<- 0
  
}


display_prompt <- function(){
  input = readline(prompt="Press [enter] to continue, [q] to abort.")
  if (input=="q"){
    stop("Terminated by user.")
  }
}

### Helper functions to retrieve Q-values more easily

Qs <- function(s){
  # helper function: returns all Q values of a given state s
  qvals[[s["row"]]][[s["col"]]]
}

Q <- function(s,a){
  Qs(s)[a] %>% unname()
}


choose_action <- function(s, print=F, explore=T){
  # if explore=T, choose epsilon-greedy action:
  # greedy with prob (1-eps), random with prob (eps)
  # if explore=F, always choose a greedy action. 
  if(terminal_value(s)){
    # in terminal state, we cannot choose any action. Return a pseudo q of 0
    return(list(action="terminal", q_value=0))
  }
  
  if (print){
    print(paste0("======================="))
    print(paste0("Choosing an action..."))
  }
  

  if (explore & runif(1) < epsilon){
    # choose a random action with probability epsilon
    if (print){
      print("Exploring: Chose a random action.")
    }
    action = sample(all_actions, 1)
  } else {
    # otherwise choose a Q-maximizing action 
    
    qs = Qs(s)
    action = qs[qs==max(qs)] %>%
      # tie braking in case of multiple argmax actions
      sample(1) %>% 
      names()
    
    if (print){
      print("Exploiting, choosing a Q-maximizing action.")
    }
    
  }
  
  q_value = Q(s,action)
  
  if(print){
    print(paste0("a: ", action, ", Q(s,a): ", q_value))
  }
  
  list(action=action, q_value = q_value)
  
}


training_loop <- function(n_episodes=1000, max_steps=Inf, print=0, interactive = F){
  
  s= s_init
  run_episode_count = 0
  run_step_count = 0
  
  if (interactive){
    print = 3
  }
  
  
  while(run_episode_count < n_episodes & run_step_count < max_steps){
    # start a new episode
    total_episode_count <<- total_episode_count + 1
    run_episode_count = run_episode_count + 1
    
    if (print){
      print("=====================")
      print(paste0("Starting Episode ", total_episode_count, "."))
    }
    
    s = s_init
    episode_step = 0
    episode_return = 0
    done = F
    
    if (interactive){
      canvas = plot_background()
      q_canvas = plot_Q(total_steps_count, canvas)
      s_canvas = plot_state(s, total_steps_count,q_canvas)
      print(s_canvas)
      display_prompt()
    }
    
    
    while (!done & run_step_count < max_steps){
      episode_step = episode_step+1
      total_steps_count <<- total_steps_count + 1
      
      #### choose an action
      a = choose_action(s, print = print>1)$action
      
      if (interactive){
        sa_canvas = plot_state_action(s,a,total_step_count,s_canvas)
        print(sa_canvas)
        display_prompt()
        
      }
      
      ### perform one step in the environment, and observe (s,a,r,s')
      obs = step(s,a, episode_step, print= print>1)
      s_new = obs$new_state
      r = obs$reward
      done = obs$terminal
      
      if (interactive){
        print(plot_transition(s,a,r,s_new, step=total_steps_count, sacanvas = sa_canvas))
        display_prompt()
        
      }
      
      ##### update the Q-value
      # calculate the TD1-lookahead q-value for the bootstrap
      lookahead_q = choose_action(s_new, explore=F)$q_value
      q_old = Q(s,a)
      q_new = (1-step_size) * q_old + step_size * (r + discount_rate * lookahead_q)
      qvals[[s["row"]]][[s["col"]]][a] <<- q_new
      
      
      
      s_old = s
      episode_return = episode_return + r
      s = s_new
      
      if (interactive){
        q_canvas = plot_Q(step=total_steps_count, canvas=canvas)
        s_canvas = plot_state(s, total_steps_count, q_canvas)
        if (done){
          print(s_canvas)
          display_prompt()
        }  

        
      }
      
      if (print>1){
        print("============")
        print(paste0("Q-learning: Episode ", total_episode_count, " step ", episode_step, " (", total_steps_count, " total steps."))
        print(paste0("Old state: (", s_old["row"],",",s_old["col"], "). action: ", a, "."))
        print(paste0("Old Q(s,a): ", q_new, ". New Q(s,a): ", q_new))
        print(paste0("New state: (", s["row"], ",", s["col"], ")"  ))
      }
    }
    if (print>0){
      print(paste0("Episode ", total_episode_count, " ended. Return:", round(episode_return,digits=3), "."))
    }
  }
  print(paste0("Completed. Total episodes:",  total_episode_count, ". Total steps: ", total_steps_count))
}
