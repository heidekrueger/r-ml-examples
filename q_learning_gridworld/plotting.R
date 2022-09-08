
library(tidyverse)

### assumes that environment.R has been sourced

state_helper = expand_grid(state_row=all_state_rows,
                           state_col=all_state_cols) %>% 
  mutate(color = 'white')
state_helper[state_helper$state_row ==2 &
               state_helper$state_col == 2,]$color = 'gray'
terminal_states = tibble(state_row = c(3,2),
                         state_col = 4,
                         action = "terminal",
                         q=c(+1, -1)
)

segments = tibble(
  x =      c(0.5, 1.5, 2.5, 3.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1.5, 2.5, 3.5),
  y =      c(0.5, 0.5, 0.5, 0.5, 1.5, 2.5, 3.5, 2.5, 1.5, 3.5, 3.5, 1.5),
  xend =   c(3.5, 3.5, 3.5, 4.5, 2.5, 1.5, 3.5, 2.5, 1.5, 4.5, 3.5, 3.5),
  yend =   c(3.5, 2.5, 1.5, 1.5, 3.5, 3.5, 0.5, 0.5, 0.5, 0.5, 2.5, 0.5)
)


q_df <- function(qvals){
  # turns qvals array into a df suitable for plotting
  tibble(row=qvals) %>% 
    mutate(state_row = as.numeric(str_remove(names(row), "row_"))) %>% 
    unnest_longer(col=row) %>% 
    transmute(
      state_row,
      state_col = as.numeric(str_remove(row_id, "col_")),
      q=row) %>% 
    unnest_longer(col=q) %>% 
    transmute(state_row, state_col, action=q_id, q) %>% 
    filter(!(state_row %in% c(2,3) & state_col == 4)) %>% 
    filter(!(state_row == 2 & state_col == 2))
  
}

plot_background <- function(){
  df = q_df(qvals)
  p = ggplot(df, aes(x=state_col, y=state_row))
  
  p = p + theme_minimal() #+ scale_y_continuous(limits=rev)
  p = p + geom_tile(data=state_helper,
                    aes(x=state_col, y=state_row),
                    width=1,height=1,
                    col = 'gray', size = 1,
                    fill=state_helper$color)
  p = p + geom_segment(data=segments,
                       aes(x=x, y=y, xend=xend, yend=yend),
                       col = 'gray')
  
  p = p + geom_label(data = terminal_states,
                     aes(x=state_col, y=state_row, label=round(q,digits=3), fill = q)
  )
  p = p +  scale_fill_distiller(type='div', palette = "RdYlBu", direction = 1)
  p = p + labs(x = "State x", y = "State y", fill = "Q(s,a)")
  p
}


plot_Q = function(step = 0, canvas = NULL){
  # takes a tibble of q values and plots them
  
  if (is_null(canvas)){
    canvas = plot_background()
  }
  
  df = q_df(qvals)
  p = canvas + geom_label(data = df %>% filter(action == 'left'),
                          aes(x=state_col, y=state_row, label = round(q,digits=3), fill = q),
                          nudge_x = -0.25)
  p = p + geom_label(data = df %>% filter(action == 'right'),
                     aes(x=state_col, y=state_row, label =round(q,digits=3), fill = q),
                     nudge_x = 0.25)
  p = p + geom_label(data = df %>% filter(action == 'up'),
                     aes(x=state_col, y=state_row, label = round(q,digits=3), fill = q),
                     nudge_y = 0.25)
  p = p + geom_label(data = df %>% filter(action == 'down'),
                     aes(x=state_col, y=state_row, label = round(q,digits=3), fill = q),
                     nudge_y = -0.25)
  
  p = p + ggtitle(paste0("Total steps: ", step))
  
  
  p
}


plot_state <- function(s, step=0, qcanvas=NULL){
  if (is_null(qcanvas)){
    qcanvas = plot_Q(step=step)
  }
  
  qcanvas + geom_point(size=5, x=s["col"], y=s["row"], col='red')
}


plot_state_action <- function(s,a, step=0, scanvas=NULL){
  
  if (is_null(scanvas)){
    scanvas = plot_state(s, step=step)
  }
  
  intended_goal = if (a == "up") {move_up(s) 
  } else if (a == "left") { move_left(s)
  } else if (a == "right") {move_right(s)
  } else {move_down(s)}
  
  g = scanvas + geom_segment(x=s["col"], y=s["row"],
                             xend = (s["col"] + intended_goal["col"])/2,
                             yend = (s["row"] + intended_goal["row"])/2,
                             col = "red",
                             size = 3,
                             arrow = arrow(angle=60)
  )
  g
}

plot_transition <- function(s, a, r, s_new, step=0, sacanvas=NULL){
  
  
  if (is_null(sacanvas)){
    sacanvas = plot_state_action(s,a,step=step)
  }
  
  g = sacanvas +
    #geom_point(x=s_new["col"], y=s_new["row"],col='blue', size = 4) +
    geom_segment(x=s["col"], y=s["row"],
                 xend = s_new["col"], yend = (s_new["row"]),
                 col = "blue",
                 size = 1,
                 arrow = arrow(angle=30, type = "closed")
    ) +
    geom_label(x=0.4*s["col"]+0.6*s_new["col"],
               y=0.4*s["row"]+0.6*s_new["row"],
               label = paste0("r=", round(r,digits=2)),
               col = "blue")
  
  g
  
}
