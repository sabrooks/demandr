# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


hour.energy <- dr.data.raw%>%
  mutate(Date.Time = mdy_hms(Date.Time))%>%
  group_by(minute.origin = (origin%--%Date.Time)%/%minutes(1))%>% #create grouping by minute (replaces spreading)
  summarise(energy = sum(Load, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(Date.Time = origin + minutes(minute.origin))%>%
  select(Date.Time, energy)

#Function that returns a time line in the form of list of dr event milestones 
event.timeline <- function(call, end, len.base = 2){
  milestones = c(event = mdy_hm(call) %--% mdy_hm(end),
                 measure.period = (int_start(call)+minutes(10)) %--% int_end(event),
                 baseline.period = (int_start(event) - hours(len.base)) %--% int_start(event))
  return(milestones)
}

# Function for maximum energy during a DR event
#df is a dataframe with Date.Time and energy.
event.max <- function(df, event.interval){
  em <- df%>%
    filter(Date.Time %within% event.interval)%>%
    summarise(max = max(energy, na.rm = TRUE))
  return(em$max)
}

#Function to calculate a simple DR baseline (Minimum Value During Baseline period)
#df is a dataframe with Date.Time and energy.
baseline <- function(df, baseline.interval){
    bm <- df%>%
      filter(Date.Time %within% baseline.interval)%>%
      summarise(min = min(energy, na.rm = TRUE))
    return(bm$min)
}

#df is a dataframe with Date.Time and energy.
#milestones is the list from event.timeline
#baseline and response are the values of the baseline (min) and response (max)

plot.event <- function(df, milestones, baseline, response){
  df%>%
    ggplot(aes(x = Date.Time, y = energy))+
    geom_line()+
    annotate("rect", xmin = int_start(milestone$event), xmax = int_end(milestone$event),
             ymin = min(df$energy), ymax = max(df$energy),
             alpha = .1, fill = "blue")+
    geom_segment(aes(x = int_start(milestones$baseline.period),
                     xend = int_end(milestones$baseline.period),
                     y = baseline,
                     yend = baseline))+
    geom_segment(aes(x = int_start(milestones$measure.period),
                     xend = int_end(milestones$measure.period),
                     y = response,
                     yend = response))
}

