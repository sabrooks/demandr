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

#Function that returns a time line in the form of list of dr event milestones
event.timeline <- function(call, end, len.base = 2){

  milestones = list(event = call %--% end,
                 measure.period = (call+minutes(10)) %--% end,
                 baseline.period = (call - hours(len.base)) %--% call)
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

  baseline.start <- int_start(milestones$baseline.period)
  baseline.end <- int_end(milestones$baseline.period)

  df%>%
    ggplot(aes(x = Date.Time, y = energy))+
  geom_line()+
  annotate("rect", xmin = int_start(milestones$event), xmax = int_end(milestones$event),
             ymin = min(df$energy), ymax = max(df$energy),
             alpha = .1, fill = "blue")
    #geom_segment(aes(x = mdy_hm(paste(baseline.start)),
    #                xend = baseline.end,
    #                 y = baseline,
    #                 yend = baseline))
    #geom_segment(aes(x = int_start(milestones$measure.period),
    #                 xend = int_end(milestones$measure.period),
    #                 y = response,
    #                 yend = response))
}

example.event <- function (){
  times <- event.timeline ( mdy_hm("2/2/15 13:00") , mdy_hm("2/2/15 14:40"))
  base <- demandr::baseline(EventData, times$baseline.period)
  response <- demandr::event.max(EventData, times$measure.period)

  demandr::plot.event(EventData, times, base, response)
}
