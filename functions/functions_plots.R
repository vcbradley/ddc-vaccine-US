
### helper function for plot 3
plot_with_errorbands = function(data, outcome
                                , ylab = NULL
                                , use_ribbons = c('5pct','10pct')
                                , include_legend = F
                                , title = NULL
                                , xlim_val = NULL
                                , ylim_val = NULL){
  plt = ggplot(data = data
               , aes(x = as.Date(end_date))) +
    lemon::geom_pointline(aes(y = get(paste0(outcome, '_no error')), color = study_name)) +
    theme_pubr() +
    scale_color_manual(values = scale_values) +
    scale_fill_manual(values = scale_values, guide = 'none') +
    labs(x = NULL, color = 'Study') +
    theme(axis.title = element_text(size = 8),
          axis.text = element_text(size = 8),
          plot.margin = unit(rep(0, 4), "lines"))

  if(!include_legend){
    plt = plt + theme(legend.position = 'none')
  }

  if(!is.null(use_ribbons)){
    for(r in use_ribbons){
      #hotfix to make ggplot evaluate each loop
      plot_loop_data = data %>% select(end_date
                                       , study_name
                                       , lower = paste0(outcome, '_plus', r)
                                       , upper = paste0(outcome, '_less', r))

      plt <- plt + geom_ribbon(data = plot_loop_data,
                               aes(ymin = lower, ymax = upper, fill = study_name), alpha=0.3)

    }
  }

  if(!is.null(title)){
    plt = plt + ggtitle(title)
  }

  if(!is.null(ylab)){
    plt = plt + ylab(ylab)
  }

  if(!is.null(xlim_val)){
    plt = plt + xlim(xlim_val)
  }
  if(!is.null(ylim_val)){
    plt = plt + ylim(ylim_val)
  }

  return(plt)
}




#### helper function for Fig 1
# makes all the inner plots
plot_comparisons = function(data, outcome, type = 'est', x = 'hp', y = 'fb', labels = NULL, title = NULL, annotate_df = NULL){

  if (type == 'est') {
    x_name = if(x == 'hp') 'household_pulse' else if(x == 'fb') 'facebook' else 'pop'
    x_est = paste0('pct_',outcome,'_',x_name)
    y_name = if(y == 'hp') 'household_pulse' else if(y == 'fb') 'facebook' else 'pop'
    y_est = paste0('pct_',outcome,'_',y_name)
    mean_diff = data %>% summarize(mean_diff = mean(get(y_est) - get(x_est), na.rm = T)) %>% pull()

    data_min = data %>% select(x_est, y_est) %>% min()
    data_max = data %>% select(x_est, y_est) %>% max()

    if(x == 'pop' & type == 'est'){
      plot_min = 0.3
      plot_max = 0.75
    }else{
      plot_min = data_min - 0.02*(data_max - data_min)
      plot_max = data_max + 0.02*(data_max - data_min)
    }



    cat(paste0('average difference:\n', y_name,ifelse(mean_diff > 0, '+', ''), round(mean_diff*100, 2), 'pp\nx = ', 0.75 * plot_max,'y = ', 0.1 * plot_min, '\n'))

    plt = ggplot(data) +
      geom_point(aes(x = get(x_est), y = get(y_est)), color = 'gray40') +
      geom_abline(slope = 1, lty = 2) +
      #geom_abline(slope = 1, intercept = mean_diff, color = 'blue') +
      theme_pubr() +
      #geom_hline(yintercept = 0.25, lty = 2, color = 'gray') +
      #geom_vline(xintercept = 0.25, lty = 2, color = 'gray') +
      # annotate('text', x = plot_min + 0.65 * (plot_max - plot_min)
      #          , y = plot_min + 0.1 * (plot_max - plot_min)
      #          , label = paste0('average difference:\n', ifelse(!is.null(labels), labels[[y]], y_name),ifelse(mean_diff > 0, '+', ''), round(mean_diff*100, 2), 'pp'), color = 'gray50', size = 2.9) +
      # # annotate('text', x = 0.2, y = 0.07, label = 'herd immunity\nthreshold', color = 'gray50') +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L), expand = c(0,0), limits = c(plot_min, plot_max)) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1L), expand = c(0,0), limits = c(plot_min, plot_max)) +
      coord_equal()

    data_avg = data %>% summarize(x_mean = mean(get(x_est)), y_mean = mean(get(y_est)))
    plt = plt +
      geom_point(data = data_avg, aes(x = x_mean, y = y_mean, color = 'State average'), color = 'red') +
      geom_text_repel(data = data_avg
                      , aes(x = x_mean, y = y_mean
                            , label = paste0('Average:\n(',round(x_mean * 100), '%, ', round(y_mean * 100), '%)'))
                      , segment.color = 'red'
                      , nudge_x = 0.5 * (plot_max - plot_min), nudge_y = -0.5 * (plot_max - plot_min)
                      , segment.alpha = 0.7
                      , segment.size = 0.3
      )



  }

  if (type != "est") {

    x_est = paste0(x,'_rank_', outcome)
    y_est = paste0(y,'_rank_', outcome)

    plot_min = 0
    plot_max = 53

    plt = ggplot(data) +
      geom_point(aes(x = get(x_est), y = get(y_est)), color = 'gray40') +
      #geom_text(data = annotate_df, aes(x= get(paste0(x,'_rank_', outcome)), y = get(paste0(y,'_rank_', outcome)) + 2, label = pop), cex = 3.5) +
      geom_abline(slope = 1, lty = 2) +
      theme_pubr() + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) +
      annotate('text', x = 15, y = 2, label = 'highest', color = 'grey50') +
      annotate('text', x = 36, y = 50, label = 'lowest', color = 'grey50') +
      geom_hline(yintercept = 51/3, lty = 3, color = 'gray') +
      geom_hline(yintercept = 2*51/3, lty = 3, color = 'gray') +
      geom_vline(xintercept = 51/3, lty = 3, color = 'gray') +
      geom_vline(xintercept = 2*51/3, lty = 3, color = 'gray') +
      scale_x_reverse(expand = c(0, 0), limits = c(plot_max,plot_min)) +
      scale_y_reverse(expand = c(0, 0), limits = c(plot_max,plot_min)) +
      coord_equal()

  }

  if(!is.null(labels)){
    plt = plt + labs(x = labels[[x]], y = labels[[y]])
  }


  if(!is.null(title)) {
    plt = plt + ggtitle(title)
  }

  if(!is.null(annotate_df)) {
    plt = plt +
      geom_text_repel(data = annotate_df
                      , aes(x = get(x_est), y = get(y_est), label = pop)
                      , segment.color = 'grey50'
      )
  }

  return(plt)
}



### function for making fig to compare state-level polls to CDC and each other
makeCompPlot <- function(df, show_states, labels){

  # subset df for annotation based on jsut the states we want to show
  annotate_df <- df %>% filter(pop %in% show_states)

  combos_top = list(
    list(x = 'hp', y = 'fb', outcome = 'hesitant', type = 'est')
    , list(x = 'hp', y = 'fb', outcome = 'willing', type = 'est')
    , list(x = 'hp', y = 'fb', outcome = 'vaccinated', type = 'est')
    , list(x = 'hp', y = 'fb', outcome = 'hesitant', type = 'rank', annotate_df = annotate_df)
    , list(x = 'hp', y = 'fb', outcome = 'willing', type = 'rank', annotate_df = annotate_df)
    , list(x = 'hp', y = 'fb', outcome = 'vaccinated', type = 'rank', annotate_df = annotate_df)
  )

  combos_bottom = list(
    list(x = 'pop', y = 'fb', outcome = 'vaccinated', type = 'est')
    , list(x = 'pop', y = 'hp', outcome = 'vaccinated', type = 'est')
    , list(x = 'pop', y = 'fb', outcome = 'vaccinated', type = 'rank', annotate_df = annotate_df)
    , list(x = 'pop', y = 'hp', outcome = 'vaccinated', type = 'rank', annotate_df = annotate_df)
  )


  ##### Build plot in two stages
  # 1) top - comparison b/w surveys
  # 2) bottom - comparison w / CDC


  ##### row and col titles used in both
  # make column titles
  col_titles = lapply(c('', 'Hesitant', 'Willing', 'Vaccinated'), function(t){
    as_ggplot(text_grob(t,size = 14))+ theme(plot.margin = margin(0,0.25,0,0, "cm"))
  })

  # make row titles
  row_titles = lapply(c('Estimated %', 'Rank'), function(t){
    as_ggplot(text_grob(t,size = 14, rot = 90))+ theme(plot.margin = margin(0,0.25,0,0, "cm"))
  })


  ### Part 1 - top
  # make list of all inner plots
  plotlist_top <- lapply(combos_top, function(c){
    plot_comparisons(data = df
                     , outcome = c$outcome
                     , x = c$x
                     , y = c$y
                     , type = c$type
                     , labels = labels
                     , annotate_df = c$annotate_df)
  })

  # combine and add panel names
  inner_plots_top = ggarrange(plotlist = plotlist_top
                              , ncol = 3, nrow = 2
                              , labels = c('A', 'B','C','D','E','F'))

  # make full top plot
  full_top = wrap_plots(
    wrap_plots(col_titles, nrow = 1, widths = c(1,3,3,3))
    , wrap_plots(wrap_plots(row_titles, ncol = 1)
                 , inner_plots_top
                 , widths = c(0.5, 9)
                 , nrow = 1)
    , heights = c(0.5, 6)
  )


  #### Part 2 - bottom
  #make a list of all inner plots
  plotlist_bottom <- lapply(combos_bottom, function(c){
    plot_comparisons(data = df
                     , outcome = c$outcome
                     , x = c$x
                     , y = c$y
                     , type = c$type
                     , labels = labels
                     , annotate_df = c$annotate_df)
  })

  # combine and add panel names
  inner_plots_bottom = ggarrange(plotlist = plotlist_bottom
                                 , ncol = 2, nrow = 2
                                 , labels = c('G','H','I','J'))
  full_bottom = wrap_plots(wrap_plots(row_titles, ncol = 1)
                           , inner_plots_bottom
                           , widths = c(0.5, 8)
                           , nrow = 1)
  # add title
  full_bottom = wrap_plots(as_ggplot(text_grob('Comparison with CDC Vaccine Uptake',size = 18)) + theme(plot.margin = margin(0,0.25,0,0, "cm"))
                           , full_bottom
                           , ncol = 1
                           , heights = c(1,6))

  # add padding
  full_bottom = wrap_plots(
    as_ggplot(text_grob('',size = 18)) + theme(plot.margin = margin(0,0,0,0, "cm"))
    , full_bottom
    , as_ggplot(text_grob('',size = 18)) + theme(plot.margin = margin(0,0,0,0, "cm"))
    , nrow = 1
    , widths = c(1.5,8,1.5)
  )

  #### Combine for full plot
  grid_plot = wrap_plots(wrap_plots(as_ggplot(text_grob('Comparison between surveys',size = 18)) + theme(plot.margin = margin(0,0.25,0,0, "cm"))
                                    , full_top, ncol = 1, heights = c(0.75,9))
                         , full_bottom
                         , ncol = 1
                         , heights = c(10,11)
  )


  return(grid_plot)
}
