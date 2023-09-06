##########################################
## Style for PLOTLY
#########################################




plotlyBarH<-function(data_in = NULL, x = "Removals", y = "responsible",
                     title = "Number of Response Removals (by interviewer)",
                     ordered_y = TRUE,
                     x_title="HALLO",
                     key = "key"){
  if (!is.data.table(data_in)) data_in<-data.table(data_in)
  ###################################################################
  ## 1. Transform VARS
  ## 1.1. for ordered and postion
  y_ord<-ifelse(ordered_y, paste0("~reorder(", y, ",", x, ")"), paste0("~", y))
  x_pos<-paste0("~round(", x, "*0.85, 2)")

  ## 1.2. other
  x_val<-paste0("~round(", x, ", 2)")
  y_val<-paste0("~", y)
  ## 1.3. color palette
  getPalette<-colorRampPalette(brewer.pal(9, "Set1"))

  ## 2. PLOTLY (maybe add ggplot option later)
  p<-plot_ly(data = data_in,
             x = eval(parse(text=x_val)),
             y = eval(parse(text=y_ord)),
             type = 'bar',
             key = eval(parse(text=y_val)), source = title,
             orientation = 'h',
             marker = list(color = getPalette(nrow(data_in)))) %>%                       #list(color = rep('#009FDA', nrow(data_in)))) %>%
    add_annotations(xref = 'x', yref = 'y',
                    x = eval(parse(text=x_pos)), y = eval(parse(text=y_val)),
                    text = eval(parse(text=x_val)),  font = list(
                      size = 11,
                      color = "#FFFFFF"
                    ),
                    showarrow = FALSE) %>%
    layout(
      title = title,
      dragmode = "select",
      legend = list(
        font=list(
          size = 14,
          color = "#FFFFFF"
        ),
        x = 1,
        y = 0.5,
        opacity = 0.2,
        bgcolor= "#009FDA"
      ),
      yaxis = list(
        title = "",
        font = list(
          color = '#009FDA'
        )
      ),
      xaxis = list(
        title = x_title,
        font = list(
          color = '#009FDA'
        )
      )
    )
  return(p)
  ###################################################################
}

plotlyLine<-function(data_in = NULL, x = "QuestionnaireProgression", y = "Av_ResponseTime",
                     title = "Time over Questionnaire Progress",
                     x_title="Questionnair Progress",
                     y_title = "Average Response Time (in Sec.)",
                     x_lab_axis_txt = "var",
                     x_lab_axis_val = "x",
                     timeLine = NULL){
  ######  A. No timeline for slider ############
  if (is.null(timeLine)) {
    if (!is.data.table(data_in)) data_in<-data.table(data_in)
    ###################################################################
    ## 1. Transform VARS
    ## 1.1. for  postion
    x_pos<-paste0("~round(", x, "*0.85, 2)")

    #x_lab_axis_txt<-ifelse(is.null(x_lab_axis_txt), paste0("~", x), paste0("~", x_lab_axis_txt) )
    #x_lab_axis_val<-ifelse(is.null(x_lab_axis_val), paste0("~", x), paste0("~", x_lab_axis_val) )
    print(x_lab_axis_txt)
    lab_vals<-data_in[round(fivenum(1:.N))]
    ## 1.2. other
    x_val<-paste0("~round(", x, ", 2)")
    y_val<-paste0("~", y)
    x_lab_axis_val<-(lab_vals[[x_lab_axis_val]])
    x_lab_axis_txt_full<-(data_in[[x_lab_axis_txt]])
    x_lab_axis_txt<-(lab_vals[[x_lab_axis_txt]])
    ## 1.3. color palette
    getPalette<-colorRampPalette(brewer.pal(9, "Set1"))
    ## 2. PLOTLY (maybe add ggplot option later)

    p<-plot_ly(data = data_in,
               type = 'scatter',
               mode = 'lines',
               x = eval(parse(text=x_val)),
               y = eval(parse(text=y_val)),
               text = x_lab_axis_txt_full,
               hoverinfo = 'text',
               showlegend = F,
               line = list(color = getPalette(1))
    ) %>%
      layout(
        title = title,
        legend = list(
          font=list(
            size = 14,
            color = "#FFFFFF"
          ),
          x = 1,
          y = 0.5,
          opacity = 0.2,
          bgcolor= "#0d47a1"
        ),
        yaxis = list(
          title = y_title,
          font = list(
            color = '#0d47a1'
          )
        ),
        xaxis = list(
          title = x_title,
          tickmode = "array",
          tickvals = x_lab_axis_val,
          ticktext = x_lab_axis_txt,
          font = list(
            color = '#0d47a1'
          )
        )
      )
  } else if (is.character(timeLine)) {
    ##############  B. Timline for slider ##################
    ##    Attention input data has to be list
    if (!is.list(data_in)) return("File is not a list!")
    ##  1. prepare data
    steps <- list()
    p<-plot_ly(source = timeLine)
    aval<-data_in
    ## 2. color palette
    getPalette<-colorRampPalette(brewer.pal(9, "Set1"))
    for (i in 1:length(aval)) {
      p <- add_lines(p,
                     x=aval[i][[1]]$x,
                     y=aval[i][[1]]$y,
                     visible = aval[i][[1]]$visible,
                     name = aval[i][[1]]$name,
                     type = 'scatter',
                     mode = 'lines',
                     text = aval[i][[1]]$var,
                     hoverinfo = 'text',
                     key = aval[i][[1]]$var,
                     line=list(color=getPalette(i)),
                     showlegend = FALSE)
      step <- list(args = list('visible',
                               rep(FALSE, length(aval))),
                   method = 'restyle',
                   label = aval[i][[1]]$name,
                   bgcolor = "#0d47a1")
      step$args[[2]][i] = TRUE
      steps[[i]] = step
    }



    # add slider control to plot
    p <- p %>%
      layout(sliders = list(list(active = 0,
                                 bgcolor = "#0d47a1",
                                 font = list(color = "#0d47a1"),
                                 currentvalue = list(prefix = "Week: "),
                                 steps = steps)),
             updatemenus = list(list(active = 0,
                                    type= 'buttons',
                                    bgcolor = I("#0d47a1"),
                                    buttons = list(
                                      list(
                                        font = list(color = "#FFFFFF"),
                                        label = "Show all weeks",
                                        method = "update",
                                        args = list(list(visible = c(TRUE, TRUE)),
                                                    list(annotations = list(c(), c()))
                                                    )
                                        )
                                      )
                                    )
                               ),
             dragmode = "select"
             )


  }
  return(p)
  ###################################################################
}



























