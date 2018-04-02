######################################################################################
#
# THIS SHINY APP PRODUCES A LINE PLOT OF ANTI-COAGULANT PATTERN BY INDIVIDUAL PATIENTS.
# THE AC PATTERN IS DEFINED TO BE ALL AC PRESCRIPTIONS AFTER INDEX VTE DATE.
#
######################################################################################

library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(scales)
library(lemon)
options(warn = -1)


patinfo <- readRDS("working_data_added.rds") 
inr_info <- readRDS("working_data_inr_added.rds")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  # Select the patid to be plotted
  output$select_patid <- renderUI({
    items <- levels(factor(unique(patinfo$patid)))
    names(items) <- items
    selectInput('pat', 'Select one or multiple patient id to be plotted', items, multiple = T, selected = items[1:3])
  })
  
  # Get the number of selected patients
  # n_pat <- length(input$pat)
  n_pat <- reactive({
    length(input$pat)
  })
  
  # CREATE A SUB DATA SET BY INTERACTIVE INPUT OF PATID
  selected <- reactive({
    selected_patid <- input$pat
    if (is.null(selected_patid)) return()
    d <- as.data.frame(patinfo %>% filter(patid %in% selected_patid) %>%
      mutate(category = factor(category),
             patid = factor(patid))  )
    d
  })
  
  # DISPLAY THE DATA FOR SELECTED PATIENTS
  output$select_patid_disp <- renderDataTable({
    selected()
  })
  
  
  # CREATE A SUB DATA SET ON INR INFORMATION FOR SELECTED PATIENTS
  selected_inr <- reactive({
    selected_patid <- input$pat
    if (is.null(selected_patid)) return()
    d <- as.data.frame(inr_info %>% filter(patid %in% selected_patid))
    d
  })
  
  # DISPLAY THE DATA FOR SELECTED PATIENTS
  output$select_inr_disp <- renderDataTable({
    selected_inr()
  })
  
  # CREATE LINE PLOT OF AC PATTERN
  output$patternPlot <- renderPlotly({
    if (is.null(selected())) return()
    # use the key aesthetic/argument to help uniquely identify selected observations
    p <- ggplot(selected(), aes(x=fst_dt, y=category_num,
                 text=paste(" Fill Date: ", fst_dt, "<br>",
                            "Copay: ", Copay_sum, "<br>",
                            "Days of Supply: ", Days_Sup, "<br>",
                            "Index Cancer Date: ", index_cancer_dt, "<br>",
                            "Index VTE Date: ", index_dt))) +
    geom_vline(aes(xintercept = as.numeric(index_dt), linetype="Index VTE Date")) + 
    geom_vline(aes(xintercept = as.numeric(index_dt+90), linetype="Index VTE Date + 90 Days")) + 
    geom_point(aes(x=fst_dt, y=id, colour=factor(category))) +
    geom_segment(aes(x=fst_dt, xend=fst_sup, y=id, yend=id,
                      colour=factor(category)), size=0.5) + #, show.legend=F
    geom_point(data = selected_inr(),
               aes(x=inr_dt, y=inr_num, colour="INR Test",
                   text = paste(" INR Date: ", inr_dt, "<br>",
                                "Result Number: ", Rslt_Nbr)),
               shape=3, alpha = 0.8, inherit.aes = FALSE) +
    guides(colour = guide_legend(), linetype = guide_legend()) +
    scale_colour_manual("", values = c("DOACS" = "#009E73",
                                   # "Index VTE Date" = "black",
                                   "INR Test" = "red",
                                   "LMWH" = "#E69F00",
                                   "Other" = "purple",
                                   "Warfarin" = "#56B4E9"),
                        guide = 'legend') +
    scale_linetype_manual("", values=c("Index VTE Date + 90 Days"='dashed',
                                       "Index VTE Date"='solid')) +
    guides(linetype=guide_legend(keywidth = 3, keyheight = 1)
           # ,
           # colour=guide_legend(keywidth = 3, keyheight = 1)
           ) +
    scale_x_date(limits=c(min(selected()$index_dt), max=max(selected()$fst_sup)),
                 labels=date_format("%m-%d-%Y"),
                 breaks = date_breaks("1 months")) +
    scale_y_continuous(breaks=c(0, 15,30,45), labels = c("Other","Warfarin", "LMWH", "DOACS"),
                       limits=c(-3, 48)) +
    labs(x = "Fill Date", y = "Anti-coagulant Category") +
    facet_wrap(~patid, ncol=1, labeller = label_both) + #, scales='free_y'
    theme(axis.ticks.y=element_blank(), 
          axis.text.x=element_text(angle=45, hjust=1),
          plot.margin=unit(c(0.5,2,1.5,1.2),"cm"),
          legend.position=c(1, 1),
          legend.key = element_rect(fill = "white", colour = "black"))

      
      
      # geom_vline(aes(xintercept = as.numeric(index_dt)),
      #            colour=guide_legend("Index VTE Date")) +
      # geom_point(aes(x=fst_dt, y=id, colour=factor(category))) +
      # geom_segment(aes(x=fst_dt, xend=fst_sup, y=id, yend=id, 
      #                  colour=factor(category)), size=0.5, show.legend=F) +
      # # geom_segment(data = selected_inr(),
      # #              aes(x=inr_dt, xend=inr_dt, y=inr_num_start, yend=inr_num,
      # #                  colour=factor(category)),
      # #              arrow = grid::arrow(length = unit(0.25, "cm")),
      # #              inherit.aes = FALSE) +
      # geom_point(data = selected_inr(), 
      #            aes(x=inr_dt, y=inr_num), shape=3, inherit.aes = FALSE) +
      # scale_shape_manual(name = 'INR Date') +
      # # scale_shape_identity(name = 'INR Date', guide = 'legend',labels = c('INR Test')) +
      # scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
      #                     labels = c("Warfarin", "LMWH", "DOACS")) +
      # scale_x_date(limits=c(min(selected()$index_dt), max=max(selected()$fst_sup)),
      #              labels=date_format("%m-%d-%Y"),
      #              breaks = date_breaks("2 months")) +
      # scale_y_continuous(breaks=c(15,30,45), labels = c("Warfarin", "LMWH", "DOACS"),
      #                    limits=c(0, 48)) +
      # labs(x = "Fill Date", y = "Anti-coagulant Category") +
      # facet_wrap(~patid, ncol = 1) +
      # theme(axis.ticks.y=element_blank()) +
      # guides(colour=guide_legend(title="AC Category"), shape=guide_legend(title = "INR Date"))
      # # colour=guide_legend(override.aes=list(linetype=c(1,0), shape=c(NA,16)))

    suppressWarnings(ggplotly(p,tooltip = c("text")) %>%
                       layout(dragmode = "select", autosize = FALSE, height = n_pat()*400, width=1000)) 
  })

  # output$hover <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  # })
  # 
  # output$click <- renderPrint({
  #   d <- event_data("plotly_click")
  #   if (is.null(d)) "Click events appear here (double-click to clear)" else d
  # })
  # 
  # output$brush <- renderPrint({
  #   d <- event_data("plotly_selected")
  #   if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
  # })


})
