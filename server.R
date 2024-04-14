

server <- function(input, output, session) {
  
  
  
  get_non_sit <- reactive({
    input$tu_slp + input$tu_lpa + input$tu_vpa
  }) 
  
  observe({
    updateNumericInput(session, "tu_sit", value = 24 - get_non_sit())
  }) 
  
  get_cur_day <- reactive({
    data.frame(
      group = c("Sleep", "Light PA", "Sit", "Mod-vig PA"),
      value = c(input$tu_slp, input$tu_lpa, input$tu_sit, input$tu_vpa)
    )
  })   
  
  current_24_diff <- reactive({
    curr_sum <- sum(get_cur_day()$value)
    curr_sum <- ifelse(curr_sum < 1e-2, 0, curr_sum)
    cdif <- curr_sum - 24
    cdif
  })
  
  current_24_diff_txt <- reactive({
    curr_offset <- current_24_diff()
    in_excess_deficit <- 
      ifelse(curr_offset > 0, "excess", ifelse(curr_offset < 0, "deficit", ""))
    out_str <- 
      paste0(in_excess_deficit, " of ", curr_offset, " hours ", h_to_hm(curr_offset))
    return(out_str)
  })
  
  what_do_txt <- reactive({
    curr_offset <- current_24_diff()
    what_do <- 
      ifelse(curr_offset > 0, "remove", ifelse(curr_offset < 0, "add", ""))
    out_str <- 
      paste0("Please ", what_do, " ", curr_offset, " hours from your time-use categories.")
    return(out_str)
  })
  
  
  # output$tu_slp_out <- reactive({HTML(paste0('<p style="size:9;">', h_to_hm(input$tu_slp), '</p>'))})
  # renderUI("tu_slp_out")
  output$tu_slp_out <- renderUI({
    p(h_to_hm(input$tu_slp), style = "color: #999999;")
  })
  
  output$tu_lpa_out <- renderUI({
    p(h_to_hm(input$tu_lpa), style = "color: #999999;")
  })
  
  output$tu_vpa_out <- renderUI({
    p(h_to_hm(input$tu_vpa), style = "color: #999999;")
  })
  
  output$tu_sit_out <- renderUI({
    p(h_to_hm(input$tu_sit), style = "color: #999999;")
  })

  
  
  # gg_plot1 <- reactive({
  #   ggplot(get_cur_day(), aes(x = "", y = value, fill = group)) +
  #     geom_col(color = "white") +
  #     coord_polar(theta = "y") +
  #     geom_text(aes(label = group),
  #               position = position_stack(vjust = 0.5)) +
  #     theme_void() +
  #     theme(legend.position = "none")
  # }) 
  # 
  # output$gg_cur <- renderPlot(gg_plot1())
  
 ggplot1 <- reactive({
    
    dat1 <- get_cur_day()
    dat2 <- dat1
    dat2$value <- c(ideal_day_vec)
    
    dat1$daytype <- "My current day"
    dat2$daytype <- "My 'ideal' day"
    
    dat <- rbind(dat1, dat2)
    
    dat$daytype <- fct_inorder(dat$daytype)
    
    comp_names <- 
      c(
        "Sleep", "Sedentary\nbehaviour", 
        "Light\nphysical activity", "Moderate-vigorous\nphysical activity"
      )
    dat <- 
      dat %>%
      mutate(
        group_fct = case_when(
          group == "Sleep" ~ comp_names[1],
          group == "Sit" ~ comp_names[2],
          group == "Light PA" ~ comp_names[3],
          group == "Mod-vig PA" ~ comp_names[4],
          TRUE ~ as.character(NA)
        ),
        group_fct = factor(group_fct, levels = comp_names)
      )
    
    plt_obj <-
      ggplot(dat, aes(x = group_fct, y = value, fill = daytype)) +
      geom_col(color = "white", width = 0.5, position = position_dodge(0.5)) +
      geom_text(aes(label = value), vjust = -0.3, size = 4, position = position_dodge(0.5)) +
      theme_classic() +
      theme(legend.position = "bottom", text = element_text(size = 14)) +
      scale_fill_manual(values = daytype_cols) +
      labs(
        x = "", fill = "",
        y = "Hours/day"
      )
    
    plt_obj
    
  }) 
  
  output$gg_cur <- renderPlot(ggplot1())
  
  
  plotly1 <- reactive({
    
    dat <- get_cur_day()
    dat <- dat %>% rename(curr_day = value)
    dat$ideal_day <- c(ideal_day_vec)
    
    comp_names <- 
      c(
        "Sleep", 
        "Sedentary\nbehaviour", 
        "Light\nphysical activity", 
        "Moderate-vigorous\nphysical activity"
      )
    dat <- 
      dat %>%
      mutate(
        group_fct = case_when(
          group == "Sleep" ~ comp_names[1],
          group == "Sit" ~ comp_names[2],
          group == "Light PA" ~ comp_names[3],
          group == "Mod-vig PA" ~ comp_names[4],
          TRUE ~ as.character(NA)
        ),
        group_fct = factor(group_fct, levels = comp_names)
      )
    
    # plt_obj <-
    #   ggplot(dat, aes(x = group_fct, y = value, fill = daytype)) +
    #   geom_col(color = "white", width = 0.5, position = position_dodge(0.5)) +
    #   geom_text(aes(label = value), vjust = -0.3, size = 4, position = position_dodge(0.5)) +
    #   theme_classic() +
    #   theme(legend.position = "bottom", text = element_text(size = 14)) +
    #   scale_fill_manual(values = daytype_cols) +
    #   labs(
    #     x = "", fill = "",
    #     y = "Hours/day"
    #   )
    # 
    # ggplotly(plt_obj)

   fig <- plot_ly(
     dat,
     x = ~group_fct, 
     y = ~curr_day, 
     type = "bar", 
     name = "My current day",
     marker = list(color = "rgb(166, 166, 166)")
   )

   fig <- fig %>% 
     add_trace(
       y = ~ideal_day, 
       name = "My 'ideal' day",
       marker = list(color = "rgb(0, 176, 240)")
      )

   fig <- fig %>% layout(
     # title = "US Export of Plastic Scrap",
     xaxis = list(
       title = "",
       tickfont = list(
         size = 14,
         color = "rgb(107, 107, 107)"
       )
     ),
     yaxis = list(
       title = "Hours/day",
       titlefont = list(
         size = 16,
         color = "rgb(107, 107, 107)"
       ),
       tickfont = list(
         size = 14,
         color = "rgb(107, 107, 107)"
       )
     ),
     legend = 
       list(
         x = 0.5,
         xanchor = "center",
         y = -0.5, # between "-2" and "3" if `yref` is "paper"
         bgcolor = "rgba(255, 255, 255, 0)", 
         bordercolor = "rgba(166, 166, 166, 0.5)",
         borderwidth = 2,
         orientation = "h"
         # valign = "bottom"
      ),
     barmode = "group", 
     bargap = 0.15
     # bargroupgap = 0.1
   )


   fig
    
    
  }) 
  
  output$ply_cur <- renderPlotly(plotly1())
  
  
  
  output$ui0 <- renderUI({
    
    # "Sleep", "Light PA", "Sit", "Mod-vig PA"
    change_req <- c(ideal_day_vec) - get_cur_day()$value
    
    card(
      full_screen = TRUE,
      card_header("Your current day vs. your 'ideal' day"),
      layout_columns(
        col_widths  = c(8, 4),
        box(width = 12,
            # p(strong("Your current day"), style = "text-align: center;"),
            if (use_plotly) {
              plotlyOutput("ply_cur")
            } else {
              plotlyOutput("gg_cur")
            }
        ),
        box(width = 12,
            p(strong("To reach your"), strong("'ideal'", style = "color: #00b0f0"), strong("day, you need to change:"), style = "text-align: center; color: #000000"),
            p(),    
            p(),
            p(strong("Sleep"), "by", style = "text-align: center; size: 10;"),
            p(
              paste(add_sgn(change_req[1]), "hours"), 
              style = paste0("color: ", get_sgn_hex_col(change_req[1]), "; size: 20; text-align: center;")),
            p(),
            p(strong("Sitting"), "by", style = "text-align: center; size: 10;"),
            p(
              paste(add_sgn(change_req[3]), "hours"), 
              style = paste0("color: ", get_sgn_hex_col(change_req[3]), "; size: 20; text-align: center;")),
            p(),
            p(strong("Light physical activity"), "by", style = "text-align: center; size: 10;"),
            p(
              paste(add_sgn(change_req[2]), "hours"), 
              style = paste0("color: ", get_sgn_hex_col(change_req[2]), "; size: 20; text-align: center;")),
            p(),
            p(strong("Moderate-vigorous physical activity"), "by", style = "text-align: center; size: 10;"),
            p(
              paste(add_sgn(change_req[4]), "hours"), 
              style = paste0("color: ", get_sgn_hex_col(change_req[4]), "; size: 20; text-align: center;"))
        ),
        tags$script(HTML("$('.box').eq(1).css('border', '2px solid #999999');"))
        # , box(width = 12,
        #     p(strong("Your best day"), style = "text-align: center;"),
        #     plotOutput("gg_best")
        # )
      )
    )
    
  })
  
  
  
  output$ui1 <- renderUI({
    
    # update any move
    curr_realloc <- 
      input$slide_lpa + input$slide_vpa +
      input$slide_sed + input$slide_slp
    
    ### intialise before if-else conditions
    fat_val <- 0 # runif(1) - 0.5 # used to be random
    
    if (input$slide_vpa >= 3) {
      fat_val <- 0.5
    } else if (input$slide_vpa < 0) {
      fat_val <- -0.5
    } else if (input$slide_lpa >= 5)  {
      fat_val <- 0.5
    } else if (input$slide_lpa <= -2)  {
      fat_val <- -0.5
    } else if (input$slide_sed >= 1)  {
      fat_val <- -0.5
    } else if (input$slide_sed <= -10) {
      fat_val <- 0.5
    } else {
      fat_val <- 0
    }
    
    pm <-     ifelse(fat_val < 0,    "", ifelse(fat_val > 0,     "+",      "")) 
    bx_col <- ifelse(fat_val < 0, "red", ifelse(fat_val > 0, "green", "black")) 
    bx_img <- ifelse(fat_val < -0.25, "red.png", ifelse(fat_val > 0.25, "green.png", "orange.png")) 
    
    if (abs(curr_realloc) > 1e-6) { # reallocations not 
      bx_img <- "not0.png"
    }
    
    # valueBox(
    #   value = paste0(pm, sprintf("%1.1f", fat_val)), 
    #   subtitle = "Predicted change in cognitive function (z-score):", 
    #   width = 12, 
    #   color = bx_col
    # )
    box(
      width = 12,
      p(strong("Predicted change in cognitive function"), style = "text-align: center;"),
      # p("Current status is: ", icon("ok", lib = "glyphicon")),
      # p("Current status is: ", icon("snowplow", lib = "font-awesome"))
      div(img(src = bx_img, height = 300 * 927 / 820, width = 300), style = "text-align: center;")
    )
  })
  

  output$ui3 <- renderUI({
    
    out_card <- NULL
    
    if (abs(current_24_diff()) < 1e-3) {
      out_card <- 
        card(
          # full_screen = TRUE,
          # fill = FALSE,
          card_header("Great work!"),
          layout_columns(
            p(
              "You have now created your lifestyle profile. On the next page, we will show you a visual of what the 'ideal' 24-hours looks like for your cognitive function, compared to what your current day looks like."
              #, style = "background-color: #78c2ad"
            )
          )
        )
    } else {
      out_card <- 
        card(
            # full_screen = TRUE,
            # fill = FALSE,
            card_header("Time-use doesn't add up to 24 hours"),
            layout_columns(
              p(
                "You are currently", current_24_diff_txt(), ".", what_do_txt()
              )
            )
          )
    }
    
    out_card
    
  })
  
  
  # output$Next_Previous <- renderUI({
  #   
  #   Previous_Button <-
  #     actionButton(
  #       "Prev_Tab", 
  #       p("Prev")
  #       # HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>')
  #     )
  #   Next_Button <- 
  #     actionButton(
  #       "Next_Tab", 
  #       # HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')
  #       p("Next")
  #     )
  #   
  #   layout_columns(
  #     fill = FALSE,
  #     col_widths = c(2, 8, 2),
  #     height = "20px",
  #     Previous_Button, 
  #     p(""),
  #     Next_Button
  #   )
  #   
  # })
  
  observeEvent(input$next1, {
    nav_select(id = "pillset", selected = "Cognitive outcome")
  })
  
  observeEvent(input$prev2, {
    nav_select(id = "pillset", selected = "Welcome")
  })
  observeEvent(input$next2, {
    nav_select(id = "pillset", selected = "Demographics")
  })
  
  observeEvent(input$prev3, {
    nav_select(id = "pillset", selected = "Cognitive outcome")
  })
  observeEvent(input$next3, {
    nav_select(id = "pillset", selected = "Health")
  })
  
  observeEvent(input$prev4, {
    nav_select(id = "pillset", selected = "Demographics")
  })
  observeEvent(input$next4, {
    nav_select(id = "pillset", selected = "Time-use")
  })
  
  observeEvent(input$prev5, {
    nav_select(id = "pillset", selected = "Health")
  })
  observeEvent(input$next5, {
    nav_select(id = "pillset", selected = "Ideal day")
  })
  
  observeEvent(input$prev6, {
    nav_select(id = "pillset", selected = "Time-use")
  })
  observeEvent(input$next6, {
    nav_select(id = "pillset", selected = "Small steps")
  })
  
  observeEvent(input$prev7, {
    nav_select(id = "pillset", selected = "Ideal day")
  })
  
}




