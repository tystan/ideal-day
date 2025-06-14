

server <- function(input, output, session) {
  
  
  
  get_cov_from_ui <- reactive({
    
    # print(input$demos_age)
    # print(input$demos_sex)
    # print(input$demos_wei)
    # print(input$demos_hei)
    # print(input$demos_edu)
    # print(input$demos_hear)
    # print(input$demos_lone)
    # print(input$demos_alc)
    # print(input$demos_smok)
    # print(input$demos_cran)
    # print(input$demos_hyp)
    # print(input$demos_dep)
    # print(input$demos_t2d)
    
    special_print_for_console(
      "This is the current set of inputs from the user",
      "(not the ilrs, those are default vals)"
    )
    
    mk_cov_df_from_ui(
      age = input$demos_age, 
      sex = input$demos_sex, 
      bmi = calc_bmi(input$demos_wei, input$demos_hei), 
      edu = input$demos_edu, 
      hear = input$demos_hear, 
      iso = input$demos_lone, 
      alc = input$demos_alc, 
      smk = input$demos_smok,  
      binj = input$demos_cran, 
      hbp = input$demos_hyp, 
      dep = input$demos_dep, 
      t2d = input$demos_t2d,
      verbose = TRUE
    )
  })  
    
  get_cog_outc <- reactive({
    this_outc <- input$cog_outc
    
    special_print_for_console(
      "The current selected outcome is:"
    )
    print(this_outc)
    
    this_outc
  }) 
  
  get_non_sit <- reactive({
    
    slp_lpa_vpa <- input$tu_slp + input$tu_lpa + input$tu_vpa
    
    special_print_for_console(
      "The current sum of inputed current sleep + lpa + vpa is:"
    )
    print(slp_lpa_vpa)
    
    slp_lpa_vpa
  }) 
  
  observe({
    updateNumericInput(session, "tu_sit", value = 24 - get_non_sit())
  }) 
  
  get_cur_day <- reactive({
    cd_df <- data.frame(
      group = c("Sleep", "Light PA", "Sit", "Mod-vig PA"),
      value = c(input$tu_slp, input$tu_lpa, input$tu_sit, input$tu_vpa)
    )
    
    special_print_for_console(
      "The current time-use day inputted is:"
    )
    print(as_tibble(cd_df))
    
    cd_df
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
  
  
  
  get_opt_cmp_from_ui <- reactive({
    
    this_cov_df <- get_cov_from_ui()
    
    this_strata_grid <- 
      get_strata_grid(
        s = this_cov_df[["sex"]], 
        a = this_cov_df[["age"]], 
        b = this_cov_df[["bmi"]]
      )[, cmp_nms]
    

    # print(this_strata_grid)
    
    
    grid_predictor_df <- 
      mk_predictor_df(
        cmp_df = this_strata_grid, 
        cov_df = this_cov_df
      )
    special_print_for_console(
      paste0(
        "These are the first 10 rows of (",
        nrow(grid_predictor_df),
        ") that are considered for optimisation"
      ),
      "(based on the strata's fenced grid)"
    )
    print(as_tibble(grid_predictor_df))
    
    # print(as_tibble(get_opt_cmp_from_preds(grid_predictor_df)))
    grid_opt_obj <- 
      get_opt_cmp_from_preds(
        grid_predictor_df, 
        which_outc = get_cog_outc()
      ) 
    special_print_for_console(
      "From the optimisation procedure, this is the optimum composition and",
      "corresponding predicted outcome:"
    )
    print(grid_opt_obj$opt_cmp)
    
    grid_opt_obj
    
  })
  
  get_curr_cmp_pred <- reactive({
    
    this_cov_df <- get_cov_from_ui()

    this_strata <- 
      get_strata_id(
        s = this_cov_df[["sex"]], 
        a = this_cov_df[["age"]], 
        b = this_cov_df[["bmi"]]
      )

      
    dat <- get_cur_day()
    # print(dat)
    row_v <- dat[["group"]]
    # print(row_v)
    # print(dat[["value"]])
    # print(class(dat[["value"]]))
    # print(dat[["value"]][row_v == "Sleep"])
    # print(dat[["value"]][row_v == "Sit"])
    # print(dat[["value"]][row_v == "Light PA"])
    # print(dat[["value"]][row_v == "Mod-vig PA"])
    
    this_cmp_df <- 
      mk_cmp_df_from_ui(
        sleep = dat[["value"]][row_v == "Sleep"], 
        sb = dat[["value"]][row_v == "Sit"], 
        lpa = dat[["value"]][row_v == "Light PA"], 
        mvpa = dat[["value"]][row_v == "Mod-vig PA"]
        # strata_id = this_strata
      )
    # print(this_cmp_df)
    # print(class(this_cmp_df))
    # print(class(this_cov_df))
    # print(nrow(this_cmp_df))
    # print(nrow(this_cov_df))
    this_cog_outc <- get_cog_outc()
    this_predictor_df <- 
      mk_predictor_df(cmp_df = this_cmp_df, cov_df = this_cov_df)
    this_predict <- 
      mk_pred_over_ilrs(
        this_predictor_df, 
        which_outc = this_cog_outc
      )
    
    special_print_for_console(paste(
      "This is the current day predicted outcome for",
      this_cog_outc,
      "is:"
    ))
    print(this_predict)
    
    list(dat = dat, y_hat = this_predict, strata_id = this_strata)
    
  })
  
  plotly1 <- reactive({
    
    grid_opt_obj <- get_opt_cmp_from_ui()
    grid_opt_df <- grid_opt_obj$opt_cmp
    grid_ys <- grid_opt_obj$y_dist
    

    # print(grid_ys)
    
    lst_obj <- get_curr_cmp_pred()
    curr_pred <- lst_obj$y_hat
    
    dat <- lst_obj$dat %>% rename(curr_day = value)
    # print(dat)
    ideal_day_calc <- 
      c(
        "Sleep" = grid_opt_df[["sleep"]], # / 60, 
        "Light PA" = grid_opt_df[["lpa"]], # / 60, 
        "Sit" = grid_opt_df[["sb"]], # / 60, 
        "Mod-vig PA" = grid_opt_df[["mvpa"]] # / 60, 
      )
    # print(dat)
    # print(ideal_day_calc)
    
    dat$ideal_day <- ideal_day_calc 
      
    
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

   outc_nm <- names(cog_outc_choices_alt)[cog_outc_choices_alt == get_cog_outc()]
   fig <- fig %>% 
     add_trace(
       y = ~ideal_day, 
       name = paste0(
         "My 'ideal' day (", 
         outc_nm,
         ")"
      ),
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
    
    
    grid_opt_obj <- get_opt_cmp_from_ui()
    grid_opt_df <- grid_opt_obj$opt_cmp
    grid_ys <- grid_opt_obj$y_dist
    
    # special_print_for_console(
    #   "This is the optimum time-use and corresponding predicted",
    #   "outcome for the current inputs:"
    # )
    # print(grid_opt_df)
    # 
    
    lst_obj <- get_curr_cmp_pred()
    curr_pred <- lst_obj$y_hat
    dat <- lst_obj$dat 
    
    
    # print(dat)
    ideal_day_calc <- 
      c(
        "Sleep" = grid_opt_df[["sleep"]], # / 60, 
        "Light PA" = grid_opt_df[["lpa"]], # / 60, 
        "Sit" = grid_opt_df[["sb"]], # / 60, 
        "Mod-vig PA" = grid_opt_df[["mvpa"]] # / 60, 
      )
    # print(dat)
    # print(ideal_day_calc)
    
    
    # "Sleep", "Light PA", "Sit", "Mod-vig PA"
    # print(ideal_day_calc)
    curr_day <- dat$value
    names(curr_day) <- dat$group
    # print(curr_day)
    change_req <- ideal_day_calc - curr_day
    
    outc_nm <- names(cog_outc_choices_alt)[cog_outc_choices_alt == get_cog_outc()]
    
    card(
      full_screen = TRUE,
      card_header(paste0("Your current day vs. your 'ideal' day (", outc_nm, ")")),
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
            p(
              strong("To reach your"), 
              strong("'ideal'", style = "color: #00b0f0"), 
              strong(paste0("day (", outc_nm, "), you should aim to change:")), 
              style = "text-align: center; color: #000000"
            ),
            p(),    
            p(),
            p(strong("Sleep"), "by", style = "text-align: center; size: 10;"),
            p(
              paste(add_sgn(change_req[1]), "hours"), 
              style = 
                paste0(
                  "color: ", get_sgn_hex_col(change_req[1]), "; size: 20; text-align: center;"
                )
            ),
            p(),
            p(strong("Sitting"), "by", style = "text-align: center; size: 10;"),
            p(
              paste(add_sgn(change_req[3]), "hours"), 
              style = 
                paste0(
                "color: ", get_sgn_hex_col(change_req[3]), "; size: 20; text-align: center;"
                )
            ),
            p(),
            p(strong("Light physical activity"), "by", style = "text-align: center; size: 10;"),
            p(
              paste(add_sgn(change_req[2]), "hours"), 
              style = 
                paste0(
                  "color: ", get_sgn_hex_col(change_req[2]), "; size: 20; text-align: center;"
                )
            ),
            p(),
            p(
              strong("Moderate-vigorous physical activity"), 
              "by", 
              style = "text-align: center; size: 10;"
            ),
            p(
              paste(add_sgn(change_req[4]), "hours"), 
              style = 
                paste0(
                  "color: ", get_sgn_hex_col(change_req[4]), "; size: 20; text-align: center;"
                )
            )
        ),
        tags$script(HTML("$('.box').eq(1).css('border', '2px solid #999999');"))
        # , box(width = 12,
        #     p(strong("Your best day"), style = "text-align: center;"),
        #     plotOutput("gg_best")
        # )
      )
    )
    
  })
  
  
  get_realloc_cmp <- reactive({
  # update any move
      c(
        sleep = input$slide_slp / 60, 
        sb = input$slide_sed / 60, 
        lpa = input$slide_lpa / 60, 
        mvpa = input$slide_vpa / 60
      )
  
  })
  
  get_new_cmp_pred <- reactive({
    
    this_cov_df <- get_cov_from_ui()
    
    dat <- get_cur_day()
    row_v <- dat[["group"]]
    orig_alloc <-
      c(
        sleep = dat[["value"]][row_v == "Sleep"], 
        sb = dat[["value"]][row_v == "Sit"], 
        lpa = dat[["value"]][row_v == "Light PA"], 
        mvpa = dat[["value"]][row_v == "Mod-vig PA"]
      )
    
    realloc_delta <- get_realloc_cmp()

    # print(orig_alloc)
    # print(realloc_delta)
    
    realloc_cmp_df <- 
      mk_cmp_df_from_ui(
        sleep = orig_alloc["sleep"] + realloc_delta["sleep"], 
        sb = orig_alloc["sb"] + realloc_delta["sb"], 
        lpa = orig_alloc["lpa"] + realloc_delta["lpa"], 
        mvpa = orig_alloc["mvpa"] + realloc_delta["mvpa"], 
      )
    
    special_print_for_console(
      "This is the new reallocated time-use day set of predictors:"
    )
    print(realloc_cmp_df)

    this_cog_outc <- get_cog_outc()
    
    realloc_predictor_df <- 
      mk_predictor_df(
        cmp_df = realloc_cmp_df, 
        cov_df = this_cov_df
      )
    
    realloc_predict <- 
      mk_pred_over_ilrs(
        realloc_predictor_df,
        which_outc = this_cog_outc
      )
    
    special_print_for_console(paste0(
      "This is the new reallocated time-use day predicted outcome for ",
      this_cog_outc, 
      ":"
    ))
    print(realloc_predict)
    
    list(delta = realloc_delta, y_hat = realloc_predict, cmp_df = realloc_cmp_df)
    
  })
  
  output$ui1 <- renderUI({
    
  # the traffic lights logic/algorithm is:
  #   * There is a predicted cog value for the user’s current inputs, call this: y_current
  #   * There is an optimal composition (additionally with the user’s covariates/demographics) 
  #       with a predicted cog value, call this: y_opt
  #   * With the user’s reallocation in composition, there is another predicted cog value, 
  #       call this: y_new
  # Then:
  #   [GREEN] light if: (y_new – y_current) >= p * (y_opt – y_current) 
  #      [[that is, if p = 1/3 say, the change is a third of the way to y_opt in a positive direction]]
  #   [RED] light if: (y_new – y_current) <= - p * (y_opt – y_current) 
  #      [[that is, if p = 1/3 say, the change is a third of distance in the opposite direction to y_opt]]
  #   [ORANGE] light otherwise
    
    dat <- get_cur_day()
    row_v <- dat[["group"]]
    this_cmp_df <- 
      mk_cmp_df_from_ui(
        sleep = dat[["value"]][row_v == "Sleep"], 
        sb = dat[["value"]][row_v == "Sit"], 
        lpa = dat[["value"]][row_v == "Light PA"], 
        mvpa = dat[["value"]][row_v == "Mod-vig PA"]
        # strata_id = this_strata
      )

    grid_opt_obj <- get_opt_cmp_from_ui()
    grid_opt_df <- grid_opt_obj$opt_cmp
    grid_ys <- grid_opt_obj$y_dist
    # x_opt <- 
    y_opt <- grid_opt_df[["y_hat"]]
    
    lst_obj <- get_curr_cmp_pred()
    y_cur <- lst_obj$y_hat
    contraint_strata <- lst_obj$strata_id
    
    m_v_lst_strata <- 
      m_and_v_ilrs %>% 
      dplyr::filter(strata_id == contraint_strata)
    
    
    contour_cmp_cur <- 
      get_countour_propn(
        mk_ilr(this_cmp_df[1, ]), 
        m_v_lst_strata[["m"]][[1]],
        m_v_lst_strata[["v"]][[1]]
      )
    feasible_cmp_cur <- 
      (contour_cmp_cur <= 0.8)
      
    # is_within_constraint(rep(0, 3), rep(0, 3), diag(3))
    
    special_print_for_console(
      "The original composition has the estimated quantile contour from the mean:"
    )
    cat(sprintf("%1.3f", contour_cmp_cur))
    
    if (!feasible_cmp_cur) {
      special_print_for_console(
        "NOTE the original composition is NOT within the feasible region!",
        "NB: therefore calculating y_cur is extrapolating beyond the highest density",
        "of the sample data so we will set y_cur to the median of all the fenced time-use grid",
        "predictions as a default but is not used in any calculations from here"
        # "(manually making y_current median of grid ests)\n"
      )
      #   print(as_tibble(out_df))
      y_cur <- unname(quantile(grid_ys, 0.5))
    }
    
    delta_obj <- get_new_cmp_pred()
    
    special_print_for_console("The current change in time-use is:")
    print(delta_obj[1]) # $delta
    
    y_new  <- delta_obj$y_hat
    delta_sum <- sum(delta_obj$delta)
    new_cmp_df  <- delta_obj$cmp_df
    
    
    contour_cmp_new <- 
      get_countour_propn(
        mk_ilr(new_cmp_df[1, ]),
        m_v_lst_strata[["m"]][[1]],
        m_v_lst_strata[["v"]][[1]]
      )
    feasible_cmp_new <- 
      (contour_cmp_new <= 0.8)
    
    special_print_for_console(
      "The new/realloc composition is estimated to be at quantile contour from the mean:"
    )
    cat(sprintf("%1.3f", contour_cmp_new))
    # is_within_constraint(rep(0, 3), rep(0, 3), diag(3))
    
    if (!feasible_cmp_new) {
      special_print_for_console(
        "NOTE the new/realloc composition is NOT within the feasible region!",
        "NB: therefore calculating y_new is extrapolating beyond the highest density",
        "of the sample data so we will set y_new to the median of all the fenced time-use grid",
        "predictions as a default but is not used in any calculations from here"
        # "(manually making y_new median of grid ests)\n"
      )
      #   print(as_tibble(out_df))
      y_new <- unname(quantile(grid_ys, 0.5))
    }
    
    x_cur <- this_cmp_df
    x_new <- delta_obj$cmp_df
    x_opt <- 60 * grid_opt_df[cmp_nms]
    
    special_print_for_console(
      "These are the predicted outcome values for the (current, new, optimal) predictions",
      "NB: y_cur and/or y_new may be the same value (median of all the fenced time-use grid preds)",
      "when the cur and new compositions/ilrs are outside the feasible contours/time-use grid for the strata"
    )
    print(c(y_cur = y_cur, y_new = y_new, y_opt = y_opt))
    
    special_print_for_console(
      "These are the (current, new, optimal) time-use compositions"
    )
    print(rbind(x_cur = x_cur, x_new = x_new, x_opt = x_opt))
    
    
    z_cur <- mk_ilr(x_cur)
    z_new <- mk_ilr(x_new)
    z_opt <- mk_ilr(x_opt)
    
    special_print_for_console(
      "These are the corresponding (current, new, optimal) ilrs"
    )
    print(rbind(z_cur = z_cur, z_new = z_new, z_opt = z_opt))
    
    z_propos <- z_new - z_cur
    z_wanted <- z_opt - z_cur
    
    
    z_propos <- unlist(z_propos)
    z_wanted <- unlist(z_wanted)
    ### testing
    # unlist(data.frame(a=1,b=2,c=1))
    # class(unlist(data.frame(a=1,b=2,c=1)))
    
    special_print_for_console(
      "These are the proposed (new - cur) and wanted (optimal - cur) vectors [in the ilr-space]"
    )
    print(rbind(z_propos = z_propos, z_wanted = z_wanted))

    # print(class(z_propos))
    # print(class(z_wanted))
    
    
    scalar_proj_val <- 
      ifelse(
        vlength_euclid(z_propos) < effective_zero,
        0,
        vlength_euclid(z_propos) * cosangl_euclid(z_propos,z_wanted)
      )
    special_print_for_console(
      "This is the scalar projected value of [z_propos] onto [z_wanted]",
      "It can be thought of as the length the vector [z_propos] sits ontop of [z_wanted]",
      "Larger positive values mean [z_propos] and [z_wanted] are more similar (angle < 90 deg)",
      "0 means [z_propos] is a 0 vector or orthogonal (angle ==90 deg) to [z_wanted] ",
      "Larger negative values mean [z_propos] and [z_wanted] point in different directions (angle > 90 deg)"
    )
    print(scalar_proj_val)
   
    special_print_for_console(
      "This is the covariance matrix in the ilr space (specific to each strata A to H)",
        "The (matrix squareroot) inverse of this covariance matrix is used to standardise changes",
        "in the three ilr axis directions based on both the sample standard deviations in the",
        "time-use compositions (ilrs) and their covariance/correlations!",
      "For example MVPA is generally less variable so even small changes in this should be weighted",
      "more than small changes in sleep or sedentarry behaviour for example"
    )
    std_var_mat <- m_v_lst_strata[["v"]][[1]]
    print(std_var_mat)
    # print(class(std_var_mat))


    std_var_mat_inv <- sqrtm(solve(std_var_mat))
    # print(std_var_mat_inv)
    # print(class(std_var_mat_inv))
    
    # print(matrix(z_propos, nrow = 1))
    # print(std_var_mat_inv)
    z_propos_std <- as.numeric(
      matrix(z_propos, nrow = 1) %*% std_var_mat_inv
    )
    z_wanted_std <-  as.numeric(
      matrix(z_wanted, nrow = 1) %*% std_var_mat_inv
    )
    
    special_print_for_console(
      "These are the standardised proposed and wanted vectors ([z_propos_std] and [z_wanted_std])",
      "using the sample covariance to standardise their differences from the origin (0 vector)",
      "i.e., [z_propos_std] = t([z_propos]) %*% Sigma^(-1/2)"
    )
    print(rbind(z_propos_std = z_propos_std, z_wanted_std = z_wanted_std))
   # print(z_propos_std)
   # print(z_wanted_std)
   
    scalar_proj_val_std <- 
      ifelse(
        abs(scalar_proj_val) < effective_zero,
        0,
        vlength_euclid(z_propos_std) * 
          cosangl_euclid(
            z_propos_std,
            z_wanted_std
          )
      )
    
    special_print_for_console(
      "This is the scalar projected value of the sample covariance standardised [z_propos_std] onto [z_wanted_std]",
      "This is a better measure of the relative length the vector [z_propos] sits ontop of [z_wanted] when",
      "importantly considering the sample covariance to up-weight hard time-use changes or down-weight easier time-use changes",
      "NB: currently this value needs to be greater than 0.05 or less than -0.05 to invoke a change from orange light to green or red, respectively."
    )
   print(scalar_proj_val_std)
    
   
   special_print_for_console(
     "And this is the ratio of the scalar projected value and the length sample covariance standardised [z_wanted_std].",
     "i.e., length(projection of [z_propos_std] ON [z_wanted_std]) / length([z_wanted_std]).",
     "This value represents the amount/proportion of the [z_wanted_std] vector covered by [z_propos_std]."
   )
   proj_vec <- 
     if (abs(vlength_euclid(z_wanted_std)) < effective_zero) {
       rep(0, 3)
     } else {
       scalar_proj_val_std * z_wanted_std / vlength_euclid(z_wanted_std)
     }
   ratio_of_wanted <- 
     ifelse(
       abs(vlength_euclid(z_wanted_std)) < effective_zero, 
       0, 
       scalar_proj_val_std / vlength_euclid(z_wanted_std)
     )
   ### redundant
   # print(paste("length(proj_[z_wanted_std]([z_propos_std])) =", vlength_euclid(scalar_proj_val_std * z_wanted_std / vlength_euclid(z_wanted_std))))
   cat(paste0("[z_wanted_std] = c(", paste(round(z_wanted_std, 4), collapse = ", "),")\n\n"))
   cat(paste0("proj_[z_wanted_std]([z_propos_std]) = c(", paste(round(proj_vec, 4), collapse = ", "),")\n\n"))
   cat(paste("scalar projected value = length(proj_[z_wanted_std]([z_propos_std])) =", scalar_proj_val_std, "\n\n"))
   cat(paste("length([z_wanted_std]) =", vlength_euclid(z_wanted_std), "\n\n"))
   cat(paste("length(proj_[z_wanted_std]([z_propos_std])) / length([z_wanted_std]) =", ratio_of_wanted, "\n\n"))

   
    ### intialise before if-else conditions
    # is_opt_largest <- (y_opt > y_cur)
    # is_good_delta <- is_opt_largest & check_good_delta(y_new, y_cur, y_opt)
    # is_bad_delta  <- is_opt_largest & check_bad_delta(y_new, y_cur, y_opt)
    is_magnitude <- (abs(scalar_proj_val_std) > 0.05)
    is_good_delta <- is_magnitude & (scalar_proj_val_std > 0)
    is_bad_delta  <- is_magnitude & (scalar_proj_val_std < 0)
    
    # pm <-     ifelse(fat_val < 0,    "", ifelse(fat_val > 0,     "+",      "")) 
    # bx_col <- ifelse(fat_val < 0, "red", ifelse(fat_val > 0, "green", "black")) 
    bx_img <- ifelse(is_bad_delta, "red.png", ifelse(is_good_delta, "green.png", "orange.png")) 
    
    if (abs(delta_sum) > 1e-6) { # reallocations not sum to one
      bx_img <- "not0.png"
    }
    
    # valueBox(
    #   value = paste0(pm, sprintf("%1.1f", fat_val)), 
    #   subtitle = "Predicted change in cognitive function (z-score):", 
    #   width = 12, 
    #   color = bx_col
    # )
    outc_nm <- names(cog_outc_choices_alt)[cog_outc_choices_alt == get_cog_outc()]
    box(
      width = 12,
      p(strong(paste0("Predicted change in ", outc_nm)), style = "text-align: center;"),
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




