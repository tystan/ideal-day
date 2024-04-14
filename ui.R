
ui <- page(
  # img(src="p1.png"), # style="position:absolute;right:15px;z-index:1000000;"),
  layout_columns(
    col_widths = c(1, 10, 1),
    row_heights = c(5, 5, 5),
    div(img(src = "p1.png", height = 50, width = 50)),
    # layout_columns(
    #   col_widths = c(12, 12),
    div(
      p("", style = "size: 20"), # yuck hack apologies to anyone reading this
      h2("What does", strong("your 'ideal' day", style = "color: #78c2ad;"), "look like?", style = "text-align: center;")
    ),
    div(img(src = "Picture2.svg", height = 50, width = 50))
  ),
  theme = bs_theme(
    bootswatch = "minty",
    base_font = font_google("Inter")
  ),
  navset_card_pill(
    id = "pillset",
    placement = "above",
    # navset_pill(
    nav_panel(
      # id = "tab1",
      title = "Welcome", 
      card(p(
        "The aim of this app is to provide you with an idea of what the 'ideal' 24 hours looks like to maximise your brain health and cognitive function. To do this, we will begin by asking you some questions about yourself and your current health. Then, based on this information, we will show you what the 'ideal' 24-hours looks like for the best possible cognitive function."
      )), 
      layout_columns(col_widths = c(10, 2), p(), actionButton(inputId = "next1", label = "Next"))
    ),
    nav_panel(
      # id = "tab2",
      title = "Cognitive outcome", 
      cards[[1]], 
      layout_columns(
        col_widths = c(2, 8, 2), 
        actionButton(inputId = "prev2", label = "Prev"), 
        p(), 
        actionButton(inputId = "next2", label = "Next")
      )
      # layout_columns(cards[[1]], row_heights = 14, col_widths = 12),
    ),
    nav_panel(
      title = "Demographics", 
      cards[[2]], 
      layout_columns(
        col_widths = c(2, 8, 2), 
        actionButton(inputId = "prev3", label = "Prev"), 
        p(), 
        actionButton(inputId = "next3", label = "Next")
      )
    ),
    nav_panel(
      title = "Health", 
      cards[[3]], 
      layout_columns(
        col_widths = c(2, 8, 2), 
        actionButton(inputId = "prev4", label = "Prev"), 
        p(), 
        actionButton(inputId = "next4", label = "Next")
      )
    ),
    nav_panel(
      title = "Time-use", 
      cards[[4]],
      uiOutput("ui3"), 
      layout_columns(
        col_widths = c(2, 8, 2), 
        actionButton(inputId = "prev5", label = "Prev"), 
        p(), 
        actionButton(inputId = "next5", label = "Next")
      )
      #   success_cards[[1]], 
      #   success_cards[[2]]
      # )
    ),
    nav_panel(title = "Ideal day", 
      # p("On this page, we will show you what your current day looks like versus what the", strong("'ideal' day"), "looks like for your cognitive health. Firstly, we will need you to provide an indication of how many", strong("hours"), "you typically spend in each activity type (i.e., what does an average day look like for you?)"),
      # p(),
      # p("The current and 'ideal' days are displayed in the bar chart below, with four bars each: sleep, sitting, light-intensity physical activity, and moderate-vigorous physical activity. The grey bars (on the left for each activity) represent your current day, and the green bars (on the right for each activity) represent your 'ideal' day. "),
      uiOutput("ui0"), 
      layout_columns(
        col_widths = c(2, 8, 2), 
        actionButton(inputId = "prev6", label = "Prev"), 
        p(), 
        actionButton(inputId = "next6", label = "Next")
      )
    ),
    nav_panel(
      title = "Small steps", 
      cards[[5]], 
      layout_columns(
        col_widths = c(2, 10), 
        actionButton(inputId = "prev7", label = "Prev"), 
        p()
      )
    ),
    nav_spacer(),
    # nav_item(link_shiny),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(a("ARENA UniSA", href = "https://www.unisa.edu.au/research/arena/"))
    )
  )
  # uiOutput("Next_Previous")
  
)


