


library(shiny)
library(shinydashboard)
library(shinyjs)
library(bslib)
library(ggplot2)
library(plotly)
library(forcats)
library(dplyr)
library(lubridate)


# Learn more about bslib dashboards
# https://rstudio.github.io/bslib/articles/dashboards.html

use_plotly <- TRUE


ideal_day_vec <- c("Sleep" = 8, "Light PA" = 4, "Sit" = 11, "Mod-vig PA" = 1)


daytype_cols <-
  c(
    "My current day" = "#a6a6a6",
    "My 'ideal' day" = "#00b0f0"
  )

get_sgn_hex_col <- function(x) {
  ifelse(x < 0, "#ff3200", ifelse(x > 0, "#00a349", "#000000")) 
}
add_sgn <- function(x) {
  paste0(ifelse(x < 0, "", ifelse(x > 0, "+", "")), x)
}


h_to_hm <- function(hours_decimal) {
  h <- floor(hours_decimal)
  m <- round(60 * (hours_decimal - h), 0)
  return(sprintf("(%2.0fh %2.0fm)", h, m))
}






cog_outc_choices <- c(
  "Global cognition (my overall cognitive function)",
  "Memory",
  "Reasoning",
  "Processing speed",
  "Executive function"
)

demos_age_choices <- paste(c(
  # "<=45",
  # "46-55",
  # "56-65",
  # ">=66"
  "<65",
  "≥65"
), "years")

demos_sex_choices <- c(
  "Female",
  "Male",
  "Other"
  # "Prefer not to say"
)

demos_edu_choices <- c(
  "University/college degree",
  "Other professional qualification\n(e.g., nursing or teachers’ college)",
  "Certificate III, IV or diploma",
  "High school (≥ Year 10)",
  "Other/prefer not to say"
)


demos_hyp_choices <- c(
  "Yes",
  "No",
  "Unknown"
)

demos_t2d_choices <- c(
  "Yes",
  "No",
  "Unknown"
)

demos_dep_choices <- c(
  "Yes",
  "No",
  "Unknown"
)

demos_binj_choices <- c(
  "Yes",
  "No",
  "Unknown"
)

demos_hear_choices <- c(
  "Yes",
  "No",
  "Unknown"
)

demos_lone_choices <- c(
  "Yes",
  "No",
  "Unknown"
)

demos_alc_choices <- c(
  "3 or more times per week",
  "2 or less times per week\n(includes no alcohol)"
)

demos_smok_choices <- c(
  "I currently smoke" = 2,
  "I previously smoked, but not anymore" = 1,
  "I have never smoked" = 0
)



# success_cards <- 
#   list(
#     
#   )


cards <- list(
  card(
    full_screen = TRUE,
    fill = FALSE,
    card_header("Choose which cognitive outcome you’d like to focus on:"),
    radioButtons(inputId = "cog_outc", label = "", choices = cog_outc_choices, inline = FALSE, width = "100%")
  ),
  card(
    full_screen = TRUE,
    fill = FALSE,
    card_header("Tell us about yourself:"),
    layout_columns(
      col_widths = c(3, 3, 6),
      # radioButtons(inputId = "demos_age", label = "What is your current age?", choices = demos_age_choices),
      numericInput(inputId = "demos_age", label = "What is your current age (years)?", 
                   width = "100%", 
                   value = 70, step = 5, min = 30, max = 105),
      radioButtons(inputId = "demos_sex", label = "What is your sex?", 
                   choices = demos_sex_choices, 
                   selected = demos_sex_choices[demos_sex_choices == "Female"]),
      radioButtons(inputId = "demos_edu", label = "What is your highest qualification?", 
                   choices = demos_edu_choices, 
                   selected = demos_edu_choices[demos_edu_choices == "High school (≥ Year 10)"]),
    ),
    layout_columns(
      col_widths = c(3, 3, 6),
      numericInput(inputId = "demos_wei", label = "What is your current weight (kg)?", 
                   width = "100%", 
                   value = 80, step = 5, min = 30, max = 200),
      numericInput(inputId = "demos_wei", label = "What is your height (cm)?", 
                   width = "100%", 
                   value = 170, step = 5, min = 30, max = 230),
      p()
    ),
  ),
  card(
    full_screen = TRUE,
    card_header("Tell us about your health:"),
    layout_columns(
      radioButtons(inputId = "demos_hyp", label = "Have you been diagnosed with high blood pressure?", choices = demos_hyp_choices, selected = demos_hyp_choices[demos_hyp_choices == "No"]),
      radioButtons(inputId = "demos_t2d", label = "Have you been diagnosed with Type 2 Diabetes?", choices = demos_t2d_choices, selected = demos_t2d_choices[demos_t2d_choices == "No"]),
      radioButtons(inputId = "demos_dep", label = "Have you ever had a time where you felt depressed or down for at least one week?", choices = demos_dep_choices, selected = demos_dep_choices[demos_dep_choices == "No"])
    ),
    p(),
    layout_columns(
      radioButtons(inputId = "demos_cran", label = "Have you been diagnosed with a concussion or other traumatic brain injury?", choices = demos_binj_choices, selected = demos_binj_choices[demos_binj_choices == "No"]),
      radioButtons(inputId = "demos_hear", label = "Do you have any difficulty with your hearing?", choices = demos_hear_choices, selected = demos_hear_choices[demos_hear_choices == "No"]),
      radioButtons(inputId = "demos_lone", label = "In your life currently, do you often feel lonely or isolated?", choices = demos_lone_choices, selected = demos_lone_choices[demos_lone_choices == "No"])
    ),
    p(),
    layout_columns(
      radioButtons(inputId = "demos_alc", label = "How often do you consume alcohol?", choices = demos_alc_choices, selected = demos_alc_choices[length(demos_alc_choices)]),
      radioButtons(inputId = "demos_smok", label = "What is your history of tobacco smoking?", choices = demos_smok_choices, selected = demos_smok_choices[length(demos_smok_choices)])
    )
  ),
  card(
    full_screen = TRUE,
    card_header("On an average day, how much time do you spend in the following behaviours?"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      p("Sleep (hrs):"),
      p("Sitting (hrs):"),
      p("Light physical activity (hrs):"),
      p("Moderate-vigorous physical activity (hrs):"),
      numericInput(inputId = "tu_slp", label = "", 
                       width = "50%", 
                       value = 7.5, step = 0.5, min = 1, max = 23),
      numericInput(inputId = "tu_sit", label = "", 
                       width = "50%", 
                   value = 13, step = 0.5, min = 1, max = 23),
      numericInput(inputId = "tu_lpa", label = "", 
                       width = "50%", 
                       value = 2.8, step = 0.2, min = 1, max = 23),
      numericInput(inputId = "tu_vpa", label = "", 
                       width = "50%", 
                   value = 0.2, step = 0.2, min = 0, max = 12),
      uiOutput(outputId = "tu_slp_out"),
      uiOutput(outputId = "tu_sit_out"),
      uiOutput(outputId = "tu_lpa_out"),
      uiOutput(outputId = "tu_vpa_out")
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Using the interactive sliders below, visualise what happens to your cognitive function when you make small changes in your day"),
    layout_columns(
      col_widths  = c(7, 5),
      box(
        width = 12,
        sliderInput(
          "slide_slp", "Sleep (mins)", 
          value = 0, min = -60, max = 60, ticks = FALSE,
          width = "100%"
        ), # sliderInput.Sleep
        sliderInput(
          "slide_sed", "Sedentary behaviour (mins)", 
          value = 0, min = -60, max = 60, ticks = FALSE,
          width = "100%"
        ), # sliderInput.DomSoc
        sliderInput(
          "slide_lpa", "Light physical activity (mins)", 
          value = 0, min = -60, max = 60, ticks = FALSE,
          width = "100%"
        ), # sliderInput.PA
        sliderInput(
          "slide_vpa", "Moderate-vigorous physical activity (mins)", 
          value = 0, min = -60, max = 60, ticks = FALSE,
          width = "100%"
        )
      ),
      
      
      uiOutput("ui1")
      
    )
  )
)

