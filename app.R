library(shiny)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(RColorBrewer)

ui <- bootstrapPage(
  useShinyjs(),
  withMathJax(),
  navbarPage("Bayesian Re-Analysis of Clinical Trials", id = "tabs",
             
    tabPanel(title = "Study Data",
      fluidPage(
        fluidRow(
          column(width = 4,
            h4("Study Details"),
            hr(),
            prettyRadioButtons("est_type",
              "What type of study is being analyzed?",
              choices = list("Time to Event (HR)" = 1, 
                             "Dichotomous Outcome (OR)" = 2,
                             "Dichotomous Outcome (RR)" = 3),
              selected = 1),
            hr(),
            prettyRadioButtons("se_avail", 
              label = "Is the standard error of the point estimate available?",
              choices = list("Yes" = 1, "No" = 2), 
              inline = FALSE,
              selected = 2),
            hr()
          ),
          
          column(width = 4,
            conditionalPanel(
              condition = "input.est_type == 1",
              h4("Time to Event Data (HR)"),
              hr(),
              conditionalPanel(
                condition = "input.se_avail == 1",
                numericInput("se_hr", "Standard Error of HR", value = NA)
              ),
              conditionalPanel(
                condition = "input.se_avail == 2",
                numericInput("hr_pt_est", "Point Estimate (HR)", value = 0.9),
                numericInput("hr_lower_ci", "Lower Confidence Limit", value = 0.87),
                numericInput("hr_upper_ci", "Upper Confidence Limit", value = 1.04)
              )
            ),
            
            conditionalPanel(
              condition = "input.est_type != 1",
              h4("Dichotomous Outcome Data (OR/RR)"),
              hr(),
              conditionalPanel(
                condition = "input.se_avail == 1",
                numericInput("se_or_rr", "Standard Error of OR/RR", value = NA)
              ),
              conditionalPanel(
                condition = "input.se_avail == 2",
                numericInput("or_rr_pt_est", "Point Estimate (OR/RR)", value = 0.9),
                numericInput("or_rr_lower_ci", "Lower Confidence Limit", value = 0.87),
                numericInput("or_rr_upper_ci", "Upper Confidence Limit", value = 1.04)
              )
            )
          )
        )
      )
    ),

    tabPanel(title = "Distributions",
      fluidPage(
        sidebarPanel(width = 4,
          sliderInput("theta", "Prior Mean:", min = 0.1, max = 2, value = 1, step = 0.01),
          sliderInput("sd", "Prior SD:", min = 0.1, max = 1, value = 0.42, step = 0.01)
        ),
        mainPanel(width = 8,
          plotOutput("distPlot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  est_type <- reactive({ input$est_type })
  se_avail <- reactive({ input$se_avail })
  
  # Reactively retrieve the correct inputs for HR or OR/RR
  pt_est <- reactive({
    if (est_type() == 1) return(input$hr_pt_est)
    else return(input$or_rr_pt_est)
  })
  
  lower_ci <- reactive({
    if (est_type() == 1) return(input$hr_lower_ci)
    else return(input$or_rr_lower_ci)
  })
  
  upper_ci <- reactive({
    if (est_type() == 1) return(input$hr_upper_ci)
    else return(input$or_rr_upper_ci)
  })
  
  se_value <- reactive({
    if (est_type() == 1) return(input$se_hr)
    else return(input$se_or_rr)
  })

  # Compute likelihood standard deviation (SE) correctly
  likelihood_sd <- reactive({
    if (se_avail() == 1) {  # Use directly inputted SE
      return(se_value())
    } else {  # Compute SE from confidence interval
      return((log(upper_ci()) - log(lower_ci())) / (2 * 1.96))
    }
  })

  # Compute prior parameters
  prior_theta <- reactive({ log(input$theta) })
  prior_sd <- reactive({ input$sd })

  # Compute posterior parameters correctly
  post_theta <- reactive({
    ((prior_theta() / prior_sd()^2) + (log(pt_est()) / likelihood_sd()^2)) /
    ((1 / prior_sd()^2) + (1 / likelihood_sd()^2))
  })
  
  post_sd <- reactive({
    sqrt(1 / ((1 / prior_sd()^2) + (1 / likelihood_sd()^2)))
  })

  # Plot Distributions
  output$distPlot <- renderPlot({
    x <- seq(-5, 3, by = 0.01)
    prior_plot <- dnorm(x, prior_theta(), prior_sd())
    likelihood_plot <- dnorm(x, log(pt_est()), likelihood_sd())
    posterior_plot <- dnorm(x, post_theta(), post_sd())

    plot_data <- tibble(
      x = rep(x, 3),
      dist = rep(c("prior", "likelihood", "posterior"), each = length(x)),
      y = c(prior_plot, likelihood_plot, posterior_plot)
    ) %>% 
    mutate(x = exp(x))

    ggplot(plot_data, aes(x = x, y = y, group = dist, color = dist)) +
      geom_line(size = 1.1) +
      scale_x_continuous(trans = "log") +
      scale_color_manual(values = c("blue", "red", "green"), labels = c("Prior", "Likelihood", "Posterior")) +
      theme_classic() +
      labs(x = "Effect Estimate (HR, OR, RR)", y = "Density", color = "Distribution") +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)
