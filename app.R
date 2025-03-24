library(shiny)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(RColorBrewer)

ui <- bootstrapPage(
  useShinyjs(),
  withMathJax(),
  shinyUI(
    navbarPage("臨床試驗的貝葉斯重新分析", 
               id = "tabs",
               tabPanel(title = "首頁",
                        fluidPage(
                          fluidRow(
                            column(width = 12,
                                   h4("關於此應用程式："),
                                   uiOutput("link_twitter"),
                                   hr(),
                                   h5("步驟 1："),
                                   uiOutput("step_1"),
                                   h5("步驟 2："),
                                   uiOutput("step_2"),
                                   h5("步驟 3："),
                                   uiOutput("step_3"),
                                   hr(),
                                   uiOutput("link_email"),
                                   uiOutput("test"),
                                   br(),
                                   renderText(expr = output$paper_link)
                            )))),
               tabPanel(title = "研究數據", 
                        fluidPage(
                          fluidRow(
                            column(width = 4,
                                   h4("研究詳情"),
                                   hr(),
                                   prettyRadioButtons("est_type",
                                                      "正在分析哪種類型的研究？",
                                                      choices = list("事件時間 (HR)" = 1, 
                                                                     "二分結果 (OR)" = 2,
                                                                     "二分結果 (RR)" = 3,
                                                                     "連續結果 (平均值)" = 4),
                                                      selected = 1),
                                   hr(),
                                   prettyRadioButtons("se_avail", 
                                                      label = "點估計的標準誤差是否可用？",
                                                      choices = list("是" = 1, "否" = 2), 
                                                      inline = FALSE,
                                                      selected = 2),
                                   conditionalPanel(
                                     condition = "input.est_type == 1",
                                     hr(),
                                     prettyRadioButtons("rates_avail", 
                                                        label = "是否提供組別事件率？",
                                                        choices = list("是" = 1, "否" = 2), 
                                                        inline = FALSE,
                                                        selected = 2)),
                                   conditionalPanel(
                                     condition = "input.est_type == 4",
                                     hr(),
                                     prettyRadioButtons("cont_input_type",
                                                        label = "連續數據輸入類型：",
                                                        choices = list("樣本數、平均值、標準誤差" = 1,
                                                                       "樣本數、平均值、95%信賴區間" = 2),
                                                        selected = 1))),
                            column(width = 4,
                                   conditionalPanel(
                                     condition = "input.est_type == 1",
                                     h4("事件時間數據"),
                                     hr(),
                                     numericInput("pt_est", "HR點估計", value = 0.5),
                                     conditionalPanel(condition = "input.se_avail == 1",
                                                      numericInput("se_hr", "HR的標準誤差", value = NA)),
                                     numericInput("ci_width", "信賴區間寬度", value = 0.95, min = 0.9, max = 1, step = 0.01),
                                     numericInput("upper_ci", "信賴區間上限", value = 0.9),
                                     conditionalPanel(condition = "input.rates_avail == 1",
                                                      numericInput("hr_a", "干預組的事件數", value = 10, min = 0, step = 1),
                                                      numericInput("hr_b", "對照組的事件數", value = 30, min = 0, step = 1))),
                                   conditionalPanel(
                                     condition = "input.est_type != 1",
                                     conditionalPanel(condition = "input.est_type == 2", h4("二分結果數據 (OR)")),
                                     conditionalPanel(condition = "input.est_type == 3", h4("二分結果數據 (RR)")),
                                     conditionalPanel(condition = "input.est_type == 4", h4("連續結果數據")),
                                     hr(),
                                     conditionalPanel(condition = "input.se_avail == 1 && input.est_type != 4",
                                                      numericInput("se_or", "OR的標準誤差", value = NA)),
                                     conditionalPanel(condition = "input.est_type == 2 || input.est_type == 3",
                                                      numericInput("or_a", "干預組的事件數", value = 10, min = 0, step = 1),
                                                      numericInput("or_b", "對照組的事件數", value = 30, min = 0, step = 1),
                                                      numericInput("trt_n", "干預組樣本數", value = 50, min = 1, step = 1),
                                                      numericInput("ctrl_n", "對照組樣本數", value = 50, min = 1, step = 1)),
                                     conditionalPanel(condition = "input.est_type == 4",
                                                      numericInput("mean_int", "干預組平均值", value = 10, step = 0.1),
                                                      numericInput("mean_ctrl", "對照組平均值", value = 12, step = 0.1),
                                                      numericInput("n_int", "干預組樣本數", value = 50, min = 1, step = 1),
                                                      numericInput("n_ctrl", "對照組樣本數", value = 50, min = 1, step = 1),
                                                      conditionalPanel(condition = "input.cont_input_type == 1",
                                                                       numericInput("se_int", "干預組標準誤差", value = 0.5),
                                                                       numericInput("se_ctrl", "對照組標準誤差", value = 0.6)),
                                                      conditionalPanel(condition = "input.cont_input_type == 2",
                                                                       numericInput("ci_lower_int", "干預組95%信賴區間下限", value = 9),
                                                                       numericInput("ci_upper_int", "干預組95%信賴區間上限", value = 11),
                                                                       numericInput("ci_lower_ctrl", "對照組95%信賴區間下限", value = 11),
                                                                       numericInput("ci_upper_ctrl", "對照組95%信賴區間上限", value = 13)))),
                                   hr()),
                            column(width = 4,
                                   h4("技術說明"),
                                   hr(),
                                   conditionalPanel(condition = "input.est_type == 1", uiOutput("tech_notes_1")),
                                   conditionalPanel(condition = "input.est_type == 2", uiOutput("tech_notes_2")),
                                   conditionalPanel(condition = "input.est_type == 3", uiOutput("tech_notes_3")),
                                   conditionalPanel(condition = "input.est_type == 4", uiOutput("tech_notes_4")),
                                   br(),
                                   conditionalPanel(condition = "input.est_type == 1", uiOutput("eqn_1a"), uiOutput("eqn_2a")),
                                   conditionalPanel(condition = "input.est_type == 2", uiOutput("eqn_1b"), uiOutput("eqn_2b")),
                                   conditionalPanel(condition = "input.est_type == 3", uiOutput("eqn_1c")),
                                   conditionalPanel(condition = "input.est_type == 4", uiOutput("eqn_1d"), uiOutput("eqn_2d")),
                                   hr())))),
               tabPanel(title = "分佈",
                        fluidPage(
                          tags$style(HTML(".irs-bar {width: 100%; height: 5px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}")),
                          tags$style(HTML(".irs-bar-edge {background: black; border: 1px solid black; height: 5px; border-radius: 15px 15px 15px 15px;}")),
                          tags$style(HTML(".irs-line {border: 1px solid black; height: 5px;}")),
                          tags$style(HTML(".irs-grid-text {font-family: 'arial'; color: black}")),
                          tags$style(HTML(".irs-max {font-family: 'arial'; color: black;}")),
                          tags$style(HTML(".irs-min {font-family: 'arial'; color: black;}")),
                          tags$style(HTML(".irs-single {color:white; background:black;}")), 
                          sidebarPanel(width = 4,
                                       sliderInput(inputId = "theta", label = "先驗平均值：", min = 0.1, max = 2, value = 1, step = 0.01, ticks = FALSE),
                                       sliderInput(inputId = "hr", label = "計算先驗分佈寬度的關注值（例如MCID）：", min = 0.1, max = 2, value = 0.5, step = 0.01, ticks = FALSE),
                                       sliderInput(inputId = "pr", label = "先驗小於此值的概率：", min = 0, max = 1, value = 0.05, step = 0.01, ticks = FALSE),
                                       sliderInput(inputId = "sd", label = "先驗標準差：", min = 0.1, max = 1, value = 0.42, step = 0.01, ticks = FALSE),
                                       sliderInput(inputId = "ci", label = "後驗可信區間：", value = 89, min = 60, max = 99, step = 1, post = "%", ticks = FALSE),
                                       sliderInput(inputId = "hr_post", label = "後驗關注值：", min = 0.5, max = 1.25, value = 0.9, step = 0.01, ticks = FALSE),
                                       prettyRadioButtons(inputId = "post_alpha", label = "顯示後驗關注區域？", choices = list("是" = 1, "否" = 0), selected = 1, inline = TRUE, fill = FALSE, outline = FALSE, status = "primary"),
                                       prettyRadioButtons(inputId = "cred_alpha", label = "顯示後驗可信區間？", choices = list("是" = 1, "否" = 0), selected = 1, inline = TRUE, fill = FALSE, outline = FALSE, status = "primary"),
                                       hr(),
                                       h4("參數說明"),
                                       uiOutput("mcid_explanation"),
                                       uiOutput("pr_explanation"),
                                       uiOutput("sd_explanation")),
                          mainPanel(width = 8,
                                    plotOutput("distPlot"),
                                    hr(),
                                    h4("後驗預測檢查"),
                                    plotOutput("ppcPlot")))),
               tabPanel(title = "熱圖",
                        fluidPage(
                          fluidRow(column(12, h4("互動熱圖："), uiOutput("heat_text"), hr())),
                          sidebarPanel(sliderInput(inputId = "hr_heat", label = "後驗關注值：", min = 0.5, max = 1.25, value = 0.9, step = 0.01, ticks = FALSE)),
                          mainPanel(plotOutput("heatPlot")))))))

server <- function(input, output, session) {
  
  # Reactive values
  est_type <- reactive({input$est_type})
  rates_avail <- reactive({input$rates_avail})
  se_avail <- reactive({input$se_avail})
  cont_input_type <- reactive({input$cont_input_type})
  
  pt_est <- reactive({input$pt_est})
  upper_ci <- reactive({input$upper_ci})
  ci_width <- reactive({input$ci_width})
  likelihood_p <- reactive({(1 - ((1 - ci_width()) / 2))})
  
  hr_a <- reactive({input$hr_a})
  hr_b <- reactive({input$hr_b})
  
  se_hr <- reactive({input$se_hr})
  se_or <- reactive({input$se_or})
  
  or_a <- reactive({input$or_a})
  or_b <- reactive({input$or_b})
  trt_n <- reactive({input$trt_n})
  ctrl_n <- reactive({input$ctrl_n})
  or_c <- reactive({trt_n() - or_a()})
  or_d <- reactive({ctrl_n() - or_b()})
  
  mean_int <- reactive({input$mean_int})
  mean_ctrl <- reactive({input$mean_ctrl})
  n_int <- reactive({input$n_int})
  n_ctrl <- reactive({input$n_ctrl})
  se_int <- reactive({input$se_int})
  se_ctrl <- reactive({input$se_ctrl})
  ci_lower_int <- reactive({input$ci_lower_int})
  ci_upper_int <- reactive({input$ci_upper_int})
  ci_lower_ctrl <- reactive({input$ci_lower_ctrl})
  ci_upper_ctrl <- reactive({input$ci_upper_ctrl})
  
  theta_in <- reactive({input$theta})
  sd_in <- reactive({input$sd})
  hr_in <- reactive({input$hr})
  pr_in <- reactive({input$pr})
  
  prior_theta <- reactive({log(theta_in())})
  prior_sd <- reactive({sd_in()})

  # Observe events
  observeEvent(input$est_type, {
    toggleState(id = "upper_ci", condition = est_type() == 1)
    toggleState(id = "pt_est", condition = est_type() == 1)
    toggleState(id = "ci_width", condition = est_type() == 1)
    toggleState(id = "hr_a", condition = all(c(est_type() == 1, rates_avail() == 1)))
    toggleState(id = "hr_b", condition = all(c(est_type() == 1, rates_avail() == 1)))
    toggleState(id = "or_a", condition = est_type() %in% c(2, 3))
    toggleState(id = "or_b", condition = est_type() %in% c(2, 3))
    toggleState(id = "ctrl_n", condition = est_type() %in% c(2, 3))
    toggleState(id = "trt_n", condition = est_type() %in% c(2, 3))
    toggleState(id = "mean_int", condition = est_type() == 4)
    toggleState(id = "mean_ctrl", condition = est_type() == 4)
    toggleState(id = "n_int", condition = est_type() == 4)
    toggleState(id = "n_ctrl", condition = est_type() == 4)
  })
  
  observeEvent(input$rates_avail, {
    toggleState(id = "hr_a", condition = all(c(est_type() == 1, rates_avail() == 1)))
    toggleState(id = "hr_b", condition = all(c(est_type() == 1, rates_avail() == 1)))
  })
  
  observeEvent(input$cont_input_type, {
    toggleState(id = "se_int", condition = est_type() == 4 && cont_input_type() == 1)
    toggleState(id = "se_ctrl", condition = est_type() == 4 && cont_input_type() == 1)
    toggleState(id = "ci_lower_int", condition = est_type() == 4 && cont_input_type() == 2)
    toggleState(id = "ci_upper_int", condition = est_type() == 4 && cont_input_type() == 2)
    toggleState(id = "ci_lower_ctrl", condition = est_type() == 4 && cont_input_type() == 2)
    toggleState(id = "ci_upper_ctrl", condition = est_type() == 4 && cont_input_type() == 2)
  })
  
  # Estimate Type Labels
  short_lab <- reactive({
    if (est_type() == 1) "HR"
    else if (est_type() == 2) "OR"
    else if (est_type() == 3) "RR"
    else if (est_type() == 4) "RR"
  })
  
  long_lab <- reactive({
    if (est_type() == 1) "風險比"
    else if (est_type() == 2) "勝算比"
    else if (est_type() == 3) "風險比"
    else if (est_type() == 4) "反應比"
  })
  
  # Update sliders
  observeEvent(input$sd, {
    updateSliderInput(session, "pr", value = round(pnorm(log(hr_in()), log(theta_in()), sd_in()), 3))
  })
  observeEvent(input$hr, {
    updateSliderInput(session, "pr", value = round(pnorm(log(hr_in()), log(theta_in()), sd_in()), 3),
                      min = round(pnorm(log(hr_in()), log(theta_in()), 0.1), 3),
                      max = round(pnorm(log(hr_in()), log(theta_in()), 1), 3))
  })
  observeEvent(input$theta, {
    updateSliderInput(session, "pr", value = round(pnorm(log(hr_in()), log(theta_in()), sd_in()), 3),
                      min = round(pnorm(log(hr_in()), log(theta_in()), 0.1), 3),
                      max = round(pnorm(log(hr_in()), log(theta_in()), 1), 3))
  })
  observeEvent(input$pr, {
    updateSliderInput(session, "sd", value = round((log(hr_in()) - log(theta_in()))/qnorm(pr_in()), 3))
  })
  
  # Calculate Likelihood Parameters
  likelihood_theta <- reactive({
    if (est_type() == 1) log(pt_est())
    else if (est_type() == 2) log(((or_a() + 0.5) * (or_d() + 0.5)) / ((or_b() + 0.5) * (or_c() + 0.5)))
    else if (est_type() == 3) log((or_a() / (or_a() + or_c())) / (or_b() / (or_b() + or_d())))
    else if (est_type() == 4) log(mean_int() / mean_ctrl())
  })
  
  likelihood_sd <- reactive({
    if (est_type() == 1) {
      if (rates_avail() == 1) sqrt((1 / hr_a()) + (1 / hr_b()))
      else (log(upper_ci()) - log(pt_est())) / qnorm(likelihood_p())
    } else if (est_type() == 2) {
      sqrt(((1 / (or_a() + 0.5)) + (1 / (or_b() + 0.5)) + (1 / (or_c() + 0.5)) + (1 / (or_d() + 0.5))))
    } else if (est_type() == 3) {
      sqrt((or_c() / (or_a() * (or_a() + or_c()))) + (or_d() / (or_b() * (or_b() + or_d()))))
    } else if (est_type() == 4) {
      if (cont_input_type() == 1) sqrt((se_int()^2 / n_int()) + (se_ctrl()^2 / n_ctrl()))
      else {
        se_int_est <- (ci_upper_int() - ci_lower_int()) / (2 * 1.96)
        se_ctrl_est <- (ci_upper_ctrl() - ci_lower_ctrl()) / (2 * 1.96)
        sqrt((se_int_est^2 / n_int()) + (se_ctrl_est^2 / n_ctrl()))
      }
    }
  })
  
  # Posterior Parameters
  post_theta <- reactive({
    ((prior_theta() / (prior_sd())^2) + (likelihood_theta() / likelihood_sd()^2)) / 
      ((1 / (prior_sd())^2) + (1 / likelihood_sd()^2))
  })
  
  post_sd <- reactive({
    sqrt(1 / ((1 / (prior_sd())^2) + (1 / likelihood_sd()^2)))
  })
  
  # Posterior Predictive Checks
  ppc_data <- reactive({
    n_sim <- 1000
    post_samples <- rnorm(n_sim, post_theta(), post_sd())
    
    if (est_type() == 1) {
      if (rates_avail() == 1) {
        sim_hr_a <- rpois(n_sim, hr_a() * exp(post_samples))
        sim_hr_b <- rpois(n_sim, hr_b())
        tibble(sim_stat = sim_hr_a / sim_hr_b, observed = hr_a() / hr_b())
      } else {
        tibble(sim_stat = exp(post_samples), observed = pt_est())
      }
    } else if (est_type() == 2) {
      sim_or_a <- rbinom(n_sim, trt_n(), plogis(post_samples))
      sim_or_b <- rbinom(n_sim, ctrl_n(), plogis(post_samples))
      sim_or_c <- trt_n() - sim_or_a
      sim_or_d <- ctrl_n() - sim_or_b
      tibble(sim_stat = (sim_or_a * sim_or_d) / (sim_or_b * sim_or_c), observed = (or_a() * or_d()) / (or_b() * or_c()))
    } else if (est_type() == 3) {
      sim_or_a <- rbinom(n_sim, trt_n(), plogis(post_samples))
      sim_or_b <- rbinom(n_sim, ctrl_n(), plogis(post_samples))
      sim_or_c <- trt_n() - sim_or_a
      sim_or_d <- ctrl_n() - sim_or_b
      tibble(sim_stat = (sim_or_a / (sim_or_a + sim_or_c)) / (sim_or_b / (sim_or_b + sim_or_d)), 
             observed = (or_a() / (or_a() + or_c())) / (or_b() / (or_b() + or_d())))
    } else if (est_type() == 4) {
      sim_mean_int <- rnorm(n_sim, mean_int() * exp(post_samples), se_int())
      sim_mean_ctrl <- rnorm(n_sim, mean_ctrl(), se_ctrl())
      tibble(sim_stat = sim_mean_int / sim_mean_ctrl, observed = mean_int() / mean_ctrl())
    }
  })
  
  # PPC Plot
  output$ppcPlot <- renderPlot({
    ppc_data() %>%
      ggplot(aes(x = sim_stat)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "grey70", color = "black") +
      geom_vline(aes(xintercept = observed), color = "red", linewidth = 1.2, linetype = "dashed") +
      labs(title = "後驗預測分佈與觀測值比較",
           x = paste("模擬", short_lab()), y = "密度") +
      theme_minimal() +
      theme(text = element_text(family = "Gill Sans MT"),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 12))
  }, height = 300)
  
  # Plot data
  x <- seq(-4, 2, by = 0.01) # Adjusted range to avoid extreme exp values
  prior_plot <- reactive({dnorm(x, prior_theta(), prior_sd())})
  likelihood_plot <- reactive({dnorm(x, likelihood_theta(), likelihood_sd())})
  posterior_plot <- reactive({dnorm(x, post_theta(), post_sd())})
  
  plot_data <- reactive({
    tibble(x = rep(x, 3)) %>%
      mutate(dist = rep(c("prior", "likelihood", "posterior"), each = nrow(.) / 3),
             y = c(prior_plot(), likelihood_plot(), posterior_plot())) %>%
      mutate(x = exp(x), 
             y = ifelse(is.infinite(exp(y)) | is.na(exp(y)), 0, exp(y))) # Handle Inf/NA
  })
  
  max_x <- reactive({max(exp(likelihood_theta() + 1.5), 2)})
  
  ci_in <- reactive({input$ci})
  lower_cred <- reactive({round(exp(qnorm((1 - (ci_in()/100)) / 2, post_theta(), post_sd())), 2)})
  upper_cred <- reactive({round(exp(qnorm(1 - (1 - (ci_in()/100)) / 2, post_theta(), post_sd())), 2)})
  mid_cred <- reactive({round(exp(qnorm(0.5, post_theta(), post_sd())), 2)})
  
  hr_post <- reactive({input$hr_post})
  like_col <- brewer.pal(3, "Dark2")[1]
  post_col <- brewer.pal(3, "Dark2")[2]
  prior_col <- brewer.pal(3, "Dark2")[3]
  post_alpha <- reactive({as.numeric(input$post_alpha)})
  cred_alpha <- reactive({as.numeric(input$cred_alpha)})
  
  output$distPlot <- renderPlot({
    plot_data() %>%
      ggplot(aes(x = x, y = y, group = dist)) + 
      geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", alpha = 0.75, linewidth = 0.75) +
      geom_line(aes(color = dist), linewidth = 1.1) +
      geom_ribbon(data = plot_data() %>% filter(dist == "posterior", x < hr_post()),
                  aes(ymin = 1, ymax = y, x = x), alpha = post_alpha() * 0.5, fill = post_col) + 
      geom_vline(xintercept = hr_post(), color = post_col, linewidth = 0.75, linetype = "dashed", alpha = post_alpha() * 0.75) +
      geom_segment(aes(y = 1, yend = 1, x = lower_cred(), xend = upper_cred()), linewidth = 1.5, alpha = cred_alpha() * 1) +
      geom_point(aes(x = mid_cred(), y = 1), size = 2.5, alpha = cred_alpha() * 1) + 
      scale_color_brewer(name = NULL, type = "qual", palette = "Dark2",
                         breaks = c("likelihood", "prior", "posterior"),
                         labels = c("似然", "先驗", "後驗")) + 
      xlim(0, max_x()) + 
      labs(x = long_lab(), y = "概率密度") + 
      annotate(geom = "text", label = paste("後驗概率 ", short_lab(), " < 1: ", round(pnorm(log(1), post_theta(), post_sd(), lower.tail = TRUE), 3)), 
               x = max_x(), y = max(plot_data()$y), hjust = 1, fontface = "bold") + 
      annotate(geom = "text", label = paste("後驗概率 ", short_lab(), " < ", hr_post(), ": ", round(pnorm(log(hr_post()), post_theta(), post_sd(), lower.tail = TRUE), 3)), 
               x = max_x(), y = max(plot_data()$y) - max(plot_data()$y/25), hjust = 1, fontface = "bold") + 
      annotate(geom = "text", label = paste("後驗中位數 (", ci_in(), "% 可信區間): ", mid_cred(), " (", lower_cred(), ", ", upper_cred(), ")"), 
               x = max_x(), y = max(plot_data()$y) - (2 * max(plot_data()$y)/25), hjust = 1, fontface = "bold") + 
      theme_classic() + 
      theme(legend.position = "bottom", text = element_text(family = "Gill Sans MT"), 
            axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
            axis.title = element_text(size = 15), axis.text = element_text(size = 12), legend.text = element_text(size = 15))
  }, height = 620)
  
  # Heatmap
  hr_heat <- reactive({input$hr_heat})
  theta_list <- seq(from = 0.5, to = 1.5, by = 0.01)
  sd_list <- seq(from = 0.1, to = 0.8, length = length(theta_list))
  
  heat_data <- reactive({
    tibble(prior_theta = rep(theta_list, each = length(theta_list)),
           prior_sd = rep(sd_list, times = length(sd_list))) %>%
      mutate(post_theta = ((log(prior_theta) / (prior_sd)^2) + (likelihood_theta() / likelihood_sd()^2)) / ((1 / (prior_sd)^2) + (1 / likelihood_sd()^2)),
             post_sd = sqrt(1 / ((1 / (prior_sd)^2) + (1 / likelihood_sd()^2))),
             p_hr = pnorm(log(hr_heat()), post_theta, post_sd, lower.tail = TRUE))
  })
  
  output$heatPlot <- renderPlot({
    heat_data() %>%
      ggplot(aes(x = prior_theta, y = prior_sd)) + 
      geom_tile(aes(fill = p_hr)) + 
      scale_fill_viridis_c(name = paste("後驗概率 ", short_lab(), " < ", hr_heat()), begin = min(heat_data()$p_hr), end = max(heat_data()$p_hr)) + 
      labs(x = "先驗平均值", y = "先驗標準差") + 
      theme_classic() + 
      theme(text = element_text(family = "Gill Sans MT"), axis.title = element_text(size = 15), 
            axis.text = element_text(size = 12), legend.text = element_text(size = 12), legend.title = element_text(size = 14), legend.position = "right")
  }, width = 750, height = 550)
  
  # UI Outputs
  output$link_twitter <- renderUI({tagList("這是一個用於臨床試驗貝葉斯重新分析的互動工具。由Dan Lane ", a("(@DanLane911)", href="https://twitter.com/DanLane911"), "和Ben Andrew ", a("(@BenYAndrew)", href="https://twitter.com/BenYAndrew"), "編寫程式碼，方法改編自 ", a("Wijeysundera et al.", href="https://www.ncbi.nlm.nih.gov/pubmed/18947971"), "。此版本由顧進裕修改。")})
  output$step_1 <- renderUI({tagList("在「研究數據」標籤中輸入臨床試驗的基本結果。這將允許近似似然分佈。")})
  output$step_2 <- renderUI({tagList("在「分佈」標籤中使用滑桿動態調整先驗分佈，通過選擇平均值和(1)標準差或(2)感興趣的結果水平及該水平以下的先驗概率質量。")})
  output$step_3 <- renderUI({tagList("在「熱圖」標籤中，使用滑桿選擇感興趣的結果水平，以視覺化後驗概率在先驗平均值和標準差的不同組合下低於該水平的可能性。")})
  output$tech_notes_1 <- renderUI({withMathJax(paste0("似然分佈構建為正態分佈：$$L \\sim N(\\theta, s)$$, 其中$$\\theta = log(HR)$$, $$s = \\widehat{SE}_{log(HR)}$$，標準誤差使用下方方程估計。"))})
  output$tech_notes_2 <- renderUI({withMathJax(paste0("似然分佈構建為正態分佈：$$L \\sim N(\\theta, s)$$, 其中$$\\theta = log\\frac{(a + \\frac{1}{2})(d + \\frac{1}{2})}{(b + \\frac{1}{2})(c + \\frac{1}{2})}$$, $$s = \\widehat{SE}_{log(OR)}$$，標準誤差使用下方方程估計。"))})
  output$tech_notes_3 <- renderUI({withMathJax(paste0("似然分佈構建為正態分佈：$$L \\sim N(\\theta, s)$$, 其中$$\\theta = log\\frac{\\frac{a}{a + c}}{\\frac{b}{b + d}}$$, $$s = \\widehat{SE}_{log(RR)}$$，標準誤差使用下方方程估計。"))})
  output$tech_notes_4 <- renderUI({withMathJax(paste0("似然分佈構建為正態分佈：$$L \\sim N(\\theta, s)$$, 其中$$\\theta = log(\\frac{\\mu_{int}}{\\mu_{ctrl}})$$, $$s = \\widehat{SE}_{log(RR)}$$，標準誤差從標準誤差或信賴區間估計。"))})
  output$eqn_1a <- renderUI({withMathJax("標準誤差方程1：$$s = \\sqrt{\\frac{1}{E_1} + \\frac{1}{E_2}}$$")})
  output$eqn_2a <- renderUI({withMathJax("標準誤差方程2：$$s = \\frac{log(UCI) - log(HR)}{qnorm(p)}$$")})
  output$eqn_1b <- renderUI({withMathJax("標準誤差方程1：$$s = \\sqrt{\\frac{1}{a + \\frac{1}{2}} + \\frac{1}{b + \\frac{1}{2}} + \\frac{1}{c + \\frac{1}{2}} + \\frac{1}{d + \\frac{1}{2}}}$$")})
  output$eqn_2b <- renderUI({withMathJax("標準誤差方程2：$$s = \\sqrt{\\frac{1}{a} + \\frac{1}{b} + \\frac{1}{c} + \\frac{1}{d}}$$")})
  output$eqn_1c <- renderUI({withMathJax("標準誤差方程1：$$s = \\sqrt{\\frac{c}{a(a + c)} + \\frac{d}{b(b + d)}}$$")})
  output$eqn_1d <- renderUI({withMathJax("標準誤差方程1：$$s = \\sqrt{\\frac{se_{int}^2}{n_{int}} + \\frac{se_{ctrl}^2}{n_{ctrl}}}$$")})
  output$eqn_2d <- renderUI({withMathJax("標準誤差方程2：$$s = \\sqrt{\\frac{(\\frac{UCI_{int} - LCI_{int}}{2 \\times 1.96})^2}{n_{int}} + \\frac{(\\frac{UCI_{ctrl} - LCI_{ctrl}}{2 \\times 1.96})^2}{n_{ctrl}}}$$")})
  output$mcid_explanation <- renderUI({tagList(h5("關注值（例如MCID）說明："), p("此滑桿設定一個您感興趣的特定效果大小（如風險比HR），通常是臨床研究中的「最小臨床重要差異」(MCID)。"))})
  output$pr_explanation <- renderUI({tagList(h5("先驗小於此值的概率說明："), p("此滑桿指定先驗分佈小於「關注值」的概率，量化您對效果大小低於某閾值的信念。"))})
  output$sd_explanation <- renderUI({tagList(h5("先驗標準差內定0.42說明："), p("預設為0.42，基於先驗平均值1、關注值 MCID 0.5和 MCID <0.5 的先驗機率 <0.05（亦即先驗機率有 90% 的機率介於 1/2 至 2）計算得出，提供適度信息量的先驗。但是要注意的是實務上的 HR/OR/RR/反應比是介於 0.8-1.2，此外觀測值愈接近後驗預測分布的中心表示統計模型愈可靠。"))})
}

shinyApp(ui = ui, server = server)
