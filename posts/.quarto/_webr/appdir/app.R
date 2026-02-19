library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(stringr)

ui <- page_sidebar(
  sidebar = sidebar(open = "open",
    numericInput("avg_stars", "Star Rating", 4,
                 min = 1, max = 5, step = 0.1),
    numericInput("num_reviews", "Number of reviews", 250,
                 min = 0, step = 1),
    numericInput("prior_stars", "Prior average rating (Bayesian average)", 3.5,
                 min = 1, max = 5, step = 0.1),
    numericInput("prior_num", "Prior number of reviews (Bayesian average)", 200),
    numericInput("alpha", "Alpha (empirical Bayes)", 3,
                 step = 0.1,
                 min = 0),
    numericInput("beta", "Beta (empirical Bayes)",1.5,
                 step = 0.1,
                 min = 0)
  ),
  navset_card_tab(
    nav_panel(title = "Comparison",
              plotOutput("comparison_plot")),
    nav_panel(title = "Binomial CI", 
              textOutput("binom_text")),
    nav_panel(title = "Laplace", 
              textOutput("laplace_text")),
    nav_panel(title = "Bayesian Average", 
              textOutput("bayes_avg_text")),
    nav_panel(title = "Empirical Bayes", 
              textOutput("beta_prior_text"),
              plotOutput("beta_dist"), br(),
              textOutput("beta_est"))
  )
)

server <- function(input, output, session) {
  
  binom_est <- reactive({
    p = (input$avg_stars - 1) / 4
    high_ci = 1+4*qbinom(p = 0.975,size = input$num_reviews,prob=p)/input$num_reviews
    low_ci = 1+4*qbinom(p = 0.025,size = input$num_reviews,prob=p)/input$num_reviews
    
    return(c(input$avg_stars,low_ci, high_ci))
  })
  
  output$binom_text <- renderText({
        str_c("Mean: ", binom_est()[1],", 95% CI: ",round(binom_est()[2],3),"-",round(binom_est()[3],3), " stars")
  })
  
  laplace_est <- reactive({
      total_stars = (input$avg_stars - 1) * input$num_reviews
      laplace_est = 1 + ((total_stars + 4) / (input$num_reviews + 2))
      return(laplace_est)
  })
  
  output$laplace_text <- renderText({
        str_c("Laplace estimate: ", round(laplace_est(),3), " stars")
  })
  
  bayes_avg <- reactive({
    bayes_est <- 1 + ((input$prior_num*(input$prior_stars-1))+ 
                        ((input$avg_stars-1) * input$num_reviews))/(input$prior_num + input$num_reviews)
    bayes_est
  })
  
  output$bayes_avg_text <- renderText({
    
    str_c("Bayesian average: ", round(bayes_avg(),3), " stars")
  })
  
  output$beta_prior_text <- renderText({
    alpha <- input$alpha
    beta <- input$beta
    
    beta_mean <- 1 + (4 * alpha / (alpha+beta))
    
    str_c("Beta prior mean: ", round(beta_mean,3), " stars")
  })
  
  output$beta_dist <- renderPlot({
    x <- seq(0, 1, length.out = 100)
    y <- dbeta(x, input$alpha, input$beta)
    
    rand_data = data.frame(points = 1 + (4*rbeta(1000, input$alpha, input$beta))) |>
      mutate(bin = case_when(points < 1.5 ~ 1,
                             points >= 1.5 & points < 2.5 ~ 2,
                             points >= 2.5 & points < 3.5 ~ 3,
                             points >= 3.5 & points < 4.5 ~ 4,
                             points >= 4.5~ 5)) |>
      summarize(.by = bin,
                n =n())
    
    data.frame(x = x, y = y) |>
      mutate(x = 1 + (x*4)) |>
      ggplot() +
      geom_line(aes(x = x, y = y)) +
      # geom_col(data = rand_data,
      #                aes(x = bin, y = n/1000),
      #                alpha = 0.5) + 
      labs(title = paste("Beta Distribution (alpha =", input$alpha, ", beta =", input$beta, ")"),
           x = "Star value", y = "Density")
  })
  
  beta_est <- reactive({
    adj_total_stars = (input$avg_stars - 1) * input$num_reviews

    estimate = 1 + ((adj_total_stars + input$alpha) / (input$num_reviews + input$alpha + input$beta))
    
    return(estimate)
  })
  output$beta_est <- renderText({
    str_c("Empirical Bayes estimate: ", round(beta_est(), 3), " stars")
  })
  
  output$comparison_plot <- renderPlot({
    data <- data.frame(method = factor(c("Binomial CI","Laplace","Bayesian Average","Empirical Bayes"),
                                         levels = c("Binomial CI","Laplace","Bayesian Average","Empirical Bayes")),
               value = c(binom_est()[1], laplace_est(), bayes_avg(), beta_est()),
               low_ci = c(binom_est()[2],NA,NA,NA),
               high_ci = c(binom_est()[3],NA,NA,NA)
               ) 
    data |>
      ggplot(aes(x = value, y = forcats::fct_rev(method))) +
      geom_errorbar(aes(xmin = low_ci, xmax = high_ci),
                    width = 0.2) +
      geom_point(color = "goldenrod") +
      theme_bw(base_size = 12) +
      labs(x = "Star Estimate",y = "Method") + 
      xlim(1,5)
  })
}

shinyApp(ui = ui, server = server)
