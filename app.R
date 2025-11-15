library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(yaml)

ui <- page_fluid(
  theme = bs_theme(brand = TRUE),
  tags$head(
    tags$style(HTML(
      "
      .well {
        background-color: #FFD500 !important;  /* ZOwonen geel */
      }
    "
    ))
  ),
  # Header with logo
  page_navbar(
    title = tags$img(
      src = "logo-black.svg",
      height = "30px"
    ),
    bg = "#FFD400",
    nav_panel(
      "Scenario-analyse: Cv-ketel vs. Warmtepomp",
      sidebarLayout(
        sidebarPanel(
          numericInput("n_woningen", "Aantal woningen:", value = 100, min = 1),
          # CV-ketel
          accordion(
            accordion_panel(
              title = "Parameters cv-ketel",
              sliderInput(
                "invest_cv",
                "Investeringskosten cv-ketel per woning (€):",
                min = 1500,
                max = 4000,
                value = 3000,
                step = 100
              ),
              sliderInput(
                "levensduur_cv",
                "Levensduur cv-ketel (jaren):",
                min = 10,
                max = 25,
                value = 15,
                step = 1
              ),
              sliderInput(
                "onderhoud_cv",
                "Onderhoudskosten cv-ketel per jaar (€):",
                min = 100,
                max = 500,
                value = 250,
                step = 25
              ),
              sliderInput(
                "verbruik_cv",
                "Gemiddeld gasverbruik per woning (m³/jaar):",
                min = 500,
                max = 2000,
                value = 1200,
                step = 50
              )
            ),
            hr(),

            # Warmtepomp
            accordion_panel(
              title = "Parameters warmtepomp",
              sliderInput(
                "invest_wp",
                "Investeringskosten warmtepomp per woning (€):",
                min = 5000,
                max = 20000,
                value = 8000,
                step = 500
              ),
              sliderInput(
                "levensduur_wp",
                "Levensduur warmtepomp (jaren):",
                min = 10,
                max = 30,
                value = 20,
                step = 1
              ),
              sliderInput(
                "onderhoud_wp",
                "Onderhoudskosten warmtepomp per jaar (€):",
                min = 100,
                max = 500,
                value = 200,
                step = 25
              ),
              sliderInput(
                "verbruik_wp",
                "Gemiddeld elektraverbruik per woning (kWh/jaar):",
                min = 1500,
                max = 4000,
                value = 2500,
                step = 100
              ),
              # Subsidie op warmtepomp
              numericInput(
                "subsidie",
                "Subsidie per woning (€):",
                value = 1500,
                min = 0
              ),
            ),
            hr(),

            # Energieprijzen
            accordion_panel(
              title = "Parameters energieprijzen",
              accordion(
                sliderInput(
                  "gasprijs",
                  "Gasprijs per m³ (€):",
                  min = 0.20,
                  max = 2.00,
                  value = 0.80,
                  step = 0.05
                ),
                sliderInput(
                  "elekprijs",
                  "Elektriciteitsprijs per kWh (€):",
                  min = 0.10,
                  max = 1.00,
                  value = 0.40,
                  step = 0.05
                ),
              )
            ),
            hr(),

            # Zonnepanelen
            checkboxInput(
              "zonnepanelen",
              "Zonnepanelen aanwezig?",
              value = TRUE
            ),
            numericInput(
              "opbrengst_zon",
              "Opbrengst zonnepanelen per woning (kWh/jaar):",
              value = 2500,
              min = 0
            ),
            hr(),

            # Tevredenheid
            accordion_panel(
              title = "Parameters tevredenheid",
              accordion(
                sliderInput(
                  "tevreden_cv",
                  "Tevredenheidscore cv-ketel (1-10):",
                  min = 1,
                  max = 10,
                  value = 6
                ),
                sliderInput(
                  "tevreden_wp",
                  "Tevredenheidscore warmtepomp (1-10):",
                  min = 1,
                  max = 10,
                  value = 8
                )
              ),
            )
          )
        ),

        mainPanel(
          h3("Resultaten"),
          tableOutput("result_table"),
          plotOutput("cost_plot"),
          plotOutput("payback_plot"),
          textOutput("summary")
        )
      )
    ),
    # nav_panel("About")
  ),
  # titlePanel("Scenario-analyse: Cv-ketel vs. Warmtepomp"),
)

server <- function(input, output) {
  calc_values <- reactive({
    gas_use <- input$verbruik_cv
    elec_use <- input$verbruik_wp

    # Energie
    gas_cost <- input$n_woningen * gas_use * input$gasprijs
    elec_cost <- input$n_woningen *
      (elec_use - ifelse(input$zonnepanelen, input$opbrengst_zon, 0)) *
      input$elekprijs

    # Onderhoud
    cv_maint <- input$n_woningen * input$onderhoud_cv
    wp_maint <- input$n_woningen * input$onderhoud_wp

    # Investeringen
    invest_cv_total <- input$n_woningen * input$invest_cv
    invest_wp_total <- input$n_woningen * (input$invest_wp - input$subsidie)

    # Afschrijving
    cv_afschr <- invest_cv_total / input$levensduur_cv
    wp_afschr <- invest_wp_total / input$levensduur_wp

    # Kosten splitsen
    cv_owner <- cv_maint + cv_afschr
    wp_owner <- wp_maint + wp_afschr

    cv_tenant <- gas_cost
    wp_tenant <- elec_cost

    # Totale exploitatiekosten
    cv_total <- cv_owner + cv_tenant
    wp_total <- wp_owner + wp_tenant

    invest_diff <- invest_wp_total - invest_cv_total
    annual_savings <- cv_total - wp_total
    payback <- ifelse(annual_savings > 0, invest_diff / annual_savings, NA)

    list(
      cv_owner = cv_owner,
      wp_owner = wp_owner,
      cv_tenant = cv_tenant,
      wp_tenant = wp_tenant,
      cv_total = cv_total,
      wp_total = wp_total,
      payback = payback
    )
  })

  output$result_table <- renderTable({
    vals <- calc_values()
    data.frame(
      Scenario = c("Cv-ketel", "Warmtepomp"),
      Kosten_eigenaar = c(
        scales::number(round(vals$cv_owner, 0), big.mark = " "),
        scales::number(round(vals$wp_owner, 0), big.mark = " ")
      ),
      Kosten_huurder = c(
        scales::number(round(vals$cv_tenant, 0), big.mark = " "),
        scales::number(round(vals$wp_tenant, 0), big.mark = " ")
      ),
      Totale_kosten = c(
        scales::number(round(vals$cv_total, 0), big.mark = " "),
        scales::number(round(vals$wp_total, 0), big.mark = " ")
      ),
      Tevredenheid = c(input$tevreden_cv, input$tevreden_wp),
      Terugverdientijd_jaren = c(
        NA,
        ifelse(is.na(vals$payback), NA, round(vals$payback, 1))
      )
    )
  })

  output$cost_plot <- renderPlot({
    vals <- calc_values()
    costs <- c(vals$cv_total, vals$wp_total)
    barplot(
      costs,
      names.arg = c("Cv-ketel", "Warmtepomp"),
      col = c("#E52050", "#27B757"),
      main = "Exploitatiekosten per jaar",
      ylab = "Kosten (€)"
    )
  })

  output$payback_plot <- renderPlot({
    # definities opnieuw opnemen
    gas_use <- 1200
    elec_use <- 3500

    gas_cost <- input$n_woningen * gas_use * input$gasprijs
    elec_cost_base <- input$n_woningen *
      (elec_use - ifelse(input$zonnepanelen, input$opbrengst_zon, 0))

    cv_maint <- input$n_woningen * input$onderhoud_cv
    wp_maint <- input$n_woningen * input$onderhoud_wp

    invest_cv_total <- input$n_woningen * input$invest_cv
    invest_wp_total <- input$n_woningen * (input$invest_wp - input$subsidie)

    cv_afschr <- invest_cv_total / input$levensduur_cv
    wp_afschr <- invest_wp_total / input$levensduur_wp

    cv_total <- gas_cost + cv_maint + cv_afschr
    invest_diff <- invest_wp_total - invest_cv_total

    # simulatie bij verschillende elektriciteitsprijzen
    elec_prices <- seq(0.2, 1.0, by = 0.1)
    paybacks <- sapply(elec_prices, function(p) {
      elec_cost <- elec_cost_base * p
      wp_total <- elec_cost + wp_maint + wp_afschr
      annual_savings <- cv_total - wp_total
      ifelse(annual_savings > 0, invest_diff / annual_savings, NA)
    })

    plot(
      elec_prices,
      paybacks,
      type = "b",
      lwd = 2,
      xlab = "Elektriciteitsprijs €/kWh",
      ylab = "Terugverdientijd (jaren)",
      main = "Scenario terugverdientijd"
    )
  })

  output$summary <- renderText({
    "Bij huidige aannames is de warmtepomp na X jaar rendabel en stijgt de tevredenheidsscore."
  })
}


shinyApp(ui = ui, server = server)
