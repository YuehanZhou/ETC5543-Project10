library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)

# Read combined development and biodiversity data
explorer_data <- read_csv(
  "data/processed/priority_explorer_data.csv",
  show_col_types = FALSE
)

# Read PSP boundaries
psp_boundary <- st_read(
  "data/PSP_Boundary/PSP_Boundary.geojson",
  quiet = TRUE
)

# Read LGA boundaries
lga_boundary <- st_read(
  "data/LGA_2025_AUST_GDA2020/LGA_2025_AUST_GDA2020.shp",
  quiet = TRUE
)

# Keep the five study LGAs
study_lga <- lga_boundary |>
  filter(
    LGA_NAME25 %in% c(
      "Casey",
      "Hume",
      "Melton",
      "Whittlesea",
      "Wyndham"
    )
  ) |>
  st_make_valid()

# Standardise PSP names
psp_boundary <- psp_boundary |>
  mutate(
    PSP_NAME = case_when(
      PSP_NAME == "Quandong PSP (Council-led)" ~ "Quandong PSP",
      PSP_NAME == "Croskell" ~ "Croskell (Casey Central)",
      TRUE ~ PSP_NAME
    )
  ) |>
  st_make_valid()

# Transform LGAs to match PSP coordinate system
study_lga_psp_crs <- st_transform(
  study_lga,
  st_crs(psp_boundary)
)

# Create combined five-LGA study area
study_area <- st_union(
  study_lga_psp_crs
)

# Join analysis data to PSP polygons
explorer_map <- psp_boundary |>
  left_join(
    explorer_data,
    by = c("PSP_NAME" = "psp_name")
  ) |>
  filter(!is.na(growth_ha))

# Clip precinct polygons to five-LGA study area
explorer_map <- st_intersection(
  explorer_map,
  study_area
) |>
  st_make_valid()

# Convert precincts to latitude/longitude for Leaflet
explorer_map <- st_transform(
  explorer_map,
  4326
)

# Convert LGA boundaries to latitude/longitude
study_lga_ll <- st_transform(
  study_lga,
  4326
)

# Identify which study LGA each precinct intersects
lga_matches <- st_intersects(
  explorer_map,
  study_lga_ll
)

explorer_map$study_lga <- vapply(
  lga_matches,
  function(x) {
    
    if (length(x) == 0) {
      return("Study area")
    }
    
    paste(
      study_lga_ll$LGA_NAME25[x],
      collapse = ", "
    )
  },
  character(1)
)

# Create standardised component scores
explorer_map <- explorer_map |>
  mutate(
    
    development_score =
      (
        percent_rank(growth_ha) +
          percent_rank(urban_share_change)
      ) / 2,
    
    biodiversity_score =
      percent_rank(high_habitat_percent),
    
    conservation_gap_score =
      percent_rank(high_habitat_outside_bcs_ha)
  )

# Full study-area extent
study_bbox <- st_bbox(
  explorer_map
)

ui <- fluidPage(
  
  titlePanel(
    "Melbourne Growth & Biodiversity Priority Explorer"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Study area"),
      
      p(
        "Casey, Hume, Melton, Whittlesea and Wyndham"
      ),
      
      hr(),
      
      h4("Find a precinct"),
      
      selectizeInput(
        "precinct_search",
        "Search precinct",
        choices = sort(
          unique(explorer_map$PSP_NAME)
        ),
        selected = NULL,
        options = list(
          placeholder = "Type a precinct name..."
        )
      ),
      
      actionButton(
        "reset_map",
        "Reset map"
      ),
      
      hr(),
      
      h4("Focal species"),
      
      selectInput(
        "species_filter",
        "Show precincts",
        choices = c(
          "All precincts",
          "Golden Sun Moth only",
          "Growling Grass Frog only",
          "Both species"
        ),
        selected = "All precincts"
      ),
      
      p(
        "Filter precincts by recent focal-species records. 'Only' excludes precincts where both species were recorded."
      ),
      
      hr(),
      
      h4("What should matter most?"),
      
      p(
        "Choose a scenario or adjust the sliders manually."
      ),
      
      fluidRow(
        
        column(
          width = 4,
          actionButton(
            "balanced",
            "Balanced",
            width = "100%"
          )
        ),
        
        column(
          width = 4,
          actionButton(
            "development_focus",
            "Development",
            width = "100%"
          )
        ),
        
        column(
          width = 4,
          actionButton(
            "conservation_focus",
            "Conservation",
            width = "100%"
          )
        )
      ),
      
      br(),
      
      sliderInput(
        "development_weight",
        "Development pressure",
        min = 0,
        max = 100,
        value = 40
      ),
      
      sliderInput(
        "biodiversity_weight",
        "Biodiversity importance",
        min = 0,
        max = 100,
        value = 30
      ),
      
      sliderInput(
        "gap_weight",
        "Conservation gap",
        min = 0,
        max = 100,
        value = 30
      ),
      
      uiOutput(
        "weight_summary"
      ),
      
      hr(),
      
      p(
        "Higher slider values give that factor more influence in the priority ranking."
      ),
      
      p(
        "Changing the weights changes the ranking only. It does not change the underlying data."
      )
    ),
    
    mainPanel(
      
      leafletOutput(
        "priority_map",
        height = 550
      ),
      
      br(),
      
      uiOutput(
        "selected_precinct_card"
      ),
      
      br(),
      
      h3(
        "Top priority precincts"
      ),
      
      DTOutput(
        "priority_table"
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Store currently selected precinct
  selected_precinct_name <- reactiveVal(NULL)
  
  # Balanced scenario
  observeEvent(
    input$balanced,
    {
      
      updateSliderInput(
        session,
        "development_weight",
        value = 33
      )
      
      updateSliderInput(
        session,
        "biodiversity_weight",
        value = 33
      )
      
      updateSliderInput(
        session,
        "gap_weight",
        value = 33
      )
    }
  )
  
  # Development-focused scenario
  observeEvent(
    input$development_focus,
    {
      
      updateSliderInput(
        session,
        "development_weight",
        value = 50
      )
      
      updateSliderInput(
        session,
        "biodiversity_weight",
        value = 25
      )
      
      updateSliderInput(
        session,
        "gap_weight",
        value = 25
      )
    }
  )
  
  # Conservation-focused scenario
  observeEvent(
    input$conservation_focus,
    {
      
      updateSliderInput(
        session,
        "development_weight",
        value = 20
      )
      
      updateSliderInput(
        session,
        "biodiversity_weight",
        value = 40
      )
      
      updateSliderInput(
        session,
        "gap_weight",
        value = 40
      )
    }
  )
  
  # Show effective weighting
  output$weight_summary <- renderUI({
    
    total_weight <-
      input$development_weight +
      input$biodiversity_weight +
      input$gap_weight
    
    if (total_weight == 0) {
      
      return(
        p(
          strong("Current weighting: "),
          "all factors are set to zero."
        )
      )
    }
    
    development_pct <-
      input$development_weight /
      total_weight * 100
    
    biodiversity_pct <-
      input$biodiversity_weight /
      total_weight * 100
    
    gap_pct <-
      input$gap_weight /
      total_weight * 100
    
    tagList(
      
      strong(
        "Current weighting"
      ),
      
      br(),
      
      paste0(
        "Development: ",
        round(development_pct),
        "%"
      ),
      
      br(),
      
      paste0(
        "Biodiversity: ",
        round(biodiversity_pct),
        "%"
      ),
      
      br(),
      
      paste0(
        "Conservation gap: ",
        round(gap_pct),
        "%"
      )
    )
  })
  
  # Calculate dynamic priority score
  scored_data <- reactive({
    
    total_weight <-
      input$development_weight +
      input$biodiversity_weight +
      input$gap_weight
    
    if (total_weight == 0) {
      total_weight <- 1
    }
    
    explorer_map |>
      mutate(
        
        priority_score =
          100 * (
            development_score *
              input$development_weight +
              
              biodiversity_score *
              input$biodiversity_weight +
              
              conservation_gap_score *
              input$gap_weight
          ) / total_weight
      )
  })
  
  # Filter displayed precincts by focal species
  display_data <- reactive({
    
    data <- scored_data()
    
    if (
      input$species_filter ==
      "Golden Sun Moth only"
    ) {
      
      data <- data |>
        filter(
          golden_sun_moth_records > 0,
          growling_grass_frog_records == 0
        )
      
    } else if (
      input$species_filter ==
      "Growling Grass Frog only"
    ) {
      
      data <- data |>
        filter(
          growling_grass_frog_records > 0,
          golden_sun_moth_records == 0
        )
      
    } else if (
      input$species_filter ==
      "Both species"
    ) {
      
      data <- data |>
        filter(
          golden_sun_moth_records > 0,
          growling_grass_frog_records > 0
        )
    }
    
    data
  })
  
  # Reset search when species filter changes
  observeEvent(
    input$species_filter,
    {
      
      filtered_precincts <- display_data()
      
      selected_precinct_name(NULL)
      
      updateSelectizeInput(
        session,
        "precinct_search",
        
        choices = sort(
          unique(
            filtered_precincts$PSP_NAME
          )
        ),
        
        selected = character(0)
      )
      
      leafletProxy(
        "priority_map"
      ) |>
        clearGroup(
          "selected_precinct"
        )
    },
    ignoreInit = TRUE
  )
  
  # Interactive priority map
  output$priority_map <- renderLeaflet({
    
    map_data <- display_data()
    
    req(
      nrow(map_data) > 0
    )
    
    priority_pal <- colorNumeric(
      palette = "YlOrRd",
      domain = scored_data()$priority_score,
      na.color = "transparent"
    )
    
    map_data <- map_data |>
      mutate(
        
        popup_text = paste0(
          
          "<strong>",
          PSP_NAME,
          "</strong><br><br>",
          
          "<strong>Study LGA:</strong> ",
          study_lga,
          "<br><br>",
          
          "<strong>Priority score:</strong> ",
          round(priority_score, 1),
          "/100<br><br>",
          
          "<strong>Development</strong><br>",
          
          "Urban growth: ",
          round(growth_ha, 1),
          " ha<br>",
          
          "Urban share change: ",
          round(urban_share_change, 1),
          " percentage points<br><br>",
          
          "<strong>Biodiversity</strong><br>",
          
          "High-value habitat: ",
          round(high_habitat_percent, 1),
          "%<br>",
          
          "High-value habitat outside BCS: ",
          round(
            high_habitat_outside_bcs_ha,
            1
          ),
          " ha<br>",
          
          "High-value habitat outside BCS: ",
          round(
            high_habitat_outside_bcs_percent,
            1
          ),
          "%<br><br>",
          
          "<strong>Focal species</strong><br>",
          
          "Golden Sun Moth records: ",
          golden_sun_moth_records,
          "<br>",
          
          "Growling Grass Frog records: ",
          growling_grass_frog_records,
          "<br>",
          
          "Species category: ",
          species_group
        )
      )
    
    leaflet(
      map_data,
      options = leafletOptions(
        preferCanvas = TRUE
      )
    ) |>
      
      addProviderTiles(
        providers$Esri.WorldGrayCanvas
      ) |>
      
      addPolygons(
        
        layerId = ~PSP_NAME,
        
        color = "white",
        weight = 1,
        
        fillColor =
          ~priority_pal(
            priority_score
          ),
        
        fillOpacity = 0.8,
        
        popup =
          ~popup_text,
        
        label =
          ~paste0(
            PSP_NAME,
            " | ",
            study_lga,
            " | Score: ",
            round(
              priority_score,
              1
            )
          ),
        
        highlightOptions =
          highlightOptions(
            weight = 3,
            color = "black",
            fillOpacity = 0.95,
            bringToFront = TRUE
          )
      ) |>
      
      # Draw five study LGA boundaries
      addPolylines(
        data = study_lga_ll,
        color = "black",
        weight = 2,
        opacity = 0.8,
        label = ~LGA_NAME25
      ) |>
      
      addLegend(
        position = "bottomright",
        pal = priority_pal,
        values = scored_data()$priority_score,
        title = "Priority score"
      ) |>
      
      fitBounds(
        
        lng1 = as.numeric(
          study_bbox["xmin"]
        ),
        
        lat1 = as.numeric(
          study_bbox["ymin"]
        ),
        
        lng2 = as.numeric(
          study_bbox["xmax"]
        ),
        
        lat2 = as.numeric(
          study_bbox["ymax"]
        )
      )
  })
  
  # Click directly on map to select precinct
  observeEvent(
    input$priority_map_shape_click,
    {
      
      click <- input$priority_map_shape_click
      
      req(
        click$id
      )
      
      selected_precinct_name(
        click$id
      )
      
      updateSelectizeInput(
        session,
        "precinct_search",
        selected = click$id
      )
    }
  )
  
  # Search and zoom to selected precinct
  observeEvent(
    input$precinct_search,
    {
      
      req(
        input$precinct_search
      )
      
      selected_precinct_name(
        input$precinct_search
      )
      
      selected_precinct <-
        display_data() |>
        filter(
          PSP_NAME ==
            input$precinct_search
        )
      
      req(
        nrow(selected_precinct) > 0
      )
      
      selected_bbox <-
        st_bbox(
          selected_precinct
        )
      
      leafletProxy(
        "priority_map"
      ) |>
        
        clearGroup(
          "selected_precinct"
        ) |>
        
        fitBounds(
          
          lng1 = as.numeric(
            selected_bbox["xmin"]
          ),
          
          lat1 = as.numeric(
            selected_bbox["ymin"]
          ),
          
          lng2 = as.numeric(
            selected_bbox["xmax"]
          ),
          
          lat2 = as.numeric(
            selected_bbox["ymax"]
          )
        ) |>
        
        addPolygons(
          
          data = selected_precinct,
          
          group =
            "selected_precinct",
          
          fill = FALSE,
          
          color = "black",
          weight = 5,
          
          options =
            pathOptions(
              interactive = FALSE
            )
        )
    }
  )
  
  # Selected precinct summary card
  output$selected_precinct_card <- renderUI({
    
    selected_name <-
      selected_precinct_name()
    
    if (
      is.null(selected_name)
    ) {
      
      return(
        wellPanel(
          
          h4(
            "Selected precinct"
          ),
          
          p(
            "Click a precinct on the map or search for one to view its summary."
          )
        )
      )
    }
    
    precinct <-
      scored_data() |>
      filter(
        PSP_NAME ==
          selected_name
      ) |>
      st_drop_geometry()
    
    req(
      nrow(precinct) > 0
    )
    
    precinct <- precinct[1, ]
    
    wellPanel(
      
      h3(
        precinct$PSP_NAME
      ),
      
      p(
        strong("Study LGA: "),
        precinct$study_lga
      ),
      
      fluidRow(
        
        column(
          width = 3,
          
          h4(
            "Priority score"
          ),
          
          h3(
            paste0(
              round(
                precinct$priority_score,
                1
              ),
              "/100"
            )
          )
        ),
        
        column(
          width = 3,
          
          h4(
            "Development"
          ),
          
          p(
            paste0(
              round(
                precinct$development_score *
                  100,
                1
              ),
              "/100"
            )
          ),
          
          p(
            paste0(
              "Urban growth: ",
              round(
                precinct$growth_ha,
                1
              ),
              " ha"
            )
          )
        ),
        
        column(
          width = 3,
          
          h4(
            "Biodiversity"
          ),
          
          p(
            paste0(
              round(
                precinct$biodiversity_score *
                  100,
                1
              ),
              "/100"
            )
          ),
          
          p(
            paste0(
              "High-value habitat: ",
              round(
                precinct$high_habitat_percent,
                1
              ),
              "%"
            )
          )
        ),
        
        column(
          width = 3,
          
          h4(
            "Conservation gap"
          ),
          
          p(
            paste0(
              round(
                precinct$conservation_gap_score *
                  100,
                1
              ),
              "/100"
            )
          ),
          
          p(
            paste0(
              round(
                precinct$high_habitat_outside_bcs_ha,
                1
              ),
              " ha outside BCS"
            )
          )
        )
      ),
      
      hr(),
      
      fluidRow(
        
        column(
          width = 4,
          
          strong(
            "Golden Sun Moth"
          ),
          
          br(),
          
          paste0(
            precinct$golden_sun_moth_records,
            " recent records"
          )
        ),
        
        column(
          width = 4,
          
          strong(
            "Growling Grass Frog"
          ),
          
          br(),
          
          paste0(
            precinct$growling_grass_frog_records,
            " recent records"
          )
        ),
        
        column(
          width = 4,
          
          strong(
            "Species category"
          ),
          
          br(),
          
          precinct$species_group
        )
      ),
      
      br(),
      
      tags$small(
        "Species records indicate recorded occurrences and should not be interpreted as population abundance."
      )
    )
  })
  
  # Reset map
  observeEvent(
    input$reset_map,
    {
      
      selected_precinct_name(
        NULL
      )
      
      updateSelectizeInput(
        session,
        "precinct_search",
        selected = character(0)
      )
      
      leafletProxy(
        "priority_map"
      ) |>
        
        clearGroup(
          "selected_precinct"
        ) |>
        
        fitBounds(
          
          lng1 = as.numeric(
            study_bbox["xmin"]
          ),
          
          lat1 = as.numeric(
            study_bbox["ymin"]
          ),
          
          lng2 = as.numeric(
            study_bbox["xmax"]
          ),
          
          lat2 = as.numeric(
            study_bbox["ymax"]
          )
        )
    }
  )
  
  # Dynamic Top 10 table
  output$priority_table <- renderDT({
    
    table_data <-
      display_data() |>
      
      st_drop_geometry() |>
      
      arrange(
        desc(priority_score)
      ) |>
      
      transmute(
        
        Rank =
          row_number(),
        
        Precinct =
          PSP_NAME,
        
        `Study LGA` =
          study_lga,
        
        `Priority score` =
          round(
            priority_score,
            1
          ),
        
        `Urban growth (ha)` =
          round(
            growth_ha,
            1
          ),
        
        `High-value habitat (%)` =
          round(
            high_habitat_percent,
            1
          ),
        
        `Habitat outside BCS (ha)` =
          round(
            high_habitat_outside_bcs_ha,
            1
          ),
        
        Species =
          species_group
      ) |>
      
      slice_head(
        n = 10
      )
    
    
    datatable(
      
      table_data,
      
      rownames = FALSE,
      
      options = list(
        pageLength = 10,
        searching = FALSE,
        paging = FALSE,
        ordering = TRUE
      )
    )
  })
}

shinyApp(
  ui = ui,
  server = server
)