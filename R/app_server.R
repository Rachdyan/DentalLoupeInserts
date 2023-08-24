#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import purrr
#' @import shinyjs
#' @noRd


our_data <- readxl::read_excel("data/Dental_data.xlsx",
                               sheet = "Lens_details") %>%
  mutate(VLT = scales::percent(as.numeric(VLT)),
         `Price(from)` = scales::dollar(as.numeric(`Price(from)`)))

oem_data <- readxl::read_excel("data/Dental_data.xlsx",
                               sheet = 1)

app_server <- function(input, output, session) {
  observeEvent(input$lmfg,{
    mfg_filt <- loupe_data %>%
      filter(Mfg == input$lmfg)
    updateSelectInput(inputId = "lmod",
                      choices = sort(unique(mfg_filt$Mod)))
    if (length(mfg_filt$Mod) == 1) {
      hide(anim = T,
           time = 0.33,
           animType = "slide",
           "size")
      updateSelectInput(inputId = "size",
                        choices = unique(mfg_filt$Size))
      }
    else {
      show(anim = T,
           time = 0.33,
           animType = "slide",
           "size")
      updateSelectInput(inputId = "size",
                        choices = unique(mfg_filt$Size))
    }
  })
  observeEvent(input$lmod,{
    req(input$lmfg)
    mod_filt <- loupe_data %>%
      filter(Mfg == input$lmfg,
             Mod == input$lmod)
    if (length(mod_filt$Mod) == 1) {
      hide(anim = T,
           time = 0.33,
           animType = "slide",
           "size")
      updateSelectInput(inputId = "size",
                        choices = unique(mod_filt$Size))
    }
    else {
      show(anim = T,
           time = 0.33,
           animType = "slide",
           "size")
      updateSelectInput(inputId = "size",
                        choices = unique(mod_filt$Size))
    }
  })
  filt_data_loupe <- reactive({
    req(input$lmfg)
    req(input$lmod)
    req(input$size)
    loupe_data %>%
      filter(Mfg == input$lmfg &
               Mod == input$lmod &
               Size == input$size)%>%
      mutate("Selected Device" = input$mod_dent,
             "Selected Loupes" = paste0(`Mfg`, " ", `Mod`, " (", `Size`, ") "),
             "Compatible Loupe Insert" = `Insert Part Number`,
             .keep = "none")

  })
  output$table_loupe <- renderTable(striped = T,
                                    hover = T,
                                    bordered = T,
                                    spacing = c("s"),
                                    width = "auto",
                                    align = "c",
                                    {tibble(
                                            filt_data_loupe()[,1:2],
                                      "Compatible Loupe Insert" = paste0(filt_data_loupe()[,3], ".", filt_data_laser()[[1]]$Lens))})
  observeEvent(input$mfg_dent,{
    dent_mod <- oem_data %>%
      filter(`Laser Mfg` == input$mfg_dent)
    updateSelectInput(inputId = "mod_dent",
                      choices = sort(unique(dent_mod$`Laser Model`)))
  })
  filt_data_laser <- eventReactive(input$mod_dent,{
    dent_mod <- oem_data %>%
      filter(`Laser Model` == input$mod_dent)
    map(unique(dent_mod$`Eyewear Lens Compatible`), ~tibble(filter(our_data, Lens == .x)))
  })

  output$graphs_dent <- renderUI({
    req(filt_data_laser())
    map(1:length(filt_data_laser()), ~HTML(
      c(
        '<div class="shadow p-3 mb-5 bg-body rounded">
      <div class="row">
      <div class="col-sm-5">
        <div align="left">
        <a href="',
        filt_data_laser()[[.x]]$Website,
        '", target = "_blank", title = "', filt_data_laser()[[.x]]$Lens,'frame styles">',
        paste0(filt_data_loupe()[,3], ".", filt_data_laser()[[1]]$Lens),'
        </a>
        </div>
        <div align="center">
        <a href="',
        filt_data_laser()[[.x]]$Website,
        '", target = "_blank", title = "', filt_data_laser()[[.x]]$Lens,'frame styles">
        <img src="',
        filt_data_laser()[[.x]]$Image,
        '", width = 40%>
        </a>
        <a href="',
        filt_data_laser()[[.x]]$Website,
        '", target = "_blank", title = "', filt_data_laser()[[.x]]$Lens,'frame styles">
        <img src="',
        filt_data_laser()[[.x]]$Graph,
        '", width = 100%>
        </a>

        </div>
        </div>
        <div class="col-sm-7">

        <dl>
        <dt style="font-size:0.55em", align="left"><strong>Lens Material</strong></dt><dd style="font-size:0.55em", align="left"> ',filt_data_laser()[[.x]]$Material,'</dd>
        <dt style="font-size:0.55em", align="left"><strong>Price (from)</strong></dt><dd style="font-size:0.55em", align="left"> ',filt_data_laser()[[.x]]$`Price(from)`,'</dd>
        <dt style="font-size:0.55em", align="left"><strong>Optical Density Marking </strong></dt><dd style="font-size:0.55em", align="left">',filt_data_laser()[[.x]]$OD,'</dd>
        <dt style="font-size:0.55em", align="left"><strong>VLT </strong></dt> <dd style="font-size:0.55em", align="left">',filt_data_laser()[[.x]]$VLT,'</dd>
        </dl>
        </div>
        </div>
      </div>'
      )
    ))

  })

  observeEvent(input$run_dent, {
    hide(anim = T,
         time = 0.33,
         animType = "slide",
         "run_dent")
  })
}
