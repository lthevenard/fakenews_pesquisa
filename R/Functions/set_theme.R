fgv_palette <- list(
  primary = "#112f68",
  secondary = "#3f8ac7",
  background = "#1b2e4c",
  blues0 = "#081734",
  blues1 = "#03122e",
  blues2 = "#606d82",
  blues3 = "#CFE1F1",
  blues4 = "#EBEEFF",
  greens1 = "#328985",
  greens2 = "#D8ECEB",
  grays1 = "#444655",
  grays2 = "#A9AABC",
  browns1 = "#412F00",
  browns2 = "#745D1D",
  browns1 = "#F1F1E6",
  primary_shades1 = "#415391",
  primary_shades2 = "#6B79BC",
  primary_shades3 = "#96A2E8",
  primary_shades4 = "#C3CEFF"
)

color_ramp_fgv <- function(size, type = c("color", "bw"), mode = "dark") {
  if (type[1] == "color") {
    if (size == 1) {
      if (mode == "dark") {
        return(
          fgv_palette$primary
        )
      } else if (mode[1] == "light") {
        return(
          fgv_palette$secondary
        )
      }
    } else if (size > 3) {
      return(
        colorRampPalette(
          c(fgv_palette$primary,
            fgv_palette$secondary,
            fgv_palette$blues3)
        )(size)
      )
    } else {
      return(
        colorRampPalette(
          c(fgv_palette$primary,
            fgv_palette$secondary)
        )(size)
      )
    }
  } else if (type[1] == "bw") {
    if (size == 1) {
      if (mode[1] == "dark") {
        return(
          gray.colors(8, start = 0.15, end = 0.9)[1]
        )
      } else if (mode[1] == "light") {
        return(
          gray.colors(8, start = 0.15, end = 0.9, rev = TRUE)[4]
        )
      }
    } else {
      return(
        gray.colors(size, start = 0.15, end = 0.9)
      )
    }
  }
}

set_theme_from_input <- function(input_theme) {
  if (input_theme == "Digital") {
    set_theme_fgv_digital()
  } else if (input_theme == "Colorido") {
    set_theme_fgv_colorido()
  } else {
    set_theme_fgv_bw()
  }
}

set_theme_fgv_digital <- function () {
  ggplot2::theme_set({
    ggplot2::theme(
      panel.background = element_rect(
        fill = "grey97", 
        color = "transparent"
      ),
      panel.border = element_rect(
        color = "grey90", 
        fill = NA, 
        size = 1
      ),
      axis.ticks = element_line(
        color = "grey90"
      ),
      legend.key = element_rect(
        fill = "grey97", 
        color = "grey90"
      ),
      legend.position = "bottom",
      text = element_text(
        color = fgv_palette$blues0
      ),
      axis.title = element_text(
        margin = margin(t = 1, r = 1, b = 1, l = 1)
      ),
      axis.text = element_text(
        color = fgv_palette$blues0),
      plot.title = element_text(
        face = "bold", 
        hjust = 0.5, 
        vjust = 1,
        color = fgv_palette$blues0,
        margin = margin(b = 2)
      )
    )
  })
}

set_theme_fgv_colorido <- function () {
  ggplot2::theme_set({
    ggplot2::theme(
      panel.background = element_rect(
        fill = "white", 
        color = "white"
      ),
      panel.border = element_rect(
        color = "black", 
        fill = NA
      ),
      panel.grid = element_line(
        color = "grey95"
      ),
      axis.ticks = element_line(
        color = "black"
      ),
      legend.key = element_rect(
        fill = "white", 
        color = "black"
      ),
      legend.position = "bottom",
      text = element_text(
        color = fgv_palette$blues0
      ),
      axis.title = element_text(
        margin = margin(t = 1, r = 1, b = 1, l = 1)
      ),
      axis.text = element_text(
        color = fgv_palette$blues0),
      plot.title = element_text(
        face = "bold", 
        hjust = 0.5, 
        vjust = 1,
        color = fgv_palette$blues0
      )
    )
  })
}

set_theme_fgv_bw <- function () {
  ggplot2::theme_set({
    ggplot2::theme(
      panel.background = element_rect(
        fill = "white", 
        color = "white"
      ),
      panel.border = element_rect(
        color = "#191919", 
        fill = NA
      ),
      panel.grid = element_line(
        color = "grey95"
      ),
      axis.ticks = element_line(
        color = "#191919"
      ),
      legend.key = element_rect(
        fill = "white", 
        color = "#191919"
      ),
      legend.position = "bottom",
      text = element_text(
        color = "#191919"
      ),
      axis.title = element_text(
        margin = margin(t = 1, r = 1, b = 1, l = 1)
      ),
      axis.text = element_text(
        color = "#191919"),
      plot.title = element_text(
        face = "bold", 
        hjust = 0.5, 
        vjust = 1,
        color = "#191919"
      )
    )
  })
}

get_blank_theme <- function() {
  blank_theme <- theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank()
    )
  return(blank_theme)
}


get_app_theme <- function() {
  return(
    bslib::bs_theme(
      bootswatch = "united", 
      primary = fgv_palette$primary, 
      secondary = fgv_palette$secondary
    )
  )
}