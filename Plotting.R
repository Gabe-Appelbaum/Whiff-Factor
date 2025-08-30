# code to create visualizations used in the presentation

library(tidyverse)
library(jsonlite)
library(ggtext)
library(ggforce)
library(janitor)
library(mlbplotR)
library(gt)
library(gtExtras)
library(gtUtils)
library(webshot2)
library(magick)
library(svglite)

options(scipen = 999)

# MLB org abbreviations with their MLB and AAA stadiums
mlb_stadiums <- {c(
      "Dodger Stadium",
      "Globe Life Field", 
      "Minute Maid Park",
      "Chase Field",
      "Petco Park",
      "Oakland Coliseum",
      "Nationals Park",
      "Tropicana Field",
      "Oracle Park",
      "Angel Stadium",
      "Oriole Park at Camden Yards",
      "Great American Ball Park",
      "Kauffman Stadium",
      "Guaranteed Rate Field",
      "loanDepot park",
      "T-Mobile Park",
      "Citi Field",
      "Citizens Bank Park",
      "Wrigley Field",
      "American Family Field",
      "Target Field",
      "Busch Stadium",
      "Yankee Stadium",
      "Comerica Park",
      "Coors Field",
      "PNC Park",
      "Truist Park",
      "Progressive Field",
      "Rogers Centre",
      "Fenway Park"
)}
mlb_abbreviations <- {c(
      "LAD","TEX","HOU","AZ","SD","ATH","WSH","TB","SF",
      "LAA","BAL","CIN","KC","CWS","MIA","SEA","NYM","PHI","CHC",
      "MIL","MIN","STL","NYY","DET","COL","PIT","ATL","CLE","TOR","BOS"
)}
MLB_abbreviation_team <- data.frame(mlb_stadiums, mlb_abbreviations)

# AAA stadiums with MLB org abbreviations
aaa_stadiums <-{c(
      "Chickasaw Bricktown Ballpark", "Dell Diamond", "Constellation Field",
      "Greater Nevada Field", "Southwest University Park", "Las Vegas Ballpark",
      "Innovative Field", "Durham Bulls Athletic Park", "Sutter Health Park",
      "Smith's Ballpark", "Harbor Park", "Louisville Slugger Field",
      "Werner Park", "Truist Field", "121 Financial Ballpark",
      "Cheney Stadium", "NBT Bank Stadium", "Coca-Cola Park",
      "Principal Park", "First Horizon Park", "CHS Field",
      "AutoZone Park", "PNC Field", "Fifth Third Field",
      "Isotopes Park", "Victory Field",
      "Coolray Field", "Huntington Park", "Sahlen Field", "Polar Park"
)}
aaa_locations <- {c(
      "OKC, OK","Round Rock, TX","Sugar Land, TX","Reno, NV","El Paso, TX",
      "Las Vegas, NV","Rochester, NY","Durham, N.C.","Sacremento, CA",
      "Salt Lake City, UT","Norfolk, VA","Louisville, KY", 
      "Omaha, OK","Charlotte, N.C.", "Jacksonville, FL", "Tacoma, WA", "Syracuse, NY",
      "Lehigh Valley, PA","Des Moines, IA", "Nashville, TN", "St. Paul, MN",
      "Memphis, TN", "Scranton, PA", "Toledo, OH", "Albuquerque, NM",
      "Indianapolis, IN", "Lawrenceville, GA", "Columbus, OH",
      "Buffalo, NY", "Worcester, MA"
)}
AAA_abbreviation_team <- data.frame(aaa_stadiums, mlb_abbreviations, aaa_locations)


# function to rename log odds column and convert to 100 scale for whiff factor
clean_df <- function(df){
      df %>%
            rename(
                  stadium_term = `Stadium term`,
                  park_effect = `(Intercept)`
            ) %>%
            mutate(
                  park_effect = janitor::round_half_up(park_effect * 100 + 100),
                  stadium_term = gsub("day", "- Day", stadium_term),
                  stadium_term = gsub("night", "- Night", stadium_term),
                  stadium_term = gsub("Roof Closed", "- Roof Closed", stadium_term)
            ) %>%
            # create a column for just stadium so I can join with team abbreviations
            mutate(
                  stadium = str_trim(str_extract(stadium_term, "^[^-]+")),
                  # make sure it can handle MLB and AAA edge cases
                  stadium = case_when(
                        stadium == "T" ~ "T-Mobile Park",
                        stadium == "Coca" ~ "Coca-Cola Park",
                        T ~ stadium
                  )
            )
}

# read in the park effects
MLB_effects <- read_csv("final models/MLB day night roof split effects.csv") %>%
      # clean up idiosyncracies from when I wrote the csv
      clean_df() %>%
      # join with team abbreviations
      left_join(., MLB_abbreviation_team, join_by(stadium == mlb_stadiums))

AAA_effects <- read_csv("final models/AAA day night split effects.csv") %>%
      clean_df() %>%
      # join with team abbreviations
      left_join(., AAA_abbreviation_team, join_by(stadium == aaa_stadiums))

#### whiff factor tables ####
# function to create whiff factors table
# code from bless your chart
# https://blessyourchart.substack.com/p/144-charting-the-acc-baseball-regular
whiff_factors_table_mlb <- function(df, top_bottom, level){

      # get either top or bottom 5
      if (top_bottom == "Top") {
            selected_data <- df %>%
                  arrange(desc(park_effect)) %>%
                  head(5)
      } else {
            selected_data <- df %>%
                  arrange(park_effect) %>%
                  head(5)
      }
      
      title_text <- paste0("**", top_bottom, " 5 ", level, " Whiff Factors**")
      
      # make the table
      table_plot <- selected_data %>%
            select(mlb_abbreviations, stadium_term, park_effect) %>%
            gt() %>%
            # set a nice theme
            #gt_theme_gtutils() %>%
            # Add MLB logos using mlbplotR
            gt_fmt_mlb_logo(
                  columns = "mlb_abbreviations",
                  height = 50
            ) %>%
            # color pills for whiff factor
            gt_color_pills(
                  column = park_effect,
                  domain = c(84, 116),
                  format_type = "number", digits = 0,
                  palette = c("#228B22", "#90EE90", "#D3D3D3", "#DDA0DD", "#6A0DAD"),  # Green to purple gradient
                  alpha = 0.5,
                  pill_height = 50
            ) %>%
            # add bold dividers between stuff
            #gt_add_divider(columns = c(mlb_abbreviations, stadium_term, park_effect), 
            #              sides = "all", color = "black") %>%
            # Cleaner column labels labels
            cols_label(
                  mlb_abbreviations = "Org",
                  stadium_term = "Stadium - Environment",
                  park_effect = "Whiff Factor"
            ) %>%
            # Style the header
            tab_header(
                  title = md(title_text)
            ) %>%
            # some sizing and alignment options
            tab_options(
                  table.font.size = 24,
                  heading.title.font.size = 32,
                  column_labels.font.weight = "bold",
                  heading.align = "center"
            ) %>%
            # Center align columns
            cols_align(
                  align = "center",
                  columns = everything()
            )
      
      table_plot
}

whiff_factors_table_AAA <- function(df, top_bottom, level){
      
      # get either top or bottom 5
      if (top_bottom == "Top") {
            selected_data <- df %>%
                  arrange(desc(park_effect)) %>%
                  head(5)
      } else {
            selected_data <- df %>%
                  arrange(park_effect) %>%
                  head(5)
      }
      
      title_text <- paste0("**", top_bottom, " 5 ", level, " Whiff Factors**")
      
      # make the table
      table_plot <- selected_data %>%
            select(mlb_abbreviations, aaa_locations, stadium_term, park_effect) %>%
            gt() %>%
            # set a nice theme
            #gt_theme_gtutils() %>%
            # Add MLB logos using mlbplotR
            gt_fmt_mlb_logo(
                  columns = "mlb_abbreviations",
                  height = 50
            ) %>%
            # color pills for whiff factor
            gt_color_pills(
                  column = park_effect,
                  domain = c(84, 116),
                  format_type = "number", digits = 0,
                  palette = c("#228B22", "#90EE90", "#D3D3D3", "#DDA0DD", "#6A0DAD"),  # Green to purple gradient
                  alpha = 0.5,
                  pill_height = 50
            ) %>%
            # add bold dividers between stuff
            #gt_add_divider(columns = c(mlb_abbreviations, stadium_term, park_effect), 
            #              sides = "all", color = "black") %>%
            # Cleaner column labels labels
            cols_label(
                  mlb_abbreviations = "Org",
                  aaa_locations = "Location",
                  stadium_term = "Stadium - Environment",
                  park_effect = "Whiff Factor"
            ) %>%
            # Style the header
            tab_header(
                  title = md(title_text)
            ) %>%
            # some sizing and alignment options
            tab_options(
                  table.font.size = 24,
                  heading.title.font.size = 32,
                  column_labels.font.weight = "bold",
                  heading.align = "center"
            ) %>%
            # Center align columns
            cols_align(
                  align = "center",
                  columns = everything()
            )
      
      table_plot
}


MLB_top5_table <- whiff_factors_table_mlb(MLB_effects, 'Top', "MLB")

AAA_table <- whiff_factors_table_AAA(AAA_effects, "Top", "AAA")

gtsave(MLB_top5_table, "viz/Top 5 MLB whiff factor.html")
gtsave(
      whiff_factors_table_mlb(MLB_effects, 'Bottom', "MLB"), 
      "viz/Bottom 5 MLB whiff factor.html"
)

gtsave(
      whiff_factors_table_AAA(AAA_effects, "Top", "AAA"), 
      "viz/Top 5 AAA whiff factor.html"
)

gtsave(
      whiff_factors_table_AAA(AAA_effects, "Bottom", "AAA"), 
      "viz/Bottom 5 AAA whiff factor.html"
)

#### day vs night section ####
# day and night pairing function
day_night_df <- function(df){
      inner_join(
            df %>%
                  filter(grepl("Day", stadium_term)) %>%
                  select(mlb_abbreviations, stadium, daytime_whiff_factor = park_effect),
            
            df %>%
                  filter(grepl("Night", stadium_term)) %>%
                  select(mlb_abbreviations, stadium, nighttime_whiff_factor = park_effect)
      )
}

# day night split df's
MLB_day_night <- day_night_df(MLB_effects)
AAA_day_night <- day_night_df(AAA_effects)

#### comet plot code from Chris of the F5 blog ####
# https://thef5.substack.com/p/how-to-comet-plots-v2
MLB_comet <- MLB_day_night %>%
      mutate(
            mlb_abbreviations = case_when(
                  mlb_abbreviations == 'ATH' ~ 'OAK',
                  T ~ mlb_abbreviations
            )
      ) %>%
      mutate(
            more_less = ifelse(
                  daytime_whiff_factor - nighttime_whiff_factor >= 0, 
                  "More Whiffs in the Day", "Less Whiffs in the Day"
            ),
            mlb_abbreviations = as.factor(mlb_abbreviations),
            mlb_abbreviations = fct_reorder(mlb_abbreviations, daytime_whiff_factor)
      ) %>%
      ggplot() +
      geom_link(
            aes(x = nighttime_whiff_factor, xend = daytime_whiff_factor,
                y = mlb_abbreviations, yend = mlb_abbreviations,
                color = more_less, linewidth = after_stat(index)), n = 1000)  +
      geom_point(
            aes(x = daytime_whiff_factor, y = mlb_abbreviations, color = more_less),
            shape = 21,
            fill = "white",
            size = 3.5
      ) + 
      scale_color_manual(values = c("#6A0DAD", "#228B22")) +
      scale_linewidth(range = c(0.01, 4)) +
      coord_cartesian(clip = 'off') +
      theme_minimal(base_size = 25) +
      theme(legend.position = 'none',
            plot.title = element_markdown(size = 25, face = 'bold'), 
            plot.title.position = 'plot',
            axis.text.y = element_mlb_logo(size = 1)
      ) +
      labs(title = "Is MLB Whiff Factor 
             <span style='color:#228B22'>**higher**</span> or 
             <span style='color:#6A0DAD'>**lower**</span> during the day?", 
           subtitle = "Stadiums sorted by day time whiff factor", 
           x = "Whiff Factors",
           y = "")

MLB_comet

AAA_comet <- AAA_day_night %>%
      mutate(
            mlb_abbreviations = case_when(
                  mlb_abbreviations == 'ATH' ~ 'OAK',
                  T ~ mlb_abbreviations
            )
      ) %>%
      mutate(
            more_less = ifelse(
                  daytime_whiff_factor - nighttime_whiff_factor >= 0, 
                  "More Whiffs in the Day", "Less Whiffs in the Day"
            ),
            mlb_abbreviations = as.factor(mlb_abbreviations),
            mlb_abbreviations = fct_reorder(mlb_abbreviations, daytime_whiff_factor)
      ) %>%
      ggplot() +
      geom_link(
            aes(x = nighttime_whiff_factor, xend = daytime_whiff_factor,
                y = mlb_abbreviations, yend = mlb_abbreviations,
                color = more_less, linewidth = after_stat(index)), n = 1000)  +
      geom_point(
            aes(x = daytime_whiff_factor, y = mlb_abbreviations, color = more_less),
            shape = 21,
            fill = "white",
            size = 3.5
      ) + 
      scale_color_manual(values = c("#6A0DAD", "#228B22")) +
      scale_linewidth(range = c(0.01, 4)) +
      coord_cartesian(clip = 'off') +
      theme_minimal(base_size = 25) +
      theme(legend.position = 'none',
            plot.title = element_markdown(size = 25, face = 'bold'), 
            plot.title.position = 'plot',
            axis.text.y = element_mlb_logo(size = 1)
      ) +
      labs(title = "Is AAA Whiff Factor 
             <span style='color:#228B22'>**higher**</span> or 
             <span style='color:#6A0DAD'>**lower**</span> during the day?", 
           subtitle = "Stadiums sorted by day time whiff factor", 
           x = "Whiff Factors",
           y = "")

AAA_comet

ggsave("MLB Comet plot.png", MLB_comet, width = 10, height = 12)

ggsave("AAA Comet plot.png", AAA_comet, width = 10, height = 12)
