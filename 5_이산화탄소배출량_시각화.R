# 0. Package Installation Check (Install only if missing)
required_packages <- c("openxlsx", "ggplot2", "treemapify", "maps")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cran.seoul.go.kr/")
}

library(openxlsx)
library(ggplot2)
library(treemapify)
library(maps)

# 1. Path Setup
file_path <- "5_이산화탄소배출량_데이터.xlsx"
if (!file.exists(file_path)) {
  file_path <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용/5_이산화탄소배출량_데이터.xlsx"
}
fig_dir <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# 14 groups order definition
groups_order <- c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM", "ROW")

# Names mapping
geo_names <- data.frame(
  Group = c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM", "ROW"),
  Name = c("한국", "중국", "일본", "인도", "아세안", "오세아니아", "미국", "캐나다", "유럽", "구소련", "사하라이남", "중동·북아프리카", "중남미", "기타지역"),
  stringsAsFactors = FALSE
)

# LOCF Correction Function
apply_locf <- function(df, years) {
  for (i in 1:nrow(df)) {
    for (y_idx in 2:length(years)) {
      current_yr <- years[y_idx]
      if (is.na(df[i, current_yr])) {
        prev_years <- years[1:(y_idx - 1)]
        prev_vals <- sapply(prev_years, function(py) df[i, py])
        valid_prev_idx <- which(!is.na(prev_vals))
        if (length(valid_prev_idx) > 0) {
          closest_idx <- max(valid_prev_idx)
          df[i, current_yr] <- df[i, years[closest_idx]]
        }
      }
    }
  }
  return(df)
}

# 2. Load generation & coeff data
df_gen <- readWorkbook(file_path, sheet = "generation", startRow = 2)
colnames(df_gen) <- trimws(colnames(df_gen))
colnames(df_gen)[colnames(df_gen) == "Technology"] <- "tech"
colnames(df_gen)[colnames(df_gen) == "X2"] <- "tech"
df_gen$Group <- toupper(trimws(df_gen[[colnames(df_gen)[grep("UNICON", colnames(df_gen), ignore.case = TRUE)[1]]]]))
df_gen <- apply_locf(df_gen, as.character(2017:2023))

df_coeff <- readWorkbook(file_path, sheet = "coeff", startRow = 2)
colnames(df_coeff) <- trimws(colnames(df_coeff))

# Separate 2017 and 2020 coefficients
coeff_2017 <- df_coeff[1:12, c(1, 2, 3:16)]
colnames(coeff_2017) <- c("element", "tech", groups_order)
coeff_2020 <- df_coeff[1:12, c(1, 2, 18:31)]
colnames(coeff_2020) <- c("element", "tech", groups_order)

# Normalize tech names and convert to numeric
normalize_tech_coeff <- function(df) {
  df$tech <- trimws(tolower(df$tech))
  df$tech[df$tech == "biofuels"] <- "biomass"
  df$element <- trimws(toupper(df$element))
  for (g in groups_order) {
    df[[g]] <- as.numeric(df[[g]])
  }
  return(df)
}
coeff_2017 <- normalize_tech_coeff(coeff_2017)
coeff_2020 <- normalize_tech_coeff(coeff_2020)

# Calculate emissions function
calculate_emissions_by_fuel <- function(year, coeff_set) {
  emissions_list <- list()
  combust_techs <- c("coal", "oil", "lng", "biomass")
  
  for (g in groups_order) {
    df_g <- df_gen[df_gen$Group == g, ]
    for (t in combust_techs) {
      gen_tech_name <- ifelse(t == "biomass", "Biomass", 
                       ifelse(t == "coal", "Coal", 
                       ifelse(t == "oil", "Oil", "LNG")))
      
      val_gen_gwh <- sum(df_g[tolower(trimws(df_g$tech)) == tolower(gen_tech_name), as.character(year)], na.rm = TRUE)
      co2_coef <- coeff_set[coeff_set$element == "CO2" & coeff_set$tech == t, g]
      if (length(co2_coef) == 0 || is.na(co2_coef)) co2_coef <- 0
      
      co2_mt <- (val_gen_gwh * co2_coef) / 1000000
      
      emissions_list[[length(emissions_list) + 1]] <- data.frame(
        Group = g,
        tech = t,
        CO2_Mt = co2_mt,
        stringsAsFactors = FALSE
      )
    }
  }
  return(do.call(rbind, emissions_list))
}

# Run calculations
df_emissions_fuel_2017 <- calculate_emissions_by_fuel(2017, coeff_2017)
df_emissions_fuel_2023 <- calculate_emissions_by_fuel(2023, coeff_2020)

total_co2_2017 <- aggregate(CO2_Mt ~ Group, data = df_emissions_fuel_2017, FUN = sum)
total_co2_2023 <- aggregate(CO2_Mt ~ Group, data = df_emissions_fuel_2023, FUN = sum)


# ==============================================================================
# [5-1] 2023년 국가(그룹)별 이산화탄소 배출량 연료원별 기여도
# ==============================================================================
cat("Generating 5-1) CO2 Stacked Bar Chart...\n")
df_chart1 <- merge(df_emissions_fuel_2023, geo_names, by = "Group")
df_totals1 <- merge(total_co2_2023, geo_names, by = "Group")

sorted_groups_c1 <- df_totals1$Name[order(df_totals1$CO2_Mt)]
df_chart1$Name <- factor(df_chart1$Name, levels = sorted_groups_c1)
df_totals1$Name <- factor(df_totals1$Name, levels = sorted_groups_c1)

df_chart1$tech_ko <- factor(
  ifelse(df_chart1$tech == "coal", "석탄 (Coal)",
         ifelse(df_chart1$tech == "lng", "가스 (LNG)",
                ifelse(df_chart1$tech == "oil", "석유 (Oil)", "바이오/폐기물"))),
  levels = c("석탄 (Coal)", "가스 (LNG)", "석유 (Oil)", "바이오/폐기물")
)

coal_colors <- c(
  "석탄 (Coal)" = "#222222",
  "가스 (LNG)" = "#E31A1C",
  "석유 (Oil)" = "#FF7F00",
  "바이오/폐기물" = "#B2DF8A"
)

p1 <- ggplot(df_chart1, aes(x = CO2_Mt, y = Name, fill = tech_ko)) +
  geom_col(color = "#FFFFFF", linewidth = 0.2, width = 0.65) +
  geom_text(data = df_totals1, aes(x = CO2_Mt, y = Name, label = sprintf("%.1f Mt", CO2_Mt)),
            hjust = -0.15, size = 3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = coal_colors, name = "발전연료원") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "2023년 국가(그룹)별 이산화탄소(CO2) 배출량 및 연료원별 기여도 (Mt)",
    subtitle = "가로 막대의 총 길이는 총 배출량이며, 내부 누적 영역은 발전원별 탄소 배출비중을 나타냄",
    x = "이산화탄소 배출량 (Mt)",
    y = "국가 (그룹)",
    caption = "데이터 기준: 5_이산화탄소배출량_데이터.xlsx"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", size = 9, color = "#333333"),
    axis.text.x = element_text(size = 8, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "5-1)2023년 국가(그룹)별 이산화탄소 배출량 연료원별 기여도.png"), plot = p1, width = 11, height = 7, dpi = 300)


# ==============================================================================
# [5-2] 2017년 대비 2023년 국가(그룹)별 이산화탄소 배출량 증감량 (연료원별 양방향 누적)
# ==============================================================================
cat("Generating 5-2) CO2 Diverging Stacked Bar Chart...\n")
df_change_fuel <- merge(
  df_emissions_fuel_2017,
  df_emissions_fuel_2023,
  by = c("Group", "tech"),
  suffixes = c("_2017", "_2023")
)
df_change_fuel$Change_Mt <- df_change_fuel$CO2_Mt_2023 - df_change_fuel$CO2_Mt_2017

# Compute WORLD row
world_fuel_change <- aggregate(cbind(CO2_Mt_2017, CO2_Mt_2023, Change_Mt) ~ tech, data = df_change_fuel, FUN = sum)
world_fuel_change$Group <- "WORLD"
df_change_fuel <- rbind(df_change_fuel, world_fuel_change[, colnames(df_change_fuel)])
df_change_fuel <- merge(df_change_fuel, rbind(geo_names, data.frame(Group="WORLD", Name="전세계")), by = "Group")

df_net_change <- aggregate(Change_Mt ~ Group + Name, data = df_change_fuel, FUN = sum)
df_net_change$Abs_Net_Change <- abs(df_net_change$Change_Mt)
df_net_change$Abs_Net_Change[df_net_change$Group == "WORLD"] <- 999999

sorted_groups_c2 <- df_net_change$Name[order(df_net_change$Abs_Net_Change)]
df_change_fuel$Name <- factor(df_change_fuel$Name, levels = sorted_groups_c2)
df_net_change$Name <- factor(df_net_change$Name, levels = sorted_groups_c2)

df_change_fuel$tech_ko <- factor(
  ifelse(df_change_fuel$tech == "coal", "석탄 (Coal)",
         ifelse(df_change_fuel$tech == "lng", "가스 (LNG)",
                ifelse(df_change_fuel$tech == "oil", "석유 (Oil)", "바이오/폐기물"))),
  levels = c("석탄 (Coal)", "가스 (LNG)", "석유 (Oil)", "바이오/폐기물")
)

# Calculate positive and negative sums for outer edge label placement
sum_pos_df <- aggregate(Change_Mt ~ Group, data = df_change_fuel[df_change_fuel$Change_Mt > 0, ], FUN = sum, na.rm = TRUE)
colnames(sum_pos_df)[2] <- "Sum_Pos"

sum_neg_df <- aggregate(Change_Mt ~ Group, data = df_change_fuel[df_change_fuel$Change_Mt < 0, ], FUN = sum, na.rm = TRUE)
colnames(sum_neg_df)[2] <- "Sum_Neg"

df_net_change <- merge(df_net_change, sum_pos_df, by = "Group", all.x = TRUE)
df_net_change$Sum_Pos[is.na(df_net_change$Sum_Pos)] <- 0

df_net_change <- merge(df_net_change, sum_neg_df, by = "Group", all.x = TRUE)
df_net_change$Sum_Neg[is.na(df_net_change$Sum_Neg)] <- 0

df_net_change$x_pos <- df_net_change$Change_Mt
df_net_change$hjust_val <- ifelse(df_net_change$Change_Mt >= 0, -0.15, 1.15)

# Specific override for USA to avoid overlap (place on the opposite end of the stacked bar)
df_net_change$x_pos[df_net_change$Group == "USA"] <- df_net_change$Sum_Pos[df_net_change$Group == "USA"]
df_net_change$hjust_val[df_net_change$Group == "USA"] <- -0.15

df_net_change$Name <- factor(df_net_change$Name, levels = sorted_groups_c2)

p2 <- ggplot(df_change_fuel, aes(x = Change_Mt, y = Name, fill = tech_ko)) +
  geom_vline(xintercept = 0, color = "#555555", linewidth = 0.5) +
  geom_col(width = 0.65, color = "#FFFFFF", linewidth = 0.1) +
  geom_text(
    data = df_net_change,
    aes(x = x_pos, y = Name, label = paste0(ifelse(Change_Mt >= 0, "+", ""), round(Change_Mt, 1), " Mt"), hjust = hjust_val),
    size = 3.0,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = coal_colors, name = "발전연료원") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title = "2017년 대비 2023년 국가(그룹)별 이산화탄소(CO2) 배출 연료원별 양방향 증감 (Mt)",
    subtitle = "막대의 성분은 연료원별 배출량 증감을 나타내며, 텍스트 라벨은 순(Net) 증감량을 나타냄",
    x = "이산화탄소 증감량 (Mt)",
    y = "국가 (그룹)",
    caption = "데이터 기준: 5_이산화탄소배출량_데이터.xlsx"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", size = 9, color = "#333333"),
    axis.text.x = element_text(size = 8, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "5-2)2017년 대비 2023년 국가(그룹)별 이산화탄소 배출량 증감량.png"), plot = p2, width = 11, height = 7, dpi = 300)


# ==============================================================================
# [5-3] 2023년 글로벌 권역별 이산화탄소 배출량 GIS 버블 지도 (배출총량 기준)
# ==============================================================================
cat("Generating 5-3) CO2 GIS Bubble Map...\n")
change_df <- merge(df_net_change, total_co2_2023, by = "Group")
colnames(change_df)[colnames(change_df) == "CO2_Mt"] <- "CO2_2023"
change_df$Color_Cat <- ifelse(change_df$Change_Mt > 0, "증가 (Increase)", "감축 (Decrease)")

geo_centers <- data.frame(
  Group = c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM"),
  Name = c("한국", "중국", "일본", "인도", "아세안", "오세아니아", "미국", "캐나다", "유럽", "구소련", "사하라이남", "중동·북아프리카", "중남미"),
  lon = c(127.5, 105.0, 138.0, 78.9, 115.0, 133.0, -95.7, -106.0, 15.0, 90.0, 22.0, 45.0, -60.0),
  lat = c(36.5, 35.0, 36.0, 20.5, 1.5, -25.0, 37.0, 56.0, 50.0, 60.0, -2.0, 25.0, -15.0),
  stringsAsFactors = FALSE
)

df_map <- merge(geo_centers, change_df[, c("Group", "CO2_2023", "Change_Mt", "Color_Cat")], by = "Group")
row_data_map <- change_df[change_df$Group == "ROW", ]

world_map <- map_data("world")
world_map <- world_map[world_map$region != "Antarctica", ]

p3 <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "#E1F2E1", color = "#FFFFFF", linewidth = 0.2) +
  geom_point(data = df_map, aes(x = lon, y = lat, size = CO2_2023, color = CO2_2023), alpha = 0.7) +
  scale_size_area(max_size = 18, guide = "none") +
  scale_color_gradient(low = "#FFA500", high = "#E31A1C", name = "이산화탄소 배출량 (Mt)") +
  geom_text(data = df_map, aes(x = lon, y = lat + 4.8, label = sprintf("%s\n(%.1f Mt)", Name, CO2_2023)),
            size = 2.8, fontface = "bold", color = "#111111", lineheight = 0.9) +
  # ROW card
  annotate("rect", xmin = -160, xmax = -90, ymin = -50, ymax = -32, fill = "#FFFFFF", color = "#888888", size = 0.5, alpha = 0.9) +
  annotate("text", x = -125, y = -41,
           label = sprintf("기타지역 (ROW)\n지리적 파편화 그룹\n배출량: %.1f Mt\n변화량: %+.1f Mt", 
                           row_data_map$CO2_2023, row_data_map$Change_Mt),
           size = 3.3, fontface = "bold", color = "#333333") +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 10, color = "#555555", hjust = 0.5, margin = margin(b = 15)),
    plot.background = element_rect(fill = "#F2F7FA", color = NA),
    legend.position = "bottom"
  ) +
  labs(
    title = "2023년 글로벌 권역별 이산화탄소(CO2) 배출총량 GIS 버블 지도",
    subtitle = "버블의 크기(면적)와 색상 농도는 각 권역별 2023년 발전부문 이산화탄소 배출총량(Mt)에 비례함"
  ) +
  coord_quickmap(xlim = c(-180, 180), ylim = c(-60, 85))

ggsave(file.path(fig_dir, "5-3)2023년 글로벌 권역별 이산화탄소 배출량 GIS 버블 지도.png"), plot = p3, width = 11, height = 7, dpi = 300)


# ==============================================================================
# [5-4] 2023년 글로벌 발전 부문 이산화탄소 배출량 국가별 점유 비중 트리맵
# ==============================================================================
cat("Generating 5-4) CO2 Share Treemap...\n")
df_tree <- merge(total_co2_2023, geo_names, by = "Group")
df_tree$Share <- (df_tree$CO2_Mt / sum(df_tree$CO2_Mt)) * 100

p4 <- ggplot(df_tree, aes(
  area = CO2_Mt, 
  fill = CO2_Mt, 
  label = paste0(Name, "\n", round(CO2_Mt, 1), " Mt\n(", round(Share, 1), "%)")
)) +
  geom_treemap(color = "white", size = 1.5) +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE, fontface = "bold") +
  scale_fill_distiller(palette = "Reds", direction = 1, name = "이산화탄소 배출량 (Mt)") +
  labs(
    title = "2023년 글로벌 발전 부문 이산화탄소(CO2) 배출량 국가별 점유 비중 트리맵",
    subtitle = "전세계 총 발전 탄소 배출량 중 개별 국가(그룹)가 차지하는 비율(%) 및 배출규모(Mt)",
    caption = "데이터 기준: 5_이산화탄소배출량_데이터.xlsx"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "#555555", margin = margin(b = 15)),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "5-4) 2023년 글로벌 발전 부문 이산화탄소 배출량 국가별 점유 비중.png"), plot = p4, width = 10, height = 7.5, dpi = 300)

cat("SUCCESS: All 4 CO2 Emissions visualizations generated successfully.\n")
