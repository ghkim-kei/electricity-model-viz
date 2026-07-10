# 0. Package Installation Check (Install only if missing)
required_packages <- c("openxlsx", "ggplot2", "maps")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cran.seoul.go.kr/")
}

library(openxlsx)
library(ggplot2)
library(maps)

# 1. Path Setup
file_path <- "3_전력수요_데이터.xlsx"
if (!file.exists(file_path)) {
  file_path <- "D:/Users/KEI/Desktop/환경연구원/전력모형/시각화/송부용/3_전력수요_데이터.xlsx"
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

# Load demand worksheet
df_dem <- readWorkbook(file_path, sheet = "demand_annual", startRow = 2)
colnames(df_dem) <- trimws(colnames(df_dem))
df_dem$Group <- toupper(trimws(df_dem[[colnames(df_dem)[grep("UNICON", colnames(df_dem), ignore.case = TRUE)[1]]]]))
df_dem <- apply_locf(df_dem, as.character(2017:2023))


# ==============================================================================
# [3-1] 2023년 글로벌 권역별 연간 전력수요 GIS 버블 지도
# ==============================================================================
cat("Generating 3-1) Demand GIS Bubble Map...\n")
dem_2023 <- aggregate(df_dem[["2023"]], by = list(Group = df_dem$Group), FUN = sum, na.rm = TRUE)
colnames(dem_2023)[2] <- "Demand_GWh"
dem_2023$Demand_TWh <- dem_2023$Demand_GWh / 1000

geo_centers <- data.frame(
  Group = c("KOR", "CHN", "JPN", "IND", "ASEAN", "OCE", "USA", "CAN", "EUR", "FSU", "SSA", "MENA", "LATAM"),
  Name = c("한국", "중국", "일본", "인도", "아세안", "오세아니아", "미국", "캐나다", "유럽", "구소련", "사하라이남", "중동·북아프리카", "중남미"),
  lon = c(127.5, 105.0, 138.0, 78.9, 115.0, 133.0, -95.7, -106.0, 15.0, 90.0, 22.0, 45.0, -60.0),
  lat = c(36.5, 35.0, 36.0, 20.5, 1.5, -25.0, 37.0, 56.0, 50.0, 60.0, -2.0, 25.0, -15.0),
  stringsAsFactors = FALSE
)

map_data_dem <- merge(geo_centers, dem_2023, by = "Group")
row_dem_2023 <- dem_2023$Demand_TWh[dem_2023$Group == "ROW"]

world_map <- map_data("world")
world_map <- world_map[world_map$region != "Antarctica", ]

p_map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "#E1F2E1", color = "#FFFFFF", linewidth = 0.2) +
  geom_point(data = map_data_dem, aes(x = lon, y = lat, size = Demand_TWh), color = "#0072B2", alpha = 0.6) +
  scale_size_area(max_size = 18, guide = "none") +
  geom_text(data = map_data_dem, aes(x = lon, y = lat + 4.8, label = sprintf("%s\n(%.1f TWh)", Name, Demand_TWh)),
            size = 2.8, fontface = "bold", color = "#111111", lineheight = 0.9) +
  # ROW Box annotation
  annotate("rect", xmin = -160, xmax = -90, ymin = -50, ymax = -32,
           fill = "#FFFFFF", color = "#888888", size = 0.5, alpha = 0.9) +
  annotate("text", x = -125, y = -41,
           label = sprintf("기타지역 (ROW)\n지리적 파편화 그룹\n총 전력수요: %.1f TWh", row_dem_2023),
           size = 3.3, fontface = "bold", color = "#333333") +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(size = 10, color = "#555555", hjust = 0.5, margin = margin(b = 15)),
    plot.background = element_rect(fill = "#F2F7FA", color = NA)
  ) +
  labs(
    title = "2023년 글로벌 권역별 연간 전력수요 GIS 버블 지도",
    subtitle = "버블의 크기(면적)는 각 권역별 2023년 총 전력수요량(TWh) 규모를 나타냄"
  ) +
  coord_quickmap(xlim = c(-180, 180), ylim = c(-60, 85))

ggsave(file.path(fig_dir, "3-1)2023년 글로벌 권역별 연간 전력수요 GIS 버블 지도.png"), plot = p_map, width = 11, height = 7, dpi = 300)


# ==============================================================================
# [3-2] 2017년 대비 2023년 국가(그룹)별 전력수요 변화량 (TWh)
# ==============================================================================
cat("Generating 3-2) Demand Change Chart...\n")
dem_2017 <- aggregate(df_dem[["2017"]], by = list(Group = df_dem$Group), FUN = sum, na.rm = TRUE)
colnames(dem_2017)[2] <- "Demand_2017_GWh"
dem_2017$Demand_2017_TWh <- dem_2017$Demand_2017_GWh / 1000

df_chg_dem <- merge(dem_2017[, c("Group", "Demand_2017_TWh")], dem_2023[, c("Group", "Demand_TWh")], by = "Group")
df_chg_dem$Change_TWh <- df_chg_dem$Demand_TWh - df_chg_dem$Demand_2017_TWh
df_chg_dem$Abs_Change <- abs(df_chg_dem$Change_TWh)

# Add World row
world_row <- data.frame(
  Group = "WORLD",
  Demand_2017_TWh = sum(df_chg_dem$Demand_2017_TWh),
  Demand_TWh = sum(df_chg_dem$Demand_TWh),
  Change_TWh = sum(df_chg_dem$Change_TWh),
  Abs_Change = 999999
)
df_chg_dem <- rbind(df_chg_dem, world_row)
df_chg_dem <- merge(df_chg_dem, rbind(geo_names, data.frame(Group="WORLD", Name="전세계")), by = "Group")

sorted_groups_dem <- df_chg_dem$Name[order(df_chg_dem$Abs_Change)]
df_chg_dem$Name <- factor(df_chg_dem$Name, levels = sorted_groups_dem)

df_chg_dem$Color_Cat <- ifelse(df_chg_dem$Change_TWh > 0, "증가", "감축")
change_colors <- c("증가" = "#E31A1C", "감축" = "#1F78B4")

p_chg <- ggplot(df_chg_dem, aes(x = Change_TWh, y = Name, fill = Color_Cat)) +
  geom_vline(xintercept = 0, color = "#555555", linewidth = 0.5) +
  geom_col(width = 0.65, color = "#FFFFFF", linewidth = 0.1) +
  geom_text(
    aes(label = paste0(ifelse(Change_TWh >= 0, "+", ""), round(Change_TWh, 1), " TWh")),
    hjust = ifelse(df_chg_dem$Change_TWh >= 0, -0.15, 1.15),
    size = 3.0,
    fontface = "bold"
  ) +
  scale_fill_manual(values = change_colors, name = "전력수요 증감 분류") +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title = "2017년 대비 2023년 국가(그룹)별 전력수요 변화량 (TWh)",
    subtitle = "붉은색 막대는 수요 증가국, 파란색 막대는 수요 감소 국가군을 나타냄",
    x = "전력수요 변화량 (TWh)",
    y = "국가 (그룹)",
    caption = "데이터 기준: input_DB_2025 수정본"
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

ggsave(file.path(fig_dir, "3-2)2017년 대비 2023년 국가(그룹)별 전력수요 변화량 (TWh).png"), plot = p_chg, width = 11, height = 7, dpi = 300)

cat("SUCCESS: All 2 Power Demand visualizations generated successfully.\n")
