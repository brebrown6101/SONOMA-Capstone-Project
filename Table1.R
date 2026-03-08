library(dplyr)
library(gtsummary)
data_path <- "sonoma_data/Sonoma_cleaned2.csv"

clean_s <- read.csv(data_path, check.names = FALSE, row.names = 1)
vars <- c(
    "Age_Categories", "Sex", "appendicolithoutcomes", "ccioutcomes",
    "RaceEthnicity", "appy_sizeoutcomes", "bmi", "wbcoutcomes", "nlroutcomes"
)

# ---- site process ----
# Remove sites with 0 patients before factoring
site_counts <- table(clean_s$sitesite_abstractor)
sites_to_keep <- as.numeric(names(site_counts[site_counts > 0]))
clean_s <- clean_s[clean_s$sitesite_abstractor %in% sites_to_keep, ]

# Create site_detailed that subdivides UW into three sites
clean_s$site_detailed <- as.character(clean_s$sitesite_abstractor)
clean_s$site_detailed[clean_s$sitesite_abstractor == 9 &
    clean_s$uw_site_specificsite_abstractor == 1] <- "9.1"
clean_s$site_detailed[clean_s$sitesite_abstractor == 9 &
    clean_s$uw_site_specificsite_abstractor == 2] <- "9.2"
clean_s$site_detailed[clean_s$sitesite_abstractor == 9 &
    clean_s$uw_site_specificsite_abstractor == 3] <- "9.3"

# Define all site levels and labels
all_site_levels <- c(
    "1", "2", "3", "4", "5", "6", "7",
    "8", "9.1", "9.2", "9.3", "10", "11"
)
all_site_labels <- c(
    "Atrium Health", "Boston Medical Center",
    "Columbia University Medical Center",
    "Grady Health System", "Lyndon B Johnson Hospital",
    "Michigan", "Northwestern", "University of Iowa Hospital & Clinics",
    "Harborview Medical Center",
    "UW Montlake", "UW Northwest",
    "Medical University of South Carolina", "Kaiser Permanente"
)

# Keep only sites present in data
site_detailed_counts <- table(clean_s$site_detailed)
site_levels_to_keep <- all_site_levels[all_site_levels %in% names(site_detailed_counts[site_detailed_counts > 0])]
site_labels_to_keep <- all_site_labels[all_site_levels %in% names(site_detailed_counts[site_detailed_counts > 0])]
clean_s$site_detailed <- factor(clean_s$site_detailed, site_levels_to_keep, site_labels_to_keep)

# ----


clean_s$initial_planoutcomes <- factor(
    clean_s$initial_planoutcomes, 1:2,
    c("Appendectomy", "Antibiotics as Primary Treatment")
)
clean_s$appendicolithoutcomes <- factor(
    clean_s$appendicolithoutcomes,
    c(1, 0, 2), c("Yes", "No", "Not Mentioned")
)

clean_s <- clean_s %>%
    mutate(
        appy_sizeoutcomes = suppressWarnings(as.numeric(appy_sizeoutcomes)),
        wbcoutcomes       = suppressWarnings(as.numeric(wbcoutcomes)),
        nlroutcomes       = suppressWarnings(as.numeric(nlroutcomes))
    )


table1 <- clean_s %>%
    tbl_summary(
        by = initial_planoutcomes,
        include = all_of(vars),
        type = list(
            bmi ~ "continuous2",
            appy_sizeoutcomes ~ "continuous2",
            wbcoutcomes ~ "continuous2",
            nlroutcomes ~ "continuous2"
        ),
        statistic = list(
            all_continuous() ~ c("Median [Min, Max]" = "{median} [{min}, {max}]")
        ),
        digits = all_continuous() ~ 1,
        label = list(
            Age_Categories ~ "Age",
            Sex ~ "Sex",
            RaceEthnicity ~ "Race/Ethnicity",
            appendicolithoutcomes ~ "Appendicolith",
            ccioutcomes ~ "Charlson Comorbidity Index",
            appy_sizeoutcomes ~ "Appendix Diameter (mm)",
            bmi ~ "BMI",
            wbcoutcomes ~ "WBC (10^9/L)",
            nlroutcomes ~ "Neutrophil-Lymphocyte Ratio"
        ),
        missing = "ifany",
        missing_text = "Missing",
        missing_stat = "{N_miss} ({p_miss}%)"
    ) %>%
    add_n() %>%
    modify_header(label = "**Initial Plan Outcomes**") %>%
    bold_labels()

table1

# Save table1 to Word document
library(gt)
library(gtsummary)
gt::gtsave(table1 %>% as_gt(), filename = "table1.docx")





sum(is.na(clean_s$appy_sizeoutcomes))

table(is.na(clean_s$wbcoutcomes), is.na(clean_s$nlroutcomes) | clean_s$nlroutcomes == "")

table(is.na(clean_s$appendicolithoutcomes))

table(clean_s$abscess_combined)


table(is.na(clean_s$appy_sizeoutcomes), clean_s$RaceEthnicity)

table(is.na(clean_s$appy_sizeoutcomes), clean_s$Sex)
table(is.na(clean_s$appy_sizeoutcomes), clean_s$ccioutcomes)

table(is.na(clean_s$appy_sizeoutcomes), clean_s$Age_Categories)







## =========== presentation style separate tables
ibrary(dplyr)
library(gtsummary)

label_map <- c(
    Age_Categories = "Age",
    Sex = "Sex",
    RaceEthnicity = "Race/Ethnicity",
    appendicolithoutcomes = "Appendicolith",
    ccioutcomes = "Charlson Comorbidity Index",
    appy_sizeoutcomes = "Appendix Diameter (mm)",
    bmi = "BMI",
    wbcoutcomes = "WBC (10^9/L)",
    nlroutcomes = "Neutrophil-Lymphocyte Ratio",
    site_detailed = "Site"
)

make_tbl <- function(df, vars, label_map) {
    type_list <- list()
    if ("appy_sizeoutcomes" %in% vars) type_list$appy_sizeoutcomes <- "continuous2"
    if ("wbcoutcomes" %in% vars) type_list$wbcoutcomes <- "continuous2"
    if ("nlroutcomes" %in% vars) type_list$nlroutcomes <- "continuous2"
    if ("bmi" %in% vars) type_list$bmi <- "continuous2"

    label_list <- lapply(vars, function(v) {
        if (!is.na(label_map[v])) rlang::new_formula(rlang::sym(v), label_map[v]) else NULL
    })
    label_list <- Filter(Negate(is.null), label_list)

    df %>%
        filter(!is.na(initial_planoutcomes)) %>%
        tbl_summary(
            by = initial_planoutcomes,
            include = all_of(vars),
            label = label_list,
            type = type_list,
            statistic = list(all_continuous() ~ c("Median [Min, Max]" = "{median} [{min}, {max}]")),
            digits = all_continuous() ~ 1,
            missing = "ifany",
            missing_text = "Missing",
            missing_stat = "{N_miss} ({p_miss}%)"
        ) %>%
        add_n() %>%
        bold_labels()
}

clean_s <- clean_s %>%
    mutate(
        appy_sizeoutcomes = suppressWarnings(as.numeric(appy_sizeoutcomes)),
        wbcoutcomes       = suppressWarnings(as.numeric(wbcoutcomes)),
        nlroutcomes       = suppressWarnings(as.numeric(nlroutcomes))
    )

tbl_demo <- make_tbl(clean_s, c("Age_Categories", "Sex", "RaceEthnicity"), label_map)
tbl_other <- make_tbl(clean_s, c("appendicolithoutcomes", "ccioutcomes", "appy_sizeoutcomes", "bmi", "wbcoutcomes", "nlroutcomes"), label_map)
tbl_labs <- make_tbl(clean_s, c("site_detailed"), label_map)
