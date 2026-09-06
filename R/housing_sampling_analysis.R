# =========================================================
# Housing Sampling Project: Stratified One-Stage Cluster Design
# =========================================================

library(dplyr)
library(tidyr)

set.seed(123)

# ---------------------------------------------------------
# 1. Read data
# ---------------------------------------------------------

dir.create("output", showWarnings = FALSE)\n\ndat <- read.csv(file.path("data", "housing_sample.csv"))\n\n# ---------------------------------------------------------
# 2. Create new variables
# ---------------------------------------------------------

dat <- dat %>%
  mutate(
    rent_per_person = price_per_unit / bedrooms_per_unit,
    area = factor(area),
    type = factor(type),
    affordable_1200 = ifelse(rent_per_person <= 1200, 1, 0),
    affordable_1500 = ifelse(rent_per_person <= 1500, 1, 0)
  )

# ---------------------------------------------------------
# 3. Population information
# ---------------------------------------------------------

pop_area <- data.frame(
  area = c("Ardmore", "BrynMawr", "Haverford", "Wayne"),
  N_h = c(32, 24, 8, 40)
)

N_total <- sum(pop_area$N_h)

pop_area <- pop_area %>%
  mutate(W_h = N_h / N_total)

# ---------------------------------------------------------
# 4. Sample allocation check
# ---------------------------------------------------------

sample_allocation <- pop_area %>%
  mutate(
    n_h_final = case_when(
      area == "Ardmore" ~ 11,
      area == "BrynMawr" ~ 10,
      area == "Haverford" ~ 6,
      area == "Wayne" ~ 13
    )
  )

sample_check <- dat %>%
  distinct(area, listing_id) %>%
  count(area, name = "n_h_actual") %>%
  left_join(sample_allocation, by = "area")

# ---------------------------------------------------------
# 5. Count sampled listings correctly
# ---------------------------------------------------------

get_n_used <- function(dat) {
  
  if ("boot_listing_id" %in% names(dat)) {
    dat %>%
      distinct(area, boot_listing_id) %>%
      count(area, name = "n_used")
  } else {
    dat %>%
      distinct(area, listing_id) %>%
      count(area, name = "n_used")
  }
}

# =========================================================
# PART A: OVERALL MEAN RENT PER PERSON
# Stratified ratio estimator by area
# =========================================================

estimate_overall_mean <- function(dat, pop_area) {
  
  dat %>%
    group_by(area) %>%
    summarise(
      total_rent_per_person = sum(rent_per_person, na.rm = TRUE),
      total_units = n(),
      mean_rent_per_person_h = total_rent_per_person / total_units,
      .groups = "drop"
    ) %>%
    left_join(pop_area, by = "area") %>%
    summarise(
      group = "Overall",
      estimate = sum(W_h * mean_rent_per_person_h, na.rm = TRUE)
    )
}

overall_mean_result <- estimate_overall_mean(dat, pop_area)

# =========================================================
# PART B: MEAN RENT PER PERSON BY HOUSING TYPE
# Direct ratio estimator by housing type
# =========================================================

estimate_mean_by_type <- function(dat) {
  
  dat %>%
    group_by(type) %>%
    summarise(
      total_rent_per_person = sum(rent_per_person, na.rm = TRUE),
      total_units = n(),
      estimate = total_rent_per_person / total_units,
      .groups = "drop"
    ) %>%
    rename(group = type) %>%
    mutate(group = as.character(group))
}

mean_by_type_result <- estimate_mean_by_type(dat)

# =========================================================
# PART C: ESTIMATED TOTAL NUMBER OF UNITS
# Overall, Shared, and Private
# =========================================================

estimate_total_units <- function(dat, pop_area) {
  
  n_used_df <- get_n_used(dat)
  
  overall <- dat %>%
    group_by(area) %>%
    summarise(
      sample_total_units = n(),
      .groups = "drop"
    ) %>%
    left_join(pop_area, by = "area") %>%
    left_join(n_used_df, by = "area") %>%
    mutate(
      est_total_units = (N_h / n_used) * sample_total_units
    ) %>%
    summarise(
      group = "Overall",
      estimate = sum(est_total_units, na.rm = TRUE)
    )
  
  by_type <- dat %>%
    group_by(area, type) %>%
    summarise(
      sample_total_units = n(),
      .groups = "drop"
    ) %>%
    left_join(pop_area, by = "area") %>%
    left_join(n_used_df, by = "area") %>%
    mutate(
      est_total_units = (N_h / n_used) * sample_total_units
    ) %>%
    group_by(type) %>%
    summarise(
      group = as.character(first(type)),
      estimate = sum(est_total_units, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(group, estimate)
  
  bind_rows(overall, by_type)
}

total_units_result <- estimate_total_units(dat, pop_area)

# =========================================================
# PART D: AFFORDABILITY ESTIMATION
# =========================================================

estimate_affordability <- function(dat, pop_area, threshold_var) {
  
  n_used_df <- get_n_used(dat)
  
  area_result <- dat %>%
    group_by(area) %>%
    summarise(
      sample_total_units = n(),
      sample_affordable_units = sum(.data[[threshold_var]], na.rm = TRUE),
      sample_affordable_shared = sum(.data[[threshold_var]] == 1 & type == "Shared", na.rm = TRUE),
      sample_affordable_private = sum(.data[[threshold_var]] == 1 & type == "Private", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(pop_area, by = "area") %>%
    left_join(n_used_df, by = "area") %>%
    mutate(
      est_total_units = (N_h / n_used) * sample_total_units,
      est_affordable_units = (N_h / n_used) * sample_affordable_units,
      est_affordable_shared = (N_h / n_used) * sample_affordable_shared,
      est_affordable_private = (N_h / n_used) * sample_affordable_private,
      affordability_prop = est_affordable_units / est_total_units,
      shared_among_affordable = est_affordable_shared / est_affordable_units,
      private_among_affordable = est_affordable_private / est_affordable_units
    ) %>%
    select(
      area,
      est_total_units,
      est_affordable_units,
      affordability_prop,
      est_affordable_shared,
      est_affordable_private,
      shared_among_affordable,
      private_among_affordable
    )
  
  overall_result <- area_result %>%
    summarise(
      area = "Overall",
      est_total_units = sum(est_total_units, na.rm = TRUE),
      est_affordable_units = sum(est_affordable_units, na.rm = TRUE),
      affordability_prop = est_affordable_units / est_total_units,
      est_affordable_shared = sum(est_affordable_shared, na.rm = TRUE),
      est_affordable_private = sum(est_affordable_private, na.rm = TRUE),
      shared_among_affordable = est_affordable_shared / est_affordable_units,
      private_among_affordable = est_affordable_private / est_affordable_units
    )
  
  bind_rows(area_result, overall_result)
}

afford_1200_result <- estimate_affordability(dat, pop_area, "affordable_1200")
afford_1500_result <- estimate_affordability(dat, pop_area, "affordable_1500")

# =========================================================
# PART E: BOOTSTRAP SAMPLE
# Cluster bootstrap within strata using n_h - 1 rule
# =========================================================

bootstrap_sample_n_minus_1 <- function(dat) {
  
  dat %>%
    group_by(area) %>%
    group_modify(~ {
      
      listings <- unique(.x$listing_id)
      n_h <- length(listings)
      
      boot_listings <- sample(
        listings,
        size = n_h - 1,
        replace = TRUE
      )
      
      bind_rows(
        lapply(seq_along(boot_listings), function(k) {
          .x %>%
            filter(listing_id == boot_listings[k]) %>%
            mutate(
              boot_listing_id = paste0(boot_listings[k], "_boot_", k)
            )
        })
      )
    }) %>%
    ungroup()
}

# ---------------------------------------------------------
# Bootstrap 1: overall mean rent per person
# ---------------------------------------------------------

bootstrap_overall_mean <- function(dat, pop_area, B = 1000) {
  
  boot_estimates <- numeric(B)
  
  for (b in 1:B) {
    boot_dat <- bootstrap_sample_n_minus_1(dat)
    boot_estimates[b] <- estimate_overall_mean(boot_dat, pop_area)$estimate
  }
  
  data.frame(
    group = "Overall",
    estimate = estimate_overall_mean(dat, pop_area)$estimate,
    bootstrap_se = sd(boot_estimates, na.rm = TRUE),
    ci_lower = quantile(boot_estimates, 0.025, na.rm = TRUE),
    ci_upper = quantile(boot_estimates, 0.975, na.rm = TRUE)
  )
}

boot_overall_mean_result <- bootstrap_overall_mean(dat, pop_area, B = 1000)

# ---------------------------------------------------------
# Bootstrap 2: mean rent per person by type
# ---------------------------------------------------------

bootstrap_mean_by_type <- function(dat, B = 1000) {
  
  groups <- sort(unique(as.character(dat$type)))
  
  boot_results <- data.frame(
    matrix(NA, nrow = B, ncol = length(groups))
  )
  
  colnames(boot_results) <- groups
  
  for (b in 1:B) {
    
    boot_dat <- bootstrap_sample_n_minus_1(dat)
    boot_est <- estimate_mean_by_type(boot_dat)
    
    for (g in groups) {
      boot_results[b, g] <- boot_est$estimate[boot_est$group == g]
    }
  }
  
  original_est <- estimate_mean_by_type(dat)
  
  original_est %>%
    rowwise() %>%
    mutate(
      bootstrap_se = sd(boot_results[[group]], na.rm = TRUE),
      ci_lower = quantile(boot_results[[group]], 0.025, na.rm = TRUE),
      ci_upper = quantile(boot_results[[group]], 0.975, na.rm = TRUE)
    ) %>%
    ungroup()
}

boot_mean_by_type_result <- bootstrap_mean_by_type(dat, B = 1000)

# ---------------------------------------------------------
# Bootstrap 3: total units
# ---------------------------------------------------------

bootstrap_total_units <- function(dat, pop_area, B = 1000) {
  
  groups <- c("Overall", sort(unique(as.character(dat$type))))
  
  boot_results <- data.frame(
    matrix(NA, nrow = B, ncol = length(groups))
  )
  
  colnames(boot_results) <- groups
  
  for (b in 1:B) {
    
    boot_dat <- bootstrap_sample_n_minus_1(dat)
    boot_est <- estimate_total_units(boot_dat, pop_area)
    
    for (g in groups) {
      boot_results[b, g] <- boot_est$estimate[boot_est$group == g]
    }
  }
  
  original_est <- estimate_total_units(dat, pop_area)
  
  original_est %>%
    rowwise() %>%
    mutate(
      bootstrap_se = sd(boot_results[[group]], na.rm = TRUE),
      ci_lower = quantile(boot_results[[group]], 0.025, na.rm = TRUE),
      ci_upper = quantile(boot_results[[group]], 0.975, na.rm = TRUE)
    ) %>%
    ungroup()
}

boot_total_units_result <- bootstrap_total_units(dat, pop_area, B = 1000)

# ---------------------------------------------------------
# Bootstrap 4: affordability
# ---------------------------------------------------------

bootstrap_affordability <- function(dat, pop_area, threshold_var, B = 1000) {
  
  boot_estimates <- numeric(B)
  
  for (b in 1:B) {
    
    boot_dat <- bootstrap_sample_n_minus_1(dat)
    
    boot_result <- estimate_affordability(boot_dat, pop_area, threshold_var)
    
    boot_estimates[b] <- boot_result %>%
      filter(area == "Overall") %>%
      pull(affordability_prop)
  }
  
  original_result <- estimate_affordability(dat, pop_area, threshold_var) %>%
    filter(area == "Overall") %>%
    pull(affordability_prop)
  
  data.frame(
    threshold = threshold_var,
    estimate = original_result,
    bootstrap_se = sd(boot_estimates, na.rm = TRUE),
    ci_lower = quantile(boot_estimates, 0.025, na.rm = TRUE),
    ci_upper = quantile(boot_estimates, 0.975, na.rm = TRUE)
  )
}

boot_afford_1200_result <- bootstrap_affordability(dat, pop_area, "affordable_1200", B = 1000)
boot_afford_1500_result <- bootstrap_affordability(dat, pop_area, "affordable_1500", B = 1000)

# =========================================================
# FINAL OUTPUTS
# =========================================================

sample_allocation
sample_check

overall_mean_result
mean_by_type_result
total_units_result

print(afford_1200_result, width = Inf)
print(afford_1500_result, width = Inf)

boot_overall_mean_result
boot_mean_by_type_result
boot_total_units_result
boot_afford_1200_result
boot_afford_1500_result


# Export reproducible result tables
write.csv(sample_check, file.path("output", "sample_allocation_check.csv"), row.names = FALSE)
write.csv(boot_overall_mean_result, file.path("output", "mean_rent_overall.csv"), row.names = FALSE)
write.csv(boot_mean_by_type_result, file.path("output", "mean_rent_by_type.csv"), row.names = FALSE)
write.csv(boot_total_units_result, file.path("output", "estimated_total_units.csv"), row.names = FALSE)
write.csv(afford_1200_result, file.path("output", "affordability_1200_by_area.csv"), row.names = FALSE)
write.csv(afford_1500_result, file.path("output", "affordability_1500_by_area.csv"), row.names = FALSE)
write.csv(bind_rows(boot_afford_1200_result, boot_afford_1500_result),
          file.path("output", "affordability_bootstrap.csv"), row.names = FALSE)
