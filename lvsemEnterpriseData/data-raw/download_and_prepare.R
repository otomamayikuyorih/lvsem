#!/usr/bin/env Rscript

required <- c("jsonlite", "readxl")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing required packages: ", paste(missing, collapse = ", "), call. = FALSE)
}

root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
raw_root <- file.path(root, "inst", "extdata", "raw")
data_root <- file.path(root, "data")
dir.create(raw_root, recursive = TRUE, showWarnings = FALSE)
dir.create(data_root, recursive = TRUE, showWarnings = FALSE)

enterprise_metadata <- data.frame(
  no = c(1, 3, 4, 6, 7, 9, 10, 11, 13, 19, 20, 21, 22, 23, 24, 25, 27, 28, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 41),
  dataset_id = c(
    "agile_methodology_acceptance",
    "ai_adoption_digital_capability",
    "mexican_enterprise_digitalization",
    "green_transformational_leadership",
    "inclusive_leadership_distress",
    "vietnam_soe_commitment_job_performance",
    "vietnam_electronics_pos_stay",
    "vietnam_hospitality_supervisor_support",
    "corporate_governance_firm_value",
    "vietnam_sme_ip_management",
    "msme_ai_chatbot_adoption",
    "vietnam_sme_digital_procurement",
    "sme_digital_capability_green_innovation",
    "msme_digital_transformation_capabilities",
    "ghana_ai_innovation_readiness",
    "malaysia_sme_entrepreneurship_orientation",
    "supply_chain_risk_order_costing",
    "personality_org_support_creativity",
    "transformational_leadership_employee_performance",
    "meaningful_work_job_satisfaction",
    "digital_hr_bank_employee_performance",
    "turnover_intention_generational_differences",
    "bank_justice_turnover_fwa",
    "coal_mining_coworker_support",
    "sme_empowering_leadership_iwb",
    "hotel_empowering_leadership_iwb",
    "iran_hotel_role_conflict_turnover",
    "green_hotel_gai_employee_happiness",
    "workplace_ostracism_engagement"
  ),
  title = c(
    "Dataset and Analysis Procedure for Job-Work Fit as a Determinant of the Acceptance of Large-Scale Agile Methodology",
    "AI Adoption, Digital Capability, and Innovation Capability: The Mediating Role of Strategic Decision-Making Quality",
    "Digitalization in Mexican enterprises",
    "Green transformational leadership and its impact on employee environmental performance",
    "How and When Does Inclusive Leadership Curb Psychological Distress during a Crisis?",
    "Dataset Examining the Relationship Between Organizational Commitment and Job Performance in Vietnam's Water Supply Sector",
    "Perceived organizational support and its impact on employee's intention to stay: Dataset from the electronics industry in Vietnam",
    "Perceived supervisor support and its impact to job performance: Dataset from the hospitality industry in Vietnam",
    "Corporate governance, intellectual capital, and organizational culture: their influence on lean manufacturing and firm value",
    "The impacts of intellectual property management on firm performance: Innovation as a mediator",
    "Integration of AI Chatbots in MSMEs",
    "Open Innovation Capability and Digital Procurement Transformation in Vietnamese SMEs",
    "Digital Capability and Green Innovation as Drivers of Sustainable SME Performance",
    "The Strategic Role of Digital Transformation, Dynamic and Agile Capabilities for the Performance of MSMEs",
    "Ghana AI Innovation Readiness Dataset",
    "Entrepreneurship orientation, open innovation and cost leadership towards firm performance among SMEs in Malaysia",
    "Supply Chain Risk Management and Order-Based Costing",
    "Personality and Organizational Support: Their Role in Creativity",
    "Job satisfaction and organizational commitment effect in transformational leadership towards employee performance",
    "The Relationship between Meaningful Work and Job Satisfaction",
    "Innovative HR Strategies and Digital HR Technologies Driving Employee Performance in Digital Transformation",
    "Determinants of turnover intention with generational differences",
    "The Role of Organizational Justice in Predicting Job Satisfaction and Turnover Intention",
    "Data Respondent Coworker Support Survey in Coal Mining Industry",
    "Dataset empowering leadership and innovative work behavior of SME employees",
    "Dataset of innovative behavior in the context of hotel industry",
    "Research Project on Iranian Hotel Employees",
    "Generative artificial intelligence as a catalyst for employee happiness and sustainable performance in green hotel management",
    "Moderating influence of Employee Engagement between Workplace Ostracism and outcomes"
  ),
  repository = c(rep("mendeley", 8), "figshare", rep("mendeley", 20)),
  repository_id = c(
    "227b5x4vg6",
    "fs78cfs84v",
    "d9y59jdh5k",
    "3wgj86mph4",
    "4vg8xtsw5t",
    "tbjdwkzr57",
    "pyjkvgjmfz",
    "c5dwcxwnyj",
    "26384874",
    "gf9rv5j2c3",
    "x47h2kz4n3",
    "2ybcnf5c5b",
    "rbwyfhvb9k",
    "f57g886dvk",
    "fc669t4kft",
    "fnj7tswxvg",
    "bh5y663jnd",
    "y5r4r288sh",
    "fn9gdmmx53",
    "hdmr6hng47",
    "xbg5gzskfv",
    "y6bv87c3m4",
    "nkxjhw8dbc",
    "w36pwt8t6c",
    "nvykn26cy6",
    "5hnzsk4hx5",
    "tbmzzsz6m3",
    "999m5mhdkx",
    "6s92wr9642"
  ),
  source_url = c(
    "https://data.mendeley.com/datasets/227b5x4vg6/2",
    "https://data.mendeley.com/datasets/fs78cfs84v/2",
    "https://data.mendeley.com/datasets/d9y59jdh5k/1",
    "https://data.mendeley.com/datasets/3wgj86mph4",
    "https://data.mendeley.com/datasets/4vg8xtsw5t/1",
    "https://data.mendeley.com/datasets/tbjdwkzr57/2",
    "https://data.mendeley.com/datasets/pyjkvgjmfz/2",
    "https://data.mendeley.com/datasets/c5dwcxwnyj/1",
    "https://tandf.figshare.com/articles/dataset/Corporate_governance_intellectual_capital_and_organizational_culture_their_influence_on_lean_manufacturing_and_firm_value/26384874",
    "https://data.mendeley.com/datasets/gf9rv5j2c3/1",
    "https://data.mendeley.com/datasets/x47h2kz4n3",
    "https://data.mendeley.com/datasets/2ybcnf5c5b",
    "https://data.mendeley.com/datasets/rbwyfhvb9k",
    "https://data.mendeley.com/datasets/f57g886dvk/2",
    "https://data.mendeley.com/datasets/fc669t4kft",
    "https://data.mendeley.com/datasets/fnj7tswxvg/2",
    "https://data.mendeley.com/datasets/bh5y663jnd/1",
    "https://data.mendeley.com/datasets/y5r4r288sh",
    "https://data.mendeley.com/datasets/fn9gdmmx53/1",
    "https://data.mendeley.com/datasets/hdmr6hng47",
    "https://data.mendeley.com/datasets/xbg5gzskfv",
    "https://data.mendeley.com/datasets/y6bv87c3m4",
    "https://data.mendeley.com/datasets/nkxjhw8dbc",
    "https://data.mendeley.com/datasets/w36pwt8t6c/1",
    "https://data.mendeley.com/datasets/nvykn26cy6/1",
    "https://data.mendeley.com/datasets/5hnzsk4hx5/2",
    "https://data.mendeley.com/datasets/tbmzzsz6m3/2",
    "https://data.mendeley.com/datasets/999m5mhdkx",
    "https://data.mendeley.com/datasets/6s92wr9642"
  ),
  license = "CC BY 4.0",
  license_url = "https://creativecommons.org/licenses/by/4.0/",
  enterprise_data_degree = "high",
  included = TRUE,
  stringsAsFactors = FALSE
)

sanitize_filename <- function(x) {
  ext <- tools::file_ext(x)
  stem <- tools::file_path_sans_ext(x)
  stem <- gsub("[<>:\"/\\\\|?*]+", "_", stem)
  stem <- gsub("\\s+", "_", stem)
  stem <- gsub("[^A-Za-z0-9._-]+", "_", stem)
  stem <- substr(stem, 1, 38)
  if (nzchar(ext)) {
    paste0(stem, ".", ext)
  } else {
    stem
  }
}

download_one <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    return(dest)
  }
  message("Downloading: ", basename(dest))
  utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
  dest
}

mendeley_files <- function(dataset_id) {
  api <- sprintf("https://data.mendeley.com/api/datasets/%s/files", dataset_id)
  out <- jsonlite::fromJSON(api, simplifyVector = FALSE)
  if (!length(out)) {
    return(data.frame(filename = character(), download_url = character()))
  }
  data.frame(
    filename = vapply(out, function(x) x$filename %||% x$name %||% x$id, character(1)),
    download_url = vapply(out, function(x) x$content_details$download_url, character(1)),
    stringsAsFactors = FALSE
  )
}

figshare_files <- function(article_id) {
  api <- sprintf("https://api.figshare.com/v2/articles/%s", article_id)
  out <- jsonlite::fromJSON(api, simplifyVector = FALSE)
  files <- out$files
  data.frame(
    filename = vapply(files, function(x) x$name, character(1)),
    download_url = vapply(files, function(x) x$download_url, character(1)),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

read_table_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    return(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE))
  }
  if (ext %in% c("tsv", "txt")) {
    return(utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE))
  }
  if (ext %in% c("xlsx", "xls", "xlsm")) {
    sheets <- readxl::excel_sheets(path)
    stats::setNames(lapply(sheets, function(sheet) {
      readxl::read_excel(path, sheet = sheet)
    }), sheets)
  } else if (ext == "sav") {
    foreign::read.spss(path, to.data.frame = TRUE, use.value.labels = FALSE)
  } else {
    NULL
  }
}

enterprise_datasets <- list()

for (i in seq_len(nrow(enterprise_metadata))) {
  meta <- enterprise_metadata[i, , drop = FALSE]
  id <- meta$dataset_id
  message("\n== ", meta$no, " ", id, " ==")
  dataset_dir <- file.path(raw_root, sprintf("e%02d", meta$no))
  dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)

  files <- if (meta$repository == "mendeley") {
    mendeley_files(meta$repository_id)
  } else if (meta$repository == "figshare") {
    figshare_files(meta$repository_id)
  } else {
    data.frame(filename = character(), download_url = character())
  }

  if (!nrow(files)) {
    warning("No downloadable files found for ", id)
  }

  raw_files <- character()
  for (j in seq_len(nrow(files))) {
    dest <- file.path(dataset_dir, sprintf("%02d_%s", j, sanitize_filename(files$filename[j])))
    raw_files <- c(raw_files, download_one(files$download_url[j], dest))
  }

  table_files <- raw_files[tolower(tools::file_ext(raw_files)) %in% c("csv", "tsv", "txt", "xlsx", "xls", "xlsm", "sav")]
  tables <- list()
  for (path in table_files) {
    nm <- tools::file_path_sans_ext(basename(path))
    message("Reading table: ", basename(path))
    tables[[nm]] <- tryCatch(
      read_table_file(path),
      error = function(e) {
        warning("Could not read ", path, ": ", conditionMessage(e))
        NULL
      }
    )
  }

  enterprise_datasets[[id]] <- list(
    metadata = meta,
    raw_files = sub(paste0("^", gsub("\\\\", "/", root), "/"), "", normalizePath(raw_files, winslash = "/", mustWork = FALSE)),
    tables = tables
  )
}

enterprise_metadata$raw_file_count <- vapply(enterprise_metadata$dataset_id, function(id) {
  length(enterprise_datasets[[id]]$raw_files)
}, integer(1))
enterprise_metadata$table_count <- vapply(enterprise_metadata$dataset_id, function(id) {
  length(enterprise_datasets[[id]]$tables)
}, integer(1))
enterprise_metadata$has_tabular_data <- enterprise_metadata$table_count > 0
enterprise_metadata$included <- enterprise_metadata$has_tabular_data

for (id in enterprise_metadata$dataset_id) {
  enterprise_datasets[[id]]$metadata <- enterprise_metadata[enterprise_metadata$dataset_id == id, , drop = FALSE]
}

save(enterprise_metadata, file = file.path(data_root, "enterprise_metadata.rda"), compress = "xz")
save(enterprise_datasets, file = file.path(data_root, "enterprise_datasets.rda"), compress = "xz")

message("\nSaved:")
message("  ", file.path(data_root, "enterprise_metadata.rda"))
message("  ", file.path(data_root, "enterprise_datasets.rda"))
