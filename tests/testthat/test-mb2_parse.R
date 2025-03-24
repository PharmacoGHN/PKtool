

# Input:
admin_data <- tibble::tribble(
    ~admin_date, ~route, ~infusion_rate, ~infusion_duration, ~dose, ~ccr,
    "2025/03/07 10:55",	"IV",	3000,	0.5,	1500,	163.6,
    "2025/03/08 10:55",	"IV",	3000,	0.5,	1500,	163.6,
    "2025/03/09 10:55",	"IV",	3000,	0.5,	1500,	163.6,
    "2025/03/10 10:55",	"IV",	3000,	0.5,	1500,	163.6,
    "2025/03/11 10:55",	"IV",	3000,	0.5,	1500,	163.6
)

tdm_data <- tibble::tribble(
  ~tdm_date, ~tdm_value,
  "2025/03/08 10:50",	5,
  "2025/03/09 10:50",	20
)

weight_data <- tibble::tribble(
  ~weight_date, ~weight_value,
  "2025/03/07 10:55",	70,
  "2025/03/09 10:55",	75,
)

# Output:
pk_data <- tibble::tribble(
  ~date, ~time, ~route, ~infusion_rate, ~infusion_duration, ~dose, ~ccr, ~tdm_value, ~weight_value,
  "2025/03/07", "10:55",	"IV",	3000,	0.5,	1500,	163.6,	NA,	70,
  "2025/03/08", "10:50",	"IV",	3000,	0.5,	1500,	163.6,	5,	70,
  "2025/03/08", "10:55",	"IV",	3000,	0.5,	1500,	163.6,  NA,	70,
  "2025/03/09", "10:50",	"IV",	3000,	0.5,	1500,	163.6,	20,	75,
  "2025/03/09", "10:55",	"IV",	3000,	0.5,	1500,	163.6,	NA,	75,
  "2025/03/10", "10:55",	"IV",	3000,	0.5,	1500,	163.6,	NA,	75,
  "2025/03/11", "10:55",	"IV",	3000,	0.5,	1500,	163.6,	NA,	75
)

# mb2 file parse
read_file.mb2("dev/CEFE_DOJH.mb2")