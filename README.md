# Cash Transfer Analysis (Stata)

End-to-end Stata workflow for analyzing a village/household RCT with treatment, spillover, and control groups.

This repository includes:
- `cash_transfer_distribution_analysis.do` — main analysis script  
- `analysis_data.dta` — **synthetic dataset** shaped to match the script’s expectations (120 villages × 1,440 households)

> The dataset is synthetic and provided so the script runs out-of-the-box. Replace it with your own data as needed.

---

## 1) Prerequisites

- **Stata** (v15+ recommended; the `.dta` is saved in Stata 13+ format)  
- Stata package **estout** (for `esttab` / `estpost`):
  ```stata
  ssc install estout
  ```

---

## 2) Repository layout

```
.
├── cash_transfer_distribution_analysis.do
├── analysis_data.dta
└── (outputs are created at runtime)
```

If you prefer subfolders (recommended), the do-file supports:
```
<project root>/
  data/analysis_data.dta
  output/
  logs/
```

---

## 3) Setup

> ⚠️ **Windows path caveat:** avoid `~` (home shortcut) — it may not expand. Use an **absolute** path.

### macOS / Linux
In Stata:
```stata
* Use the repo folder as the base
global main_dir "`c(pwd)'"
global data_dir "$main_dir/data"
global output_dir "$main_dir/output"
global log_dir "$main_dir/logs"

cap mkdir "$data_dir"
cap mkdir "$output_dir"
cap mkdir "$log_dir"
```
Place the dataset at: `data/analysis_data.dta`.

### Windows
In Stata:
```stata
global main_dir "C:\Users\<you>\path\to\repo"
global data_dir "$main_dir\data"
global output_dir "$main_dir\output"
global log_dir "$main_dir\logs"

cap mkdir "$data_dir"
cap mkdir "$output_dir"
cap mkdir "$log_dir"
```
Place the dataset at: `data\analysis_data.dta`.

---

## 4) Quick start

1) **Install estout** (once):
```stata
ssc install estout
```

2) **Set folders** (see Setup above) and confirm the dataset exists at:
```
$data_dir/analysis_data.dta
```

3) **Run the analysis**:
```stata
do cash_transfer_distribution_analysis.do
```

Outputs (tables/figures) will be created under `output/` and logs under `logs/` (if enabled in the script).

---

## 5) What the script does

- Loads `analysis_data.dta` (village/household structure; 0=control, 1=treatment, 2=spillover)  
- Uses baseline and endline variables; computes changes  
- Runs regressions with **factor variables** (e.g., `i.treatment_group`) and **village-clustered** SEs (`vce(cluster village_id)`)  
- Exports publication-style tables with `esttab` and figures with `graph export`

---

## 6) Optional “preflight” checks (add to the top of the .do)

```stata
version 17
set more off

* estout installed?
capture which esttab
if _rc {
    di as error "Required package 'estout' not found. Run: ssc install estout"
    exit 499
}

* dataset present?
capture confirm file "$data_dir/analysis_data.dta"
if _rc {
    di as error "Data not found at: $data_dir/analysis_data.dta"
    di as text  "Place analysis_data.dta there or update $data_dir."
    exit 601
}
```

---

## 7) Troubleshooting

- **`command esttab not found`**  
  Install estout: `ssc install estout`

- **“file not found: analysis_data.dta”**  
  Check your `$main_dir` / `$data_dir` and ensure the dataset is at `$data_dir/analysis_data.dta`.  
  On Windows, don’t use `~`; use an absolute path.

- **Permission denied (cannot create output/logs)**  
  Choose a writable `main_dir` (e.g., within your user folder) and re-run the `mkdir` lines.

---

## 8) Using your own data

To swap in real data, either:
- Keep the same variable names/structure as `analysis_data.dta`, **or**
- Update the variable mapping in the do-file where the dataset is loaded (IDs, treatment coding, outcomes, etc.).

---

## 9) License

- Code: choose an OSI-approved license (e.g., MIT).  
- Synthetic data: for demonstration/testing only. Respect licenses/terms for any external data you use.
