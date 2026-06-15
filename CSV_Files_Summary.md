# CSV Files Summary

## Generated Output CSVs

### `data/output/quotas_SISU_univ_year.csv`

SISU quota dataset at the university-year level. One row per `CO_IES` and `year`, with total offered SISU seats, affirmative-action seats, non-reserved seats, quota-category seat counts, and corresponding shares. Includes `micro_reg_code` and `state_code`.

### `data/output/quotas_SISU_microreg_year.csv`

SISU quota dataset aggregated to the microregion-year level. One row per `micro_reg_code` and `year`, summing university-level SISU seats and recalculating quota shares.

### `data/output/quotas_Sup_Census_univ_year.csv`

Superior Census quota dataset at the university-year level. One row per `co_ies` and `year`, using entrant reservation variables: `QT_ING` as total admissions and `QT_ING_RESERVA_VAGA` as affirmative-action admissions. Includes quota subcategories, shares, `micro_reg_code`, and `state_code`.

### `data/output/quotas_Sup_Census_microreg_year.csv`

Superior Census quota dataset aggregated to the microregion-year level. One row per `micro_reg_code` and `year`, summing university-level Superior Census admissions and recalculating quota shares.

## Raw CSVs

### `data/raw/Universities_Geoinfo.csv`

University geolocation lookup. Main columns used by the code are `id_ies` and `id_municipio`. `id_ies` is the university code; `id_municipio` is the municipality code used to merge with IBGE geography.

### `data/raw/Superior Census/**/MICRODADOS_CADASTRO_CURSOS_YYYY.CSV`

Raw Higher Education Census course-level files. Used by `02_Sup_Census_quotas.R`. The code uses the course files from 2009-2022 and excludes 2023. Relevant variables include institution code, course code, municipality, public/private category, academic organization, total entrants, and reserved-vacancy entrant counts.

### `data/raw/Superior Census/**/MICRODADOS_CADASTRO_IES_YYYY.CSV` or `MICRODADOS_ED_SUP_IES_YYYY.CSV`

Raw Higher Education Census institution-level files. These are present in the folder but are not used by the current scripts.

### `data/raw/Superior Census/**/MICRODADOS_CADASTRO_CURSOS_2023.CSV`

Raw 2023 course-level Census file. Present in the folder but explicitly excluded in `02_Sup_Census_quotas.R`.

### `data/raw/Superior Census/**/MICRODADOS_ED_SUP_IES_2023.CSV`

Raw 2023 institution-level Census file. Present but not used.
