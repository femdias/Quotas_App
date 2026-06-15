# Quotas Construction Assumptions

This note documents the inputs, filters, outputs, and construction assumptions for:

- `code/01_SISU_quotas.R`
- `code/02_Sup_Census_quotas.R`

Both scripts define `project_dir`, `raw_dir`, `intermediate_dir` when used, and `output_dir` at the top. Current outputs are written as `.dta` files only.

## 01_SISU_quotas.R

### Inputs

- `data/raw/SISU/SISU aggregated/*.xlsx`
- `data/raw/SISU/DS_MOD_CONCORRENCIA_Unique_Values_CLASSIFIED.xlsx`
- `data/raw/SISU/NO_CURSO_Unique_Values_CLASSIFIED_v2.xlsx`
- `data/raw/Universities_Geoinfo.csv`
- `data/raw/IBGE/Municipality_Regions_Composition.xlsx`

### Outputs

- `data/output/quotas_SISU_univ_year.dta`
- `data/output/quotas_SISU_microreg_year.dta`
- `data/output/quotas_SISU_univ_year_only_univ.dta`
- `data/output/quotas_SISU_microreg_year_only_univ.dta`

### Filters

- Keeps only SISU files with `Vagas ofertadas` in the file name.
- Keeps only first-edition SISU records: `NU_EDICAO == 1`.
- There is no explicit administrative-category filter for public institutions in this script.
- The institutional sample is restricted by academic organization:
  - `DS_ORGANIZACAO_ACADEMICA == "Universidade"`
  - `DS_ORGANIZACAO_ACADEMICA == "Faculdade"`
  - `DS_ORGANIZACAO_ACADEMICA == "Instituto Federal de Educação, Ciência e Tecnologia"`
  - `DS_ORGANIZACAO_ACADEMICA == "Centro Federal de Educação Tecnológica"`
- `CENTRO UNIVERSITÁRIO ESTADUAL DA ZONA OESTE` is dropped because its classification changes over time.
- The `_only_univ` outputs use only:
  - `DS_ORGANIZACAO_ACADEMICA == "Universidade"`

### Assumptions

- SISU is treated as the universe of public admissions offers entering this construction, so the SISU script does not impose an additional public/private administrative-category filter.
- Files named `PORTAL_Sisu 2010 a 2018` are read from sheet 1 with `skip = 4`; later files are read from sheet 2, following the old code.
- Competition modality classifications come from the manually classified file `DS_MOD_CONCORRENCIA_Unique_Values_CLASSIFIED.xlsx`.
- Course-to-major classifications come from the manually classified file `NO_CURSO_Unique_Values_CLASSIFIED_v2.xlsx`.
- Missing competition classifications are assigned to `Others`, as in the old code.
- Missing course classifications are assigned to `Others`, as in the old code.
- `Seats_AA` is the sum of offered seats where `Affirmative_Action == 1`.
- `Seats_Not_reserved` is computed as `Seats_Total - Seats_AA`.
- `Seats_Public_School`, `Seats_Racial`, `Seats_Low_Income`, `Seats_Disability`, and `Seats_Others` are not mutually exclusive. Their sum should not be interpreted as total affirmative-action seats.
- University geography uses `data/raw/Universities_Geoinfo.csv`, keeping only `id_ies` and `id_municipio`.
- The script halts if `Universities_Geoinfo.csv` has missing institution or municipality codes.
- Microregion and state codes are assigned from `Municipality_Regions_Composition.xlsx`.

## 02_Sup_Census_quotas.R

### Inputs

- `data/raw/Superior Census/**/**/*CURSOS*.csv`, excluding files from 2023
- `data/raw/Universities_Geoinfo.csv`
- `data/raw/IBGE/Municipality_Regions_Composition.xlsx`

### Outputs

- `data/output/quotas_Sup_Census_univ_year.dta`
- `data/output/quotas_Sup_Census_microreg_year.dta`
- `data/output/quotas_Sup_Census_univ_year_only_univ.dta`
- `data/output/quotas_Sup_Census_microreg_year_only_univ.dta`

### Filters

- Files from 2023 are excluded explicitly.
- The sample is restricted to public HEIs using:
  - `TP_CATEGORIA_ADMINISTRATIVA %in% c(1, 2, 3)`
  - These codes correspond to federal, state, and municipal institutions.
- The institutional sample is restricted by academic organization using:
  - `TP_ORGANIZACAO_ACADEMICA %in% c(1, 3, 4, 5)`
  - These codes correspond to universities, faculties, federal institutes, and federal technological centers.
- Courses with zero total entrants are dropped:
  - `Seats_Total != 0`
- The `_only_univ` outputs use only:
  - `TP_ORGANIZACAO_ACADEMICA == 1`

### Assumptions

- The Superior Census files do not provide the same reserved offered-seat structure as SISU. The script uses entrant reservation variables as the available proxy.
- `Seats_Total` is based on `QT_ING`, total entrants.
- `Seats_AA` is based on `QT_ING_RESERVA_VAGA`, total entrants through reserved vacancies.
- `Seats_Not_reserved` is computed as `Seats_Total - Seats_AA`.
- Reservation subcategories are based on `QT_ING_RVREDEPUBLICA`, `QT_ING_RVETNICO`, `QT_ING_RVPDEF`, `QT_ING_RVSOCIAL_RF`, and `QT_ING_RVOUTROS`.
- Reservation subcategories are not mutually exclusive. Their sum should not be interpreted as total affirmative-action seats.
- The script caps course-level `Seats_AA` at `Seats_Total` before aggregation to prevent impossible reserved totals.
- University geography uses `data/raw/Universities_Geoinfo.csv`, keeping only `id_ies` and `id_municipio`.
- The script halts if `Universities_Geoinfo.csv` has missing institution or municipality codes.
- Microregion and state codes are assigned from `Municipality_Regions_Composition.xlsx`.
- The USP correction block from the old code is currently kept as a commented block and is not applied.
