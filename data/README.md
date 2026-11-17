# Data

## Sources

| Data | Producer | Licence | How it is obtained |
|---|---|---|---|
| Annual road injury accident databases (BAAC), 2005-2025: characteristics, places, vehicles, users | ONISR, via [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/bases-de-donnees-annuelles-des-accidents-corporels-de-la-circulation-routiere-annees-de-2005-a-2025/) | Licence Ouverte / Open Licence 2.0 | `R/01_download.R` (84 CSV files, about 470 MB) |
| Population estimates by department, 1975-2023 | [INSEE](https://www.insee.fr/fr/statistiques/1893198) | Open licence | `R/01_download.R` |
| Simplified department boundaries | [france-geojson](https://github.com/gregoiredavid/france-geojson), derived from IGN ADMIN EXPRESS | Licence Ouverte | `R/01_download.R` |

The raw and intermediate files are not versioned (`.gitignore`). Running `R/01_download.R` rebuilds
`data/raw/` from the official sources, and `R/02` to `R/04` rebuild `data/interim/` and `data/final/`.

## What a record is

The BAAC covers **injury accidents only**: an accident is recorded when at least one person needs medical
care, on a road open to public traffic, with at least one vehicle involved. Accidents with material damage
only are not in the data. Every analysis of severity in this project is therefore conditional on an injury
accident having happened.

Severity of each person (`grav`), as defined by ONISR:

- **Killed**: died on the spot or within 30 days
- **Hospitalised**: admitted to hospital for more than 24 hours
- **Slightly injured**: received medical care, not hospitalised for more than 24 hours
- **Unharmed**: involved, no medical care needed

## Known breaks and quirks, and how they are handled

| Issue | Years | Handling |
|---|---|---|
| "Hospitalised" recorded differently by the police; ONISR warns it is not comparable with earlier years and no longer labels it as an official statistic | from 2018 | Severity rates and models use 2019-2025 only. Charts mark the break. |
| New vehicle categories (e-scooters, e-bikes, three-wheelers) and new safety-equipment coding | from 2019 | Vehicle categories mapped to common groups (`R/04_features.R`). |
| Overseas collectivities (Polynesia, New Caledonia, Saint-Martin, ...) recorded | from 2019 | Excluded: scope is mainland France and the five overseas departments for every year. |
| Hit-and-run users added, often without age, sex or severity | from 2021 | Users with unknown severity excluded from severity analyses (418 people). |
| Several place rows for one accident (one per road at an intersection) | from 2023 | Keep the main road (lowest road category code). |
| Separator `,` then `;`, latin1 then UTF-8, `Accident_Id` instead of `Num_Acc` (2022), line break inside a column name (vehicles 2025) | various | Detected per file in `R/02_harmonise.R`. |
| Time as `1930` then `19:30`; department on three digits (`590`, `201` for Corse-du-Sud); coordinates in degrees x 100,000 with `0000000` for missing | until 2018 | Converted in `R/02_harmonise.R`. 2009 has no usable coordinates. |
| `0` or empty cell means "not filled" for categorical codes | all | Set to missing in `R/03_quality.R`. |

The full list of checks and how many rows each affects is in `results/quality_checks.csv`, and the
consistency checks run after the joins are in `results/consistency_checks.csv`.

## Linking the tables

`Num_Acc` identifies an accident in the four tables. A person is linked to **their own vehicle**
(pedestrians to the vehicle that hit them) with `id_vehicule` from 2019 and with the vehicle letter
`num_veh` before. Linking users and vehicles on `Num_Acc` alone pairs every person with every vehicle of
the accident: on 2005-2025 this produces 5.7 million rows instead of 3.0 million.
