# Ebola case data — provenance and attribution

The Ebola case, death and cure counts in this directory are third-party
humanitarian data, redistributed here under their original licence. They are
**not** covered by the repository's MIT `LICENSE`, which applies to the analysis
code only.

## Dataset

- **Title:** Ebola Cases and Deaths in the North Kivu Ebola Outbreak in the
  Democratic Republic of the Congo (DRC)
- **Original source:** DRC Ministry of Health (Ministère de la Santé) and the
  World Health Organization (WHO), from the daily Ebola situation reports
  (*situation épidémiologique*) for the 2018–2020 North Kivu / Ituri outbreak.
- **Distributed via:** Humanitarian Data Exchange (HDX) —
  <https://data.humdata.org/dataset/ebola-cases-and-deaths-drc-north-kivu>
- **Coverage:** cases, deaths and cures aggregated by health zone,
  4 August 2018 – 11 July 2020.
- **Methodology:** figures manually extracted from the MoH situation reports.

## Licence

Creative Commons Attribution for Intergovernmental Organisations
(**CC BY-IGO 3.0**) —
<http://creativecommons.org/licenses/by/3.0/igo/legalcode>

The licence permits redistribution and adaptation provided the source is
credited and the licence indicated, as above.

## Caveats

The figures are subject to revision due to ongoing reclassification,
retrospective investigation and the availability of laboratory results. There
is a one-week series break from 23–31 July 2019.

## Files

- `drc_ebola_cases_hdx_2020-07-11.csv` — full time series to the end of the
  outbreak, snapshotted on 2026-07-03 from the epiforecasts Google Sheet mirror
  of the HDX dataset. Values are unmodified from the source.
- `Cases+Deaths_By_Health_Zone_MOH_HDX_2019-05-21.csv` and the files under
  `old data/` — earlier extracts of the same dataset.
