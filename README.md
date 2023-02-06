## desert-fertilization-shiny

### shiny app for viewing and loading selected desert fertilization data

#### resin

The approach taken here for the resin data is to upload the raw Lachat data and
marry sample metadata directly to the machine output. This is different than,
for example, the stormwater data where raw Lachat data are loaded then results
only are pulled out and coupled to a sampling event. On the pull
side, we can query only unknowns not flagged for omit, then parse the sample ID,
which is standardized based on drop-down input. Of course, the approach works
better here (than, for example, stormwater) as there is a one-to-one
relationship between a sample and result, whereas there is a one-to-many
relationship between samples and result (Lachat, ICP, AQ2, etc.) that is handled
much better with a sampling event.

The initial approach allowed for resin Lachat data to be married to metadata
using one of two approaches: (1) manually enter all metadata details (site,
date, notes), or (2) merge metadata entered into an Excel file, and manually
enter only information not available from the merge. The latter approach proved
highly problematic as sheets containing metadata that were to be merged with
Lachat output had to be meticulously curated, which was not practical with a
spreadsheet - also there was not an efficiency gain since the data have to be
entered at some point so it might as well be with the Lachat file itself.

#### CHN

The approach taken here is similar to resin where data are uploaded into a
table that mostly mirrors the format of the analytic output with some added
metadata (e.g., collection date). Again, this differs from, for example, the
stormwater application where a single sample translates to multiple analytes.
An artifact of this simpler design is that some of the logic used in the more
complicated stormwater application, which was ported to this application, loses
some context. For example, stormwater data are written to results and raw
(machine) outputs tables in the database with corresponding formatting relevant
to the destination; some of those distinctions are included here even though,
in this case, the write is to a table that houses simultaneously both the
results and raw (machine output) data.

#### fertilizer

Fertilizer is a simple upload and viewer.

#### annuals

Currently supports composition only.


#### notes

##### unique resin events

Initial approach was to have restricted (in the application and database)
duplicate combinations of fieldID, collectionDate, Analyte Name, and omit.
However, this was not tenable in light of Lachat runs having multiple seasons
so the constraint was limited to duplicate combinations of fieldID,
collectionDate, and Analyte Name in the application only.

```sql
ALTER TABLE urbancndep.resin DROP CONSTRAINT
resin_field_id_collection_date_analyte_name_omit_key;
```
