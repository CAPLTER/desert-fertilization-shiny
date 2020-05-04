## desert-fertilization-shiny

### shiny app for viewing and loading desert fertilization data

Desert Fertilization application to address (1) current, backlog, and future
uploads of resin data, and (2) fertilizer applications.

The approach taken here for the resin data is to upload the raw lachat data and
marry sample metadata directly to the machine output. This is different than,
for example, the stormwater data where raw lachat data are loaded then results
only are pulled out and coupled to a sampling event. The approach here is
advantageous in that the techs can handle all of the data entry. On the pull
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

Fertilizer is a simple upload and viewer.

issues:
1. cannot get dateInput type for shinyInput value

#### notes

Adding after having created the table, fields for `created_at` and `updated_at`,
the latter requiring a function and trigger combination. Note here the
`trigger_set_timestamp` function and `set_timestamp` trigger are specific to the
urbancndep schema.

```sql
ALTER TABLE urbancndep.resin
  ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  ADD COLUMN updated_at TIMESTAMP NOT NULL DEFAULT NOW();

CREATE OR REPLACE FUNCTION urbancndep.trigger_set_timestamp()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_timestamp
BEFORE UPDATE ON urbancndep.resin
FOR EACH ROW
EXECUTE PROCEDURE urbancndep.trigger_set_timestamp();
```

Initial approach was to have restricted (in the application and database)
duplicate combinations of fieldID, collectionDate, Analyte Name, and omit.
However, this was not tenable in light of Lachat runs having multiple seasons so
the constraint was limited to duplicate combinations of fieldID, collectionDate,
and Analyte Name in the application only.

```sql
ALTER TABLE urbancndep.resin DROP CONSTRAINT resin_field_id_collection_date_analyte_name_omit_key;
```