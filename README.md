## desert-fertilization-shiny

### shiny app for viewing and loading selected desert fertilization data

Desert Fertilization application to address:

- resin data
- fertilizer applications
- annuals community composition

#### resin

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

#### fertilizer

Fertilizer is a simple upload and viewer.

issues:

1. cannot get dateInput type for shinyInput value

#### annuals


#### notes

##### leaked database connections

That we are unable to use pool with this app (on account of an unidentifiable
leaked pool object) is a problem. The general approach to working without pool
is to establish a DB connection in advance of each query then close the
connection at the close of the query. However, `dbDisconnect()` seemingly was
not in fact closing the connection and the app was plagued by too many open
connections in very short order. Two approaches were taken to address this.
First, a function to close all open connections when the app is closed was
added to app.R. This addition is helpful but while that ensures that all
connections are close when the app is terminated, it does not help with open
connections while the app is in use. To address open connections while in use,
a call to close apps on exit was added at each point that a connection was
established. The combination of these two appraoches seems to have addressed
the issue.

```
# close db connection after function call exits
on.exit(dbDisconnect(pg))
```

##### triggers on resin data

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

##### unique resin events

Initial approach was to have restricted (in the application and database)
duplicate combinations of fieldID, collectionDate, Analyte Name, and omit.
However, this was not tenable in light of Lachat runs having multiple seasons
so the constraint was limited to duplicate combinations of fieldID,
collectionDate, and Analyte Name in the application only.

```sql
ALTER TABLE urbancndep.resin DROP CONSTRAINT resin_field_id_collection_date_analyte_name_omit_key;
```
