## desert-fertilization-shiny

### shiny app for viewing and loading desert fertilization data

Adding after having created the table, fields for `created_at` and `updated_at`, the latter requiring a function and trigger combination. Note here the `trigger_set_timestamp` function and `set_timestamp` trigger are specific to the urbancndep schema.

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

Initial approach was to have restricted (in the application and database) duplicate combinations of fieldID, collectionDate, Analyte Name, and omit. However, this was not tenable in light of Lachat runs having multiple seasons so the constraint was limited to duplicate combinations of fieldID, collectionDate, and Analyte Name in the application only.

```sql
ALTER TABLE urbancndep.resin DROP CONSTRAINT resin_field_id_collection_date_analyte_name_omit_key;
```