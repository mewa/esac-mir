## Formats

### ESAC JSON format
`ESAC JSON` format is an intermediate format, which represents esac records. Individual records are expressed as JSON objects with the following properties:

| property  | type   | ESAC field mapping |
|-----------|--------|--------------------|
| name      | String | record name        |
| title     | String | CUT                |
| source    | String | TRD                |
| region    | String | REG                |
| signature | String | SIG                |
| key       | String | KEY                |
| melody    | String | MEL                |
| remarks   | String | REM                |

### ESAC-Id JSON format
In addition to the properties defined by the `ESAC JSON` format, the `ESAC-Id JSON` format includes record `id`, which is used to make API calls.

| property | type   |
|----------|--------|
| id       | String |

### ESAC Filter format

| property | type   |
|----------|--------|
| field    | String |
| term     | String |

`field` can take the following values:
* `name`
* `title`
* `source`
* `region`
* `signature`
* `key`
* `melody`
* `melody_raw`
* `melody_rhythm`
* `remarks`

### MIDI JSON format
`MIDI JSON` format is a JSON object representing a MIDI file.

| property | type   | description                                  |
|----------|--------|----------------------------------------------|
| midi64   | String | data url containing base64-encoded MIDI file |

## API methods

### Get ESAC record by id

| method | url         |
|--------|-------------|
| GET    | `/esac/:id` |

| parameter | description                                                                | optional |
|-----------|----------------------------------------------------------------------------|----------|
| :id       | id of ESAC record                                                          | no       |
| parse     | when non-empty value is suppplied, request body is parsed as raw ESAC file | yes      |

| response              |
|-----------------------|
| `ESAC-Id JSON` object |

### Get ESAC records list

| method | url          |
|--------|--------------|
| GET    | `/esac/list` |

| response                             |
|--------------------------------------|
| JSON array of `ESAC-Id JSON` objects |

### Search for ESACs matching filters

| method | url            |
|--------|----------------|
| POST   | `/esac/search` |

| request body                        | response                             |
|-------------------------------------|--------------------------------------|
| JSON array of `ESAC Filter` objects | JSON array of `ESAC-Id JSON` objects |


### Add ESAC record

| method | url     |
|--------|---------|
| PUT    | `/esac` |


| parameter | description                                                                | optional |
|-----------|----------------------------------------------------------------------------|----------|
| parse     | when non-empty value is suppplied, request body is parsed as raw ESAC file | yes      |

| request body                                          | response                         |
|-------------------------------------------------------|----------------------------------|
| `ESAC JSON` (or ESAC file, when `parse` is specified` | `{"id": <id of created record>}` |


### Update ESAC record by id

| method | url         |
|--------|-------------|
| PATCH  | `/esac/:id` |


| parameter | description                                                                | optional |
|-----------|----------------------------------------------------------------------------|----------|
| :id       | id of ESAC record                                                          | no       |
| parse     | when non-empty value is suppplied, request body is parsed as raw ESAC file | yes      |

### Delete ESAC record by id

| method | url         |
|--------|-------------|
| DELETE | `/esac/:id` |


| parameter | description                                                                | optional |
|-----------|----------------------------------------------------------------------------|----------|
| :id       | id of ESAC record                                                          | no       |

### Parse raw ESAC to ESAC JSON

| method | url         |
|--------|-------------|
| POST   | `/esacjson` |

| request body | response    |
|--------------|-------------|
| ESAC file    | `ESAC JSON` |

### Convert ESAC to MIDI

| method | url          |
|--------|--------------|
| POST   | `/esac2midi` |


| parameter | description                                                                | optional |
|-----------|----------------------------------------------------------------------------|----------|
| parse     | when non-empty value is suppplied, request body is parsed as raw ESAC file | yes      |
| format    | when `format=file`, response is a MIDI file                                | yes      |
| tempo     | BPM in resulting MIDI file, default is `90`                                | yes      |
| octave    | base octave of ESAC intervals, default is `5`                              | yes      |

| request body                                          | response                                       |
|-------------------------------------------------------|------------------------------------------------|
| `ESAC JSON` (or ESAC file, when `parse` is specified) | `MIDI JSON` (or MIDI file, when `format=file`) |

### Convert MIDI to ESAC

| method | url          |
|--------|--------------|
| POST   | `/esac2midi` |


| parameter | description                                      | optional |
|-----------|--------------------------------------------------|----------|
| key       | base sound of the resulting ESAC, default is `C` | yes      |

| request body | response    |
|--------------|-------------|
| MIDI file    | `ESAC JSON` |
