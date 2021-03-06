# event-log

This is a microservice which provides CRUD operations for Event Log DB.

## API

| Method | Path                                    | Description                                                    |
|--------|-----------------------------------------|----------------------------------------------------------------|
|  GET   | ```/events?latest_per_project=true```   | Finds events for all the projects with the latest `event_date` |
|  PATCH | ```/events```                           | Changes events' data by applying the given patch               |
|  POST  | ```/events```                           | Creates an event with a `NEW` status                           |
|  PATCH | ```/events/:event-id/:project-id```     | Updates chosen event's data                                    |
|  GET   | ```/metrics```                          | Returns Prometheus metrics of the service                      |
|  GET   | ```/ping```                             | Verifies service health                                        |
|  GET   | ```/processing-status?project-id=:id``` | Finds processing status of events belonging to a project       |
|  POST  | ```/subscriptions```                    | Adds a subscription for events                                 |


#### GET /events?latest_per_project=true

Finds events for all the projects with the latest `event_date`.

**Response**

| Status                     | Description                                                   |
|----------------------------|---------------------------------------------------------------|
| OK (200)                   | If there are events found for the projects or `[]` otherwise  |
| BAD_REQUEST (400)          | If value different that `true` given for `latest_per_project` |
| NOT_FOUND (404)            | If no `latest_per_project` given                              |
| INTERNAL SERVER ERROR (500)| When there are problems                                       |

Response body example:

```json
{
  "id":     "df654c3b1bd105a29d658f78f6380a842feac879",
  "project": {
    "id":   123,
    "path": "namespace/project-name"
  },
  "body":   "JSON payload"
}
```

#### PATCH /events

Changes events' data by applying the given patch.

**NOTICE:** 
Be aware that the given patch affects all the events in the Event Log.

**Request**

```json
{
  "status": "NEW"
}
```

**Response**

| Status                     | Description                                          |
|----------------------------|------------------------------------------------------|
| ACCEPTED (202)             | When the given data patch got accepted               |
| BAD_REQUEST (400)          | When request body is not valid                       |
| INTERNAL SERVER ERROR (500)| When there were problems with processing the request |

#### POST /events

Creates an event with the `NEW` status.

**Request**

```json
{
  "id": "df654c3b1bd105a29d658f78f6380a842feac879",
  "project": {
    "id":   123,
    "path": "namespace/project-name"
  },
  "date":    "2001-09-04T10:48:29.457Z",
  "batchDate": "2001-09-04T11:00:00.000Z",
  "body":      "JSON payload"
}
```

Event Body example:

```json
{
  "id":            "df654c3b1bd105a29d658f78f6380a842feac879",
  "message":       "some text",
  "committedDate": "2001-09-04T10:48:29.457Z",
  "author": {
    "name":  "author name",
    "email": "author@mail.com"    // optional
  },
  "committer": {
    "name":  "committer name",
    "email": "committer@mail.com" // optional
  },
  "parents": [
    "f307326be71b17b90db5caaf47bcd44710fe119f"
  ],
  "project": {
    "id":    123,
    "path": "namespace/project-name"
  }
}
```

**Response**

| Status                     | Description                                                                          |
|----------------------------|--------------------------------------------------------------------------------------|
| OK (200)                   | When event with the given `id` for the given project already exists in the Event Log |
| CREATED (201)              | When a new event was created in the Event Log                                        |
| BAD_REQUEST (400)          | When request body is not a valid JSON Event                                          |
| INTERNAL SERVER ERROR (500)| When there are problems with event creation                                          |

#### GET /metrics

To fetch various Prometheus metrics of the service.

**Response**

| Status                     | Description            |
|----------------------------|------------------------|
| OK (200)                   | Containing the metrics |
| INTERNAL SERVER ERROR (500)| Otherwise              |

#### PATCH /events/:event-id/:project-id

Updates event's data with the given payload.

**Request**

Currently, only status changing payloads are allowed:
- for transitioning event from status `PROCESSING` to `NEW`
```json
{
  "status": "NEW"
}
```
**Notice** `CONFLICT (409)` returned when current event status is different than `PROCESSING`.
- for transitioning event from status `PROCESSING` to `TRIPLES_STORE`
```json
{
  "status": "TRIPLES_STORE"
}
```
**Notice** `CONFLICT (409)` returned when current event status is different than `PROCESSING`.
- for transitioning event from status `PROCESSING` to `RECOVERABLE_FAILURE`
```json
{
  "status": "RECOVERABLE_FAILURE",
  "message": "error message"
}
```
**Notice** `CONFLICT (409)` returned when current event status is different than `PROCESSING`.
- for transitioning event from status `PROCESSING` to `NON_RECOVERABLE_FAILURE`
```json
{
  "status": "NON_RECOVERABLE_FAILURE",
  "message": "error message"
}
```
**Notice** `CONFLICT (409)` returned when current event status is different than `PROCESSING`.

**Response**

| Status                     | Description                                                                 |
|----------------------------|-----------------------------------------------------------------------------|
| OK (200)                   | If status update is successful                                              |
| BAD_REQUEST (400)          | When invalid payload is given                                               |
| CONFLICT (409)             | When current status of the event does not allow to become the requested one |
| INTERNAL SERVER ERROR (500)| When some problems occurs                                                   |

#### GET /ping

Verifies service health.

**Response**

| Status                     | Description           |
|----------------------------|-----------------------|
| OK (200)                   | If service is healthy |
| INTERNAL SERVER ERROR (500)| Otherwise             |

#### GET /processing-status?project-id=:id

Finds processing status of events belonging to the project with the given `id` from the latest batch.

**Response**

| Status                     | Description                                                                        |
|----------------------------|------------------------------------------------------------------------------------|
| OK (200)                   | If there are events for the project with the given `id`                            |
| BAD_REQUEST (400)          | If the `project-id` parameter is not given or invalid                              |
| NOT_FOUND (404)            | If no events can be found for the given project or no `project-id` parameter given |
| INTERNAL SERVER ERROR (500)| When some problems occurs                                                          |

Response body examples:
- all events from the latest batch are processed
```json
{
  "done": 20,
  "total": 20,
  "progress": 100.00
}
```
- some events from the latest batch are being processed
```json
{
  "done": 10,
  "total": 20,
  "progress": 50.00
}
```

#### POST /subscriptions

Adds a subscription to the events with certain statuses. Once a service gets successfully subscribed by receiving an ACCEPTED,
event-log service will start distributing events with the given `statuses` to the URL presented in the request body. Currently, 
event-log allows subscriptions to `NEW` and `RECOVERABLE_FAILURE` statuses only. 

**NOTICE:** 
As a good practice, the subscription should be renewed periodically in case of restart or URL change.

**Request**

```json
{
  "subscriberUrl": "http://host/path",
  "statuses": ["NEW","RECOVERABLE_FAILURE"]
}
```

**Response**

| Status                     | Description                                                                                         |
|----------------------------|-----------------------------------------------------------------------------------------------------|
| ACCEPTED (202)             | When subscription was successfully added/renewed                                                    |
| BAD_REQUEST (400)          | When there payload is invalid e.g. no `statuses` are different than `NEW` and `RECOVERABLE_FAILURE` |
| INTERNAL SERVER ERROR (500)| When there were problems with processing the request                                                |

## Trying out

The event-log is a part of multi-module sbt project thus it has to be built from the root level.

- build the docker image

```bash
docker build -f event-log/Dockerfile -t event-log .
```

- run the service

```bash
docker run --rm -p 9005:9005 event-log
```
