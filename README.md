## Pascal Web Server

Investigation into creating a fast modern web server in Pascal.

The assumption is that this server will run behind a reverse proxy (e.g. NGINX) and so it doesn't need to deal with things like TLS, authentication, rate limiting, serving static assets, etc. It is tailored to parsing requests forwarded from the proxy, working with MongoDB database, and returning responses in JSON or HTML format.

The focus of the work is to ensure that the server is fast, lightweight and uses minimum resources (CPU and RAM), but at the same time provides a relatively high level interface for handling requests, making DB calls and working with business logic.

Features:
- asynchronous sockets (e.g. epoll on Linux) via lNet library
- bindings for MongoDB C Driver
- thread pool for MongoDB tasks (dynamically expands / shrinks according to the load profile)

### Getting started

#### Database interface

First, you need to create a database class that provides interface to your MongoDB database.

For example, TMyDatabase below contains two collections, Users and Orders:

```pascal
unit MyDb;

{$mode objfpc}

interface

uses
    MongoDbPool, MongoDbCollection, DBSchema;

type

    TMyDatabase = class
    public type
        TUserCollection = specialize TMongoDbCollection<TUser>;
        TOrderCollection = specialize TMongoDbCollection<TOrder>;
    private
        pool: TMongoDbPool;
        client: pointer;
    public
        Users: TUserCollection;
        Orders: TOrderCollection;
        constructor Create(mongoDbPool: TMongoDbPool);
        destructor Destroy; override;
    end;

implementation

constructor TMyDatabase.Create(mongoDbPool: TMongoDbPool);
begin
    pool := mongoDbPool;
    client := pool.GetClientFromThePool;

    Users := TUserCollection.Create(client, 'mydb', 'users');
    Orders := TOrderCollection.Create(client, 'mydb', 'orders');
end;

destructor TMyDatabase.Destroy;
begin
    Users.Free;
    Orders.Free;

    pool.ReturnClientToThePool(client);
    inherited;
end;

end.
```

In this example, `DBSchema` is another unit that contains `TOrder` and `TUser` classes, which could look something like this:

```pascal
type
    TUser = class
        _id: string;
        name: string;
        // ... other fields

        constructor Create;
        constructor Create(doc: pbson_t);
        destructor Destroy; override;
    end;
```

Importantly, each DB model class should contain a constructor that accepts `pbson_t` and parses the object from BSON.

BSON parsing is done with `libbson`, so you can more or less use examples from [official libbson documentation](https://mongoc.org/libbson/current/parsing.html).

Full example of a DB model class: https://github.com/andrei-markeev/pascal-web-server/blob/main/DBSchema/OfficeLocation.pas#L55

#### Web server

Running the server is as simple as:

```pascal

var
    server: TPascalWebServer;

begin
    server := TPascalWebServer.Create(@ProcessRequest);
    server.Listen(3000);
    server.Free;
end.

```

For handling requests, you need to provide `ProcessRequest` method which would route requests to the correct handlers.

For example:

```pascal
    procedure ProcessRequest(request: TRequest; socket: TLSocket);
    var
        task: TTask;
    begin
        case request.url of
            '/users':
            begin
                task := TUsersPageTask.Create(pool, socket);
                server.EnqueueTask(task);
            end;
            '/orders':
            begin
                task := TOrdersPageTask.Create(pool, socket);
                server.EnqueueTask(task);
            end;
        else
            socket.SendMessage('HTTP/1.1 404' + CRLF + 'Content-length: 0' + CRLF + CRLF);
        end;
    end;
```

Tasks should be inherited from `TTask`.

Each task is split into two parts:
- first part is called `Execute` and runs in one of worker threads (each worker thread has it's own connection to MongoDB), this is where you use the previously created `TMyDatabase` and it's collections
- second part is called `Finalize` and runs in the main thread, this is where you send response back to the client

For example:

```pascal
procedure TUserProfilePageTask.Execute;
var
    db: TMyDatabase;
    query: pbson_t;
begin
    db := TMyDatabase.Create(pool);
    query := bson_new;
    bson_append_utf8(query, '_id', length('_id'), userId, length(userId));
    user := db.Users.findOne(query);
    bson_destroy(query);
    db.Free;
end;

procedure TUserProfilePageTask.Finalize;
var
    i: integer;
    html: string;
    body: string;
begin
    html := '<!DOCTYPE html><html><h1>Your profile</h1><p><ul>'
        + '<li><strong>Name:</strong> ' + user.name + '</li>'
        + '<li><strong>Email:</strong> ' + user.email + '</li>'
        + '</ul>';

    user.Free;

    body := 'HTTP/1.1 200' + CRLF
        + 'Content-type: text/html' + CRLF
        + 'Content-length: ' + IntToStr(Length(html)) + CRLF
        + CRLF
        + html;

    if (status <> tsCancelled) and (socket.ConnectionStatus = scConnected) then
        socket.SendMessage(body);

end;
```

See full example here: https://github.com/andrei-markeev/pascal-web-server/blob/main/Tasks/LocationAsHtml.pas

**Note**: If you don't need MongoDB, don't use `EnqueueTask`, simply send the data to the socket right from `ProcessRequest`.
The whole tasks system was devised because [MongoDB C Driver doesn't expose async API](https://www.mongodb.com/community/forums/t/why-not-supply-async-api-in-mongo-c-driver/16260), so we have to deal with thread pool and the corresponding complexity.


### Benchmarks

All tests are run on same machine, in WSL.

I know that it is not optimal, but I think it still gives an idea of the relative performance.

Also, just a note, throughput can vary a bit between the runs. Probably depends on other tasks that are running on same machine. I'm trying to present average performing runs.

#### Hello world

Server should return `<!DOCTYPE html><html><h1>Hello worlde!</h1></html>` with correct `Content-Type` header.

**server_ltcp**:

With 10 concurrent connections, uses 9.4Mb RAM, throughput 65.9k rps.

```
Running 30s test @ http://localhost:3000/hello
  2 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   187.22us  472.16us  23.28ms   98.70%
    Req/Sec    33.19k     8.54k   97.85k    68.39%
  1984403 requests in 30.10s, 210.06MB read
Requests/sec:  65927.60
Transfer/sec:      6.98MB
```

**server_nodehttp**:

With 10 concurrent connections, uses 76.1Mb RAM, throughput 13.5k rps.

```
Running 30s test @ http://localhost:3000/hello
  2 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   746.38us  241.36us  12.07ms   93.28%
    Req/Sec     6.79k   456.07     7.20k    94.19%
  406458 requests in 30.10s, 84.12MB read
Requests/sec:  13503.67
Transfer/sec:      2.79MB
```

#### JSON serialization

Server should serialize an object to JSON format and return it with correct `Content-Type` header.
JSON strings should be properly escaped.

The object has the following structure (taken from a real production application):

```ts
interface OfficeLocation {
    _id: string,
    tenantId: string,
    name: string,
    address: string,
    latitude?: number,
    longitude?: number,
    email?: string,
    phone?: string,
    sendICalNotifications: boolean,
    notificationEmail?: string,
    cateringOrderEmail?: string,
    options: {
        availableFromHour?: number,
        availableUntilHour?: number,
        altSchedule?: ('Mon' | 'Tue' | 'Wed' | 'Thu' | 'Fri' | 'Sat' | 'Sun')[],
        altAvailableFromHour?: number,
        altAvailableUntilHour?: number
    }
}
```

**server_ltcp**:

With 10 concurrent connections, uses 9.5Mb RAM, throughput 34.5k rps.

```
Running 30s test @ http://localhost:3000/json
  2 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   417.47us    1.10ms  24.53ms   97.78%
    Req/Sec    17.39k     4.31k   33.83k    78.83%
  1038414 requests in 30.03s, 724.91MB read
Requests/sec:  34579.20
Transfer/sec:     24.14MB
```

With 150 concurrent connections, uses 9.5Mb RAM, throughput increases up to 48.2k rps.

```
Running 10s test @ http://localhost:3000/json
  2 threads and 150 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    10.10ms   88.16ms   1.68s    98.58%
    Req/Sec    24.26k     8.44k   40.53k    64.50%
  482655 requests in 10.00s, 336.94MB read
  Socket errors: connect 0, read 0, write 0, timeout 12
Requests/sec:  48254.72
Transfer/sec:     33.69MB
```

**server_nodehttp**:

With 10 concurrent connections, uses 77.9Mb RAM, throughput 13.1k rps.

```
Running 30s test @ http://localhost:3000/json
  2 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.86ms    1.10ms  34.09ms   98.05%
    Req/Sec     6.63k     0.96k    8.94k    92.67%
  395709 requests in 30.03s, 315.49MB read
Requests/sec:  13176.90
Transfer/sec:     10.51MB
```

With 150 concurrent connections, uses 82.5Mb RAM, throughput *decreases* down to 11.3k rps.

```
Running 10s test @ http://localhost:3000/json
  2 threads and 150 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    13.18ms    1.99ms  44.04ms   94.42%
    Req/Sec     5.73k   578.32     6.16k    94.00%
  114003 requests in 10.01s, 90.89MB read
Requests/sec:  11391.06
Transfer/sec:      9.08MB
```

### Fetch JSON from MongoDB

Server should fetch data from MongoDB and serialize it.

**server_ltcp**:

With 150 concurrent connections, uses 37.8Mb RAM, throughput 1.7k rps.

```
Running 10s test @ http://localhost:3000/mongo
  2 threads and 150 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    90.81ms  159.85ms   1.89s    94.74%
    Req/Sec     0.90k   404.41     1.59k    61.42%
  17682 requests in 10.03s, 6.27MB read
  Socket errors: connect 0, read 0, write 0, timeout 21
Requests/sec:   1763.70
Transfer/sec:    640.72KB
```

**server_nodehttp**:

With 150 concurrent connections, uses 113Mb RAM, throughput 1.1k rps.

```
Running 10s test @ http://localhost:3000/mongo
  2 threads and 150 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   144.79ms  105.96ms 979.14ms   91.06%
    Req/Sec   580.20    223.98     0.86k    76.38%
  11515 requests in 10.02s, 7.73MB read
Requests/sec:   1149.23
Transfer/sec:    790.09KB
```
