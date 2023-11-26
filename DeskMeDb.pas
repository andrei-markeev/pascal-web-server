unit DeskMeDb;

{$mode objfpc}
{$longstrings on}

interface

uses
    libmongoc, MongoDbCollection, DbSchema;

const
    MONGO_URL = 'mongodb://localhost';

type
    TDeskDbPool = class
    private
        uri: pmongoc_uri_t;
    public
        //pool: pmongoc_client_pool_t;
        client: pmongoc_client_t;
        constructor Create;
        destructor Destroy; override;
    end;

    TDeskMeDatabase = class
    public type
        TOfficeLocationCollection = specialize TMongoDbCollection<TOfficeLocation>;
    private
        //pool: pmongoc_client_pool_t;
        client: pmongoc_client_t;
    public
        Locations: TOfficeLocationCollection;
        constructor Create(deskmePool: TDeskDbPool);
        destructor Destroy; override;
    end;

implementation

constructor TDeskDbPool.Create;
begin
    mongoc_init;
    uri := mongoc_uri_new(MONGO_URL);
    client := mongoc_client_new_from_uri(uri);
    //pool := mongoc_client_pool_new(uri);
    //mongoc_client_pool_set_error_api(pool, 2);
end;

destructor TDeskDbPool.Destroy;
begin
    //mongoc_client_pool_destroy(pool);
    mongoc_client_destroy(client);
    mongoc_uri_destroy(uri);
    mongoc_cleanup;
    inherited;
end;

constructor TDeskMeDatabase.Create(deskmePool: TDeskDbPool);
begin
    //pool := deskmePool.pool;
    //client := mongoc_client_pool_pop(pool);
    client := deskmePool.client;
    Locations := TOfficeLocationCollection.Create(client, 'deskme', 'locations', @parseOfficeLocationFromBson);
end;

destructor TDeskMeDatabase.Destroy;
begin
    Locations.Destroy;
    //mongoc_client_pool_push(pool, client);
    inherited;
end;

end.
