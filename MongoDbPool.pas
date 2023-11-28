unit MongoDbPool;

{$mode objfpc}
{$longstrings on}

interface

uses
    libmongoc, MongoDbCollection, MongoUrl;

type
    TMongoDbPool = class
    private
        uri: pmongoc_uri_t;
    public
        pool: pmongoc_client_pool_t;
        constructor Create;
        destructor Destroy; override;
        function GetClientFromThePool: pointer;
        procedure ReturnClientToThePool(client: pointer);
    end;

implementation

constructor TMongoDbPool.Create;
begin
    mongoc_init;
    uri := mongoc_uri_new(MONGO_URL);
    pool := mongoc_client_pool_new(uri);
    mongoc_client_pool_set_error_api(pool, 2);
end;

destructor TMongoDbPool.Destroy;
begin
    mongoc_client_pool_destroy(pool);
    mongoc_uri_destroy(uri);
    mongoc_cleanup;
    inherited;
end;

function TMongoDbPool.GetClientFromThePool: pointer;
begin
    GetClientFromThePool := mongoc_client_pool_pop(pool);
end;

procedure TMongoDbPool.ReturnClientToThePool(client: pointer);
begin
    mongoc_client_pool_push(pool, client);
end;

end.