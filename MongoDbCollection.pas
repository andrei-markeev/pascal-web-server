unit MongoDbCollection;

{$mode objfpc}
{$longstrings on}

interface

uses
    classes, libmongoc;

type
    generic TParseFromBsonEvent<T> = function(doc: pbson_t): T;
    generic TMongoDbCollection<T> = class
    public type
        TParseFromBson = specialize TParseFromBsonEvent<T>;
    private
        collection: pmongoc_collection_t;
        parseFromBson: TParseFromBson;
    public
        constructor Create(client: pmongoc_client_t; dbname, name: pchar; parser: TParseFromBson);
        destructor Destroy; override;
        function findOne(query: pbson_t): T;
    end;

implementation

constructor TMongoDbCollection.Create(client: pmongoc_client_t; dbname, name: pchar; parser: TParseFromBson);
begin
    collection := mongoc_client_get_collection(client, dbname, name);
    parseFromBson := parser;
end;

destructor TMongoDbCollection.Destroy;
begin
    mongoc_collection_destroy(collection);
    inherited;
end;

function TMongoDbCollection.findOne(query: pbson_t): T;
var
    cursor: pmongoc_cursor_t;
    doc: pbson_t;
begin
    cursor := mongoc_collection_find_with_opts(collection, query, nil, nil);

    if mongoc_cursor_next(cursor, @doc) then
        findOne := parseFromBson(doc)
    else
        findOne := default(T);

    mongoc_cursor_destroy(cursor);
end;

end.
