unit MongoDbCollection;

{$mode objfpc}
{$longstrings on}

interface

uses
    classes, libmongoc;

type
    generic TMongoDbCollection<T> = class
    private
        collection: pmongoc_collection_t;
    public
        constructor Create(client: pmongoc_client_t; dbname, name: pchar);
        destructor Destroy; override;
        function findOne(query: pbson_t): T;
    end;

implementation

constructor TMongoDbCollection.Create(client: pmongoc_client_t; dbname, name: pchar);
begin
    collection := mongoc_client_get_collection(client, dbname, name);
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
        findOne := T.Create(doc)
    else
        findOne := nil;

    mongoc_cursor_destroy(cursor);
end;

end.
