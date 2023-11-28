unit MyDb;

{$mode objfpc}
{$longstrings on}

interface

uses
    MongoDbPool, MongoDbCollection, OfficeLocation;

type

    TMyDatabase = class
    public type
        TOfficeLocationCollection = specialize TMongoDbCollection<TOfficeLocation>;
    private
        pool: TMongoDbPool;
        client: pointer;
    public
        Locations: TOfficeLocationCollection;
        constructor Create(mongoDbPool: TMongoDbPool);
        destructor Destroy; override;
    end;

implementation

constructor TMyDatabase.Create(mongoDbPool: TMongoDbPool);
begin
    pool := mongoDbPool;
    client := pool.GetClientFromThePool;
    Locations := TOfficeLocationCollection.Create(client, 'deskme', 'locations');
end;

destructor TMyDatabase.Destroy;
begin
    Locations.Free;
    pool.ReturnClientToThePool(client);
    inherited;
end;

end.
