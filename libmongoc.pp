unit libmongoc;

interface

uses
    ctypes;

const
    BSON_ERROR_BUFFER_SIZE = 504;

    BSON_TYPE_EOD = $00;
    BSON_TYPE_DOUBLE = $01;
    BSON_TYPE_UTF8 = $02;
    BSON_TYPE_DOCUMENT = $03;
    BSON_TYPE_ARRAY = $04;
    BSON_TYPE_BINARY = $05;
    BSON_TYPE_UNDEFINED = $06;
    BSON_TYPE_OID = $07;
    BSON_TYPE_BOOL = $08;
    BSON_TYPE_DATE_TIME = $09;
    BSON_TYPE_NULL = $0A;
    BSON_TYPE_REGEX = $0B;
    BSON_TYPE_DBPOINTER = $0C;
    BSON_TYPE_CODE = $0D;
    BSON_TYPE_SYMBOL = $0E;
    BSON_TYPE_CODEWSCOPE = $0F;
    BSON_TYPE_INT32 = $10;
    BSON_TYPE_TIMESTAMP = $11;
    BSON_TYPE_INT64 = $12;
    BSON_TYPE_DECIMAL128 = $13;
    BSON_TYPE_MAXKEY = $7F;
    BSON_TYPE_MINKEY = $FF;

    BSON_SUBTYPE_BINARY = $00;
    BSON_SUBTYPE_FUNCTION = $01;
    BSON_SUBTYPE_BINARY_DEPRECATED = $02;
    BSON_SUBTYPE_UUID_DEPRECATED = $03;
    BSON_SUBTYPE_UUID = $04;
    BSON_SUBTYPE_MD5 = $05;
    BSON_SUBTYPE_ENCRYPTED = $06;
    BSON_SUBTYPE_USER = $80;

type
    pbson_t = ^bson_t;
    bson_t = array [0..127] of cuint8;

    pbson_error_t = ^bson_error_t;
    bson_error_t = record
        domain: cuint32;
        code: cuint32;
        message: array[0..BSON_ERROR_BUFFER_SIZE-1] of cchar;
    end;

    bson_type_t = cint;
    bson_subtype_t = cint;
    bson_oid_t = array [0..11] of cuint8;
    {$PackRecords 8}
    bson_value_t = record
        case value_type: bson_type_t of
            BSON_TYPE_OID: (padding1 : cint32; v_oid: bson_oid_t);
            BSON_TYPE_INT64: (padding2 : cint32; v_int64: cint64);
            BSON_TYPE_INT32: (padding3 : cint32; v_int32: cint32);
            BSON_TYPE_DOUBLE: (padding5 : cint32; v_double: cdouble);
            BSON_TYPE_BOOL: (padding6 : cint32; v_bool: cbool);
            BSON_TYPE_DATE_TIME: (padding7 : cint32; v_datetime: cint64);
            BSON_TYPE_TIMESTAMP: (padding8 : cint32; timestamp: cuint32; increment: cuint32);
            BSON_TYPE_UTF8: (padding9 : cint32; str: pchar; len: cuint32);
            BSON_TYPE_DOCUMENT: (padding10 : cint32; data: pcuint8; data_len: cuint32);
            BSON_TYPE_BINARY: (padding11 : cint32; bin_data: pcuint8; bin_data_len: cuint32; subtype: bson_subtype_t);
            BSON_TYPE_REGEX: (padding12 : cint32; regex: pchar; options: pchar);
            BSON_TYPE_DBPOINTER: (padding13 : cint32; collection: pchar; collection_len: cuint32; oid: bson_oid_t);
            BSON_TYPE_CODE: (padding14 : cint32; code: pchar; code_len: cuint32);
            BSON_TYPE_CODEWSCOPE: (padding15 : cint32; codew: pchar; scope_data: pcuint8; codew_len: cuint32; scope_len: cuint32);
            BSON_TYPE_SYMBOL: (padding16 : cint32; symbol: pchar; symbol_len: cuint32);
            BSON_TYPE_DECIMAL128: (padding17 : cint32; low: cuint64; high: cuint64);
    end;
    {$PackRecords default}

    pbson_iter_t = ^bson_iter_t;
    bson_iter_t = record
        raw: pcuint8;
        len: cuint32;
        off: cuint32;
        type_: cuint32;
        key: cuint32;
        d1: cuint32;
        d2: cuint32;
        d3: cuint32;
        d4: cuint32;
        next_off: cuint32;
        err_off: cuint32;
        value: bson_value_t;
    end;

    pmongoc_uri_t = pointer;
    pmongoc_client_t = pointer;
    pmongoc_collection_t = pointer;
    pmongoc_cursor_t = pointer;
    pmongoc_read_prefs_t = pointer;
    pmongoc_client_pool_t = pointer;

function bson_new: pbson_t; cdecl; external 'libbson-1.0' name 'bson_new';

function bson_append_utf8(doc: pbson_t; key: pchar; key_length: cint; value: pchar; length: cint): cbool; cdecl; external 'libbson-1.0' name 'bson_append_utf8';
function bson_append_int32(doc: pbson_t; key: pchar; key_length: cint; value: cint32): cbool; cdecl; external 'libbson-1.0' name 'bson_append_int32';
function bson_append_int64(doc: pbson_t; key: pchar; key_length: cint; value: cint64): cbool; cdecl; external 'libbson-1.0' name 'bson_append_int64';
function bson_append_bool(doc: pbson_t; key: pchar; key_length: cint; value: cbool): cbool; cdecl; external 'libbson-1.0' name 'bson_append_bool';

function bson_as_json(bson: pbson_t; length: pcsize_t): pchar; cdecl; external 'libbson-1.0' name 'bson_as_json';

procedure bson_destroy(doc: pbson_t); cdecl; external 'libbson-1.0' name 'bson_destroy';
procedure bson_free(s: pchar); cdecl; external 'libbson-1.0' name 'bson_free';

procedure mongoc_init(); cdecl; external 'libmongoc-1.0' name 'mongoc_init';
procedure mongoc_cleanup(); cdecl; external 'libmongoc-1.0' name 'mongoc_cleanup';

function mongoc_uri_new(uri_string: pchar): pmongoc_uri_t; cdecl; external 'libmongoc-1.0' name 'mongoc_uri_new';
function mongoc_uri_new_with_error(uri_string: pchar; error: pbson_error_t): pmongoc_uri_t; cdecl; external 'libmongoc-1.0' name 'mongoc_uri_new_with_error';
procedure mongoc_uri_destroy(uri: pmongoc_uri_t); cdecl; external 'libmongoc-1.0' name 'mongoc_uri_destroy';

function mongoc_client_new(uri_string: pchar): pmongoc_client_t; cdecl; external 'libmongoc-1.0' name 'mongoc_client_new';
function mongoc_client_new_from_uri(uri: pmongoc_uri_t): pmongoc_client_t; cdecl; external 'libmongoc-1.0' name 'mongoc_client_new_from_uri';
procedure mongoc_client_destroy(client: pmongoc_client_t); cdecl; external 'libmongoc-1.0' name 'mongoc_client_destroy';

function mongoc_client_pool_new(uri: pmongoc_uri_t): pmongoc_client_pool_t; cdecl; external 'libmongoc-1.0' name 'mongoc_client_pool_new';
function mongoc_client_pool_set_error_api(pool: pmongoc_client_pool_t; version: cint32): cbool; cdecl; external 'libmongoc-1.0' name 'mongoc_client_pool_set_error_api';
function mongoc_client_pool_max_size(pool: pmongoc_client_pool_t; version: cuint32): cbool; cdecl; external 'libmongoc-1.0' name 'mongoc_client_pool_max_size';
function mongoc_client_pool_pop(pool: pmongoc_client_pool_t): pmongoc_client_t; cdecl; external 'libmongoc-1.0' name 'mongoc_client_pool_pop';
procedure mongoc_client_pool_push(pool: pmongoc_client_pool_t; client: pmongoc_client_t); cdecl; external 'libmongoc-1.0' name 'mongoc_client_pool_push';
procedure mongoc_client_pool_destroy(pool: pmongoc_client_pool_t); cdecl; external 'libmongoc-1.0' name 'mongoc_client_pool_destroy';

function mongoc_client_get_collection(client: pmongoc_client_t; dbname, collname: pchar): pmongoc_collection_t; cdecl; external 'libmongoc-1.0' name 'mongoc_client_get_collection';
function mongoc_collection_find_with_opts(collection: pmongoc_collection_t; filter, opts: pbson_t; read_prefs: pmongoc_read_prefs_t): pmongoc_cursor_t; cdecl; external 'libmongoc-1.0' name 'mongoc_collection_find_with_opts';
procedure mongoc_collection_destroy(collection: pmongoc_collection_t); cdecl; external 'libmongoc-1.0' name 'mongoc_collection_destroy';

function mongoc_cursor_next(cursor: pmongoc_cursor_t; doc: pbson_t): cbool; cdecl; external 'libmongoc-1.0' name 'mongoc_cursor_next';
procedure mongoc_cursor_destroy(cursor: pmongoc_cursor_t); cdecl; external 'libmongoc-1.0' name 'mongoc_cursor_destroy';
function bson_iter_init (iter: pbson_iter_t; bson: pbson_t): cbool; cdecl; external 'libmongoc-1.0' name 'bson_iter_init';
function bson_iter_next (iter: pbson_iter_t): cbool; cdecl; external 'libmongoc-1.0' name 'bson_iter_next';
function bson_iter_key(iter: pbson_iter_t): pchar; cdecl; external 'libmongoc-1.0' name 'bson_iter_key';
function bson_iter_type(iter: pbson_iter_t): bson_type_t; cdecl; external 'libmongoc-1.0' name 'bson_iter_type';
function bson_iter_int32(iter: pbson_iter_t): cint32; cdecl; external 'libmongoc-1.0' name 'bson_iter_int32';
function bson_iter_bool(iter: pbson_iter_t): cbool; cdecl; external 'libmongoc-1.0' name 'bson_iter_bool';
function bson_iter_utf8(iter: pbson_iter_t; len: pcuint32): pchar; cdecl; external 'libmongoc-1.0' name 'bson_iter_utf8';

implementation

end.