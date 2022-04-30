import csv
import sqlite3
import sys

table_delim = "|======"

# === Database functions ===
def connect_db(path):
    connection = None
    try:
        connection = sqlite3.connect(path)
        return connection
    except Error as e:
        print(f"... connecting to database", path, "raised an error: {e}")
        sys.exit()

def sql_cmd(conn, sql, params={}):
    csr = conn.cursor()
    try:
        csr.execute(sql, params)
        conn.commit()
        return csr
    except Exception as e:
        print(f"Error executing SQL: {e}")
        print(f"SQL was:", sql)
        sys.exit()

def sql_query(conn, sql, params={}):
    csr = sql_cmd(conn, sql, params)
    return csr.fetchall()


def get_records(conn):
    sql = "SELECT rowid, name FROM records ORDER BY name"
    recs = sql_query(conn, sql)
    return recs

def get_fields(conn, recid):
    sql = "SELECT number, name, type, comment FROM fields" +\
            " WHERE record_id = :recid" +\
            " ORDER BY number"
    parms = {"recid": recid}
    flds = sql_query(conn, sql, parms)
    return flds


# === Output functions ===

def print_record_heading(record):
    print("\n=== ", record,"\n")

def print_table_heading(record):
    print('[options=header,cols="^1,<10,<7,<30"]')
    print(table_delim)
    print("|#|Name|Type|Comment")

def print_field_entry(number, name, fld_type, comment):
    print("|", number ,"|", name ,"|", fld_type ,"|", comment)


# === Entity functions ===

def process_record(conn, rec):
    rowid = rec[0]
    name = rec[1]
    print_record_heading(name)
    print_table_heading(name)
    process_fields(conn, rowid)
    print(table_delim)

def process_fields(conn, recid):
    flds = get_fields(conn, recid)
    for fld in flds:
        process_field(fld)

def process_field(fld):
    number = fld[0]
    name = fld[1]
    fld_type = fld[2]
    comment = fld[3]
    print_field_entry(number, name, fld_type, comment)


#==============================================================

dbname = 'records.sqlite'
conn = connect_db(dbname)
recs = get_records(conn)

print("= records in", dbname, "\n")
for rec in recs:
    process_record(conn, rec)
