# This program reads an extract from pascal source containing record definitions and (over)writes
# a sqlite database containing two tables
#   records : containing the names of the records found
#   fields  : containing the fields which constitute those records

# TODO:
# It would be nice to wrap the incoming file in an object that presents a sequence of Clines,
# scooping up read_a_line, skip_to_type, skip_to_record and the globals 'line' and 'line_num'.
# Then with classes representing Records and Fields' ...
# Ah well, maybe one day. :-)

import csv
import sqlite3
import sys

infile =  open('292a.types.pas')
line = None

record_name = ""

# Represents 1 line of code from the pascal source file
# This code relies on the convention of having at most 1 declaration on a line of code
# and also that a declaration does not span multiple lines.
# We also assume that any lines containing only comments following a declaration
# are part of the comments for that declaration.
class Cline:

    def __init__(self, num, text):
        # initialise everything
        self.num = num
        self.text = text
        self.code = None
        self.comment = None
        self.is_eof = False
        self.is_record = False
        self.is_decl = False
        self.is_type = False
        self.is_var = False
        self.name = "--- no name ---"
        self.fld_type = "--- no type ---"
        self.is_end = False

        if len(text) == 0:
            self.is_eof = True

        self.line = text.strip()

        if self.line.lower()[0:4] == 'type':
            self.is_type = True

        if self.line.lower()[0:3] == 'var':
            self.is_var = True

        # Split into code & comment
        posn = self.line.find('//')
        if posn >= 0:
            self.code = self.line[:posn]
            self.comment = self.line[posn+2:]
        else:
            self.code = self.line
            self.comment = ""

        posn = self.code.find('{')
        if posn >= 0:
            self.comment = self.code[posn+2:] + self.comment
            self.code = self.code[:posn]

        self.code = self.code.strip()
        self.comment = self.comment.strip()

        # is it a record defn?
        posn = self.code.find('=record')
        if posn >= 0:
            self.is_record = True
            self.name = self.code[:posn].strip()

        # is it a declaration?
        pos1 = self.code.find(':')
        pos2 = self.code.find(';')
        if pos1>-1 and pos2 > -1:
            self.is_decl = True
            self.name = self.code[:pos1].strip()
            self.fld_type = self.code[pos1+1:pos2].strip()

        # Is it an "end" ?
        posn = self.code.find('end;')
        if posn >= 0:
            self.is_end = True

def skip_to_type():
    global line
    while not line.is_type:
        read_a_line()

def read_a_line():
    global line
    global line_num
    if line == None:
        line_num = 1
    else:
        line_num += 1
    text = infile.readline()
    line = Cline(line_num, text)


def write_record(conn):
    global line
    print("\nStoring record ", line.name)
    sql = 'INSERT INTO records (name) VALUES (:name)'
    sql_cmd(conn, sql, {"name" : line.name})
    conn.commit()
    sql = 'SELECT rowid from records WHERE name=:name'
    rslt = sql_query(conn, sql, {'name': line.name})
    recno = rslt[0][0]
    print("... as record", recno, "with the following fields:")
    return recno

def write_field_entry(recid, number, name, fld_type, comment):
    global line
    print("    ", number ,":", name ,"as", fld_type)
    sql = 'INSERT INTO fields \
        (record_id, number, name, type, comment) \
        VALUES (:recid, :number, :name, :type, :comment)'
    params = {
        "recid"     : recid,
        "number"    : number,
        "name"      : name,
        "type"      : fld_type,
        "comment"   : comment}
    sql_cmd(conn, sql, params)
    conn.commit()

def skip_to_record():
    global line
    while not line.is_eof and not line.is_record and not line.is_var:
        read_a_line()


def process_record(conn):
    global line
    recid = write_record(conn)
    read_a_line()
    process_fields(conn, recid)

def process_fields(conn, recid):
    global line
    count = 0
    while not line.is_end:
        if line.is_decl:
            count += 1
            process_field(conn, recid, count)
        read_a_line()

def process_field(conn, recid, number):
    global line
    write_field_entry(recid, number, line.name, line.fld_type, line.comment)



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


init_sql = [
    "DROP TABLE IF EXISTS fields",
    "DROP TABLE IF EXISTS records",
    "CREATE TABLE records (\
        name        text unique)",
    "CREATE TABLE   fields ( \
        record_id   integer, \
        number      integer, \
        name        text, \
        type        text, \
        comment     text, \
        UNIQUE  (record_id, name), \
        FOREIGN KEY (record_id) \
            REFERENCES records (rowid))"
]

def connect_db(path):
    print("Connecting to database ...")
    connection = None
    try:
        connection = sqlite3.connect(path)
        print("    ... was successful")
        return connection
    except Error as e:
        print(f"    ... raised an error: {e}")
        sys.exit()

def init_db(path):
    conn = connect_db(path)
    csr = conn.cursor()
    for line in init_sql:
        csr.execute(line)
    conn.commit()
    return conn

#==============================================================

#   I know, this is very "old school".
#   If I could have remembered how, I could probably have made an iterator
#   which returned a sequence of records, but this is simple, clear and it works.
global line_no

conn = init_db("records.sqlite")
read_a_line()
# skip_to_type()
# read_a_line()
# skip_to_type()
# print("Second type at ", line_num)
skip_to_record()
# print("First record at ", line_num)
# print(line_num, line.text)
# print(line.is_record)
while line.is_record:
    process_record(conn)
    skip_to_record()

