import csv
import json
import re
import sqlite3
import sys

'''
  Bytes     Signed      Unsigned                                    Range
    1       ShortInt(1) Byte(b)                 -128 .. 127                     0 .. 255
    2       Smallint(2)	Word(w)               -32768 .. 32767 	                0 .. 65535
  2 or 4    Integer(4) 	smallint or longint
    4       Longint(4) 	Longword/ 	     -2147483648 .. 2147483647 	            0 .. 4294967295
                        Cardinal(l)
    8       Int64(g)    QWord(q)-9223372036854775808 .. 9223372036854775807 	0 .. 18446744073709551615

  Bytes Type 	    Significant digits 	        Range
 4 or 8 Real 	    platform dependent 	???
    4   Single(f)   7-8                   1.5E-45 .. 3.4E38
    8   Double(d)   15-16                5.0E-324 .. 1.7E308
    8   Comp(c)     19-20                 -2E64+1 .. 2E63-1
    8   Currency(y)         -922337203685477.5808 .. 922337203685477.5807
    10  Extended(x)	19-20               1.9E-4932 .. 1.1E4932

        string(s)
'''

fld_type_codes = {
    'shortint': '1',
    'smallint': '2',
    'integer':  '4',
    'longint':  '4',
    'byte':     'b',
    'comp':     'c',
    'double':   'd',
    'single':   'f',
    'int64':    'g',
    'longword': 'l',
    'cardinal': 'l',
    'boolean':  'o',
    'qWord':    'q',
    'string':   's',
    'word':     'w',
    'extended': 'x',
    'currency': 'y'
}

# === Database functions ===
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


def get_record(conn, recname):
    sql = "SELECT rowid, name FROM records" +\
            " where name = :name"
    parms = {"name": recname}
    recs = sql_query(conn, sql, parms)
    return recs

def get_fields(conn, recid):
    sql = "SELECT number, name, type, comment FROM fields" +\
            " WHERE record_id = :recid" +\
            " ORDER BY number"
    parms = {"recid": recid}
    flds = sql_query(conn, sql, parms)
    return flds


# === Output functions ===

def gen_basic_entry(number, name, fld_type, comment):
    fld = {
        'number' : number,
        'name' : name,
        'type' : fld_type,
        'comment' : comment
    }
    return(fld)

# === Entity functions ===

def process_record(conn, number, name, recname):
    recs = get_record(conn, recname)
    if len(recs) != 1 :
        print("Type", recname, "unknown - aborting.")
        sys.exit()
    recdata = recs[0]
    rowid = recdata[0]

    rec = gen_basic_entry(number, name, '.'+recname, '')

    rec['fields'] = process_fields(conn, rowid)

    return(rec)

def process_fields(conn, recid):
    flds = []
    fld_datas = get_fields(conn, recid)
    for fld_data in fld_datas:
        flds.append(process_field(conn, fld_data))
    return(flds)

def process_field(conn, fld):
    number = fld[0]
    name = fld[1]
    fld_type = fld[2]
    comment = fld[3]

    fld = gen_basic_entry(number, name, fld_type, comment)

    if fld_type[:6].lower() == 'string':
        fld['type'] = 's'
        fld['strlen'] = extract_strlen(fld_type)
    elif fld_type[:5].lower() == 'array':
        fld['type'] = 'a'
        lower, upper, basetype = process_array(fld_type)
        fld['lower'] = lower
        fld['upper'] = upper
        if basetype.lower() in fld_type_codes:
            fld['basetype'] = fld_type_codes[basetype.lower()]
        else:
            fld['basetype'] = basetype
    elif fld_type.lower() in fld_type_codes:
        fld['type'] = fld_type_codes[fld_type.lower()]
    else:
        fld = process_record(conn, number, name, fld_type)
    return(fld)

def extract_strlen(fld_type):
    poslb = fld_type.find('[')
    posrb = fld_type.find(']')
    strlen = int(fld_type[poslb+1:posrb])
    return(strlen)

def process_array(fld_type):
    # arr_bnds = {'swtimbco_c': 42,
                # 'psleep_c':   51}
    match = re.match('array\s*\[(\d+)\.\.([^\]]*)\]\s*of\s*(.*)', fld_type)
    if match == None:
        print("Unmatched array defn","'" + fld_type + "'")
        sys.exit()
    lower = match.group(1)
    upper = match.group(2)
    basetype = match.group(3)
    return(lower, upper, basetype)

    fld_type = match.group(3)
    print('for ix :=', lower, 'to', upper, 'do')
    gen_basic_entry(number, name + "[',ix,'] ", fld_type.lower(),'array comment')

#==============================================================

if len(sys.argv) < 2:
    recname = 'Told_keep_data'
    print("Record name missing. Defaulting to", recname)
else:
    recname = sys.argv[1]
outfilename = recname+".json"
outfile = open(outfilename, 'w')

conn = connect_db('records.sqlite')

rec = process_record(conn, 0, '', recname)

print(json.dumps(rec, indent=2), file=outfile)
outfile.close()
print("DONE :  JSON written to file >", outfilename, "<")
